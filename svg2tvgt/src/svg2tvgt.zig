const std = @import("std");
const testing = std.testing;
const xml = @import("xml");
const Number = @import("Number.zig");

fn oom(e: error{OutOfMemory}) noreturn { @panic(@errorName(e)); }
fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.log.err(fmt, args);
    std.process.exit(0xff);
}

fn usage() !void {
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // TODO: definitely make "coordinate-range" a command-line option
    try std.io.getStdErr().writer().writeAll(
        \\svg2tvgt [--output OUT_FILE] SVG_FILE
        \\
        \\Converts a SVG file into the TinyVG text representation.
        \\Use tvg-text to convert output into binary.
        \\
        \\Options
        \\  -o, --output <file>   Writes the output tvgt to <file>. If not given, the output
        \\                        will be <input> with .tvgt extension.
        \\
    );
}

pub fn main() !u8 {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const arena = arena_instance.allocator();

    var cmd_opt: struct {
        output: ?[]const u8 = null,
    } = .{};

    const cmd_args = blk: {
        const all_args = try std.process.argsAlloc(arena);
        if (all_args.len <= 1) {
            try usage();
            return 0xff;
        }
        var new_arg_count: usize = 0;
        var arg_index: usize = 1;
        while (arg_index < all_args.len) : (arg_index += 1) {
            const arg = all_args[arg_index];
            if (!std.mem.startsWith(u8, arg, "-")) {
                all_args[new_arg_count] = arg;
                new_arg_count += 1;
            } else if (std.mem.eql(u8, arg, "--output") or std.mem.eql(u8, arg, "-o")) {
                arg_index += 1;
                if (arg_index >= all_args.len) fatal("--output requires an argument", .{});
                cmd_opt.output = all_args[arg_index];
            } else fatal("unknown cmdline option {s}", .{arg});
        }
        break :blk all_args[0 .. new_arg_count];
    };
    if (cmd_args.len != 1) fatal(
        "expected 1 non-option cmdline argument but got {}", .{cmd_args.len}
    );
    const svg_file_path = cmd_args[0];

    var file = std.fs.cwd().openFile(svg_file_path, .{}) catch |err| fatal(
        "open '{s}' failed with {s}", .{svg_file_path, @errorName(err)}
    );
    //defer file.close();
    const document = xml.parse(arena, "", file.reader()) catch |err| fatal(
        "failed to parse '{s}' as xml with {s}", .{svg_file_path, @errorName(err)}
    );
    document.acquire();

    const ext_index: usize = @intFromPtr(std.fs.path.extension(svg_file_path).ptr) - @intFromPtr(svg_file_path.ptr);
    const out_file_path = cmd_opt.output orelse
        try std.fmt.allocPrint(arena, "{s}.tvgt", .{svg_file_path[0 .. ext_index]});
    var out_file = std.fs.cwd().createFile(out_file_path, .{}) catch |err| fatal(
        "failed to create output file '{s}' with {s}", .{out_file_path, @errorName(err)}
    );
    defer out_file.close();
    var bw = std.io.bufferedWriter(out_file.writer());
    try writeTvgt(arena, bw.writer(), document);
    try bw.flush();
    return 0;
}

const RootAttr = enum {
    width,
    height,
    viewBox,
    fill,
    xmlns,
};
const root_attr_map = std.StaticStringMap(RootAttr).initComptime(.{
    .{ "width", .width },
    .{ "height", .height },
    .{ "viewBox", .viewBox },
    .{ "fill", .fill },
    .{ "xmlns", .xmlns },
});

fn writeTvgt(
    arena: std.mem.Allocator,
    writer: anytype,
    document: xml.Document,
) !void {
    try writer.writeAll("(tvg 1 ");

    var svg_default_fill: ?tvg.Color = named_colors.get("black").?;

    var maybe_viewbox: ?[]const u8 = null;
    var maybe_width: ?[]const u8 = null;
    var maybe_height: ?[]const u8 = null;
    for (elementAttrs(document.root)) |attr| {
        const attr_name = attr.name.slice();
        if (root_attr_map.get(attr.name.slice())) |root_attr| switch (root_attr) {
            .width => maybe_width = attr.value.slice(),
            .height => maybe_height = attr.value.slice(),
            .viewBox => maybe_viewbox = attr.value.slice(),
            .fill => svg_default_fill = parseColorOrNone(attr.value.slice()),
            .xmlns => {},
        } else fatal("todo handle svg attribute '{s}'", .{attr_name});
    }

    var analysis = Analysis{ .allocator = arena };
    for (document.root.children()) |child_node_index| {
        try analyze(&analysis, child_node_index.v().element, svg_default_fill);
    }

    const viewbox: struct {
        min: XY(Number),
        size: XY(Number),
    } = blk: {
        if (maybe_viewbox) |viewbox_str| {
            const min_x = (try parseNumber(viewbox_str, 0)) orelse fatal("viewbox missing min-x", .{});
            const min_y = (try parseNumber(viewbox_str, min_x.after)) orelse fatal("viewbox missing min-y", .{});
            const width = (try parseNumber(viewbox_str, min_y.after)) orelse fatal("viewbox missing width", .{});
            const height = (try parseNumber(viewbox_str, width.after)) orelse fatal("viewbox missing height", .{});
            const end = scanWhitespace(viewbox_str, height.after);
            if (end != viewbox_str.len) fatal("viewbox has extra junk '{s}'", .{viewbox_str[end..]});
            break :blk .{
                .min = .{ .x = min_x.value, .y = min_y.value },
                .size = .{ .x = width.value, .y = height.value },
            };
        }

        const width_str = maybe_width orelse @panic("svg has neither a viewBox nor a width");
        const height_str = maybe_height orelse @panic("svg has neither a viewBox nor a height");
        const width = Number.parse(width_str) catch |err| fatal(
            "parse width '{s}' failed with {s}",
            .{width_str, @errorName(err)}
        );
        const height = Number.parse(height_str) catch |err| fatal(
            "parse height '{s}' failed with {s}",
            .{height_str, @errorName(err)}
        );
        if (width.bits <= 0) fatal("width {} is not positive", .{width});
        if (height.bits <= 0) fatal("height {} is not positive", .{height});
        break :blk .{
            .min = .{
                .x = .{ .bits = 0, .frac_digits = 0 },
                .y = .{ .bits = 0, .frac_digits = 0 },
            },
            .size = .{ .x = width, .y = height },
        };
    };

    if (viewbox.min.x.bits != 0 or viewbox.min.y.bits != 0) fatal(
        (
            "non-origin viewbox {},{}, there's either a TinyVG feature for this " ++
            "or I'll need to implement offseting all points"
        ),
        .{viewbox.min.x, viewbox.min.y},
    );
    analysis.addNumber(viewbox.min.x);
    analysis.addNumber(viewbox.min.y);
    analysis.addNumber(viewbox.size.x);
    analysis.addNumber(viewbox.size.y);

    const scale: tvg.Scale = blk: {
        const min_max = analysis.getMinMax();
        const range = min_max.max.add(min_max.min.abs());
        std.log.info("number range is {} ({} - {})", .{
            range, min_max.min, min_max.max
        });
        const min_float = min_max.min.toFloat(f64);
        const max_float = min_max.max.toFloat(f64);
        var scale: tvg.Scale = .@"1/32768";
        while (true) {
            const unit_min = scale.map(min_float);
            const unit_max = scale.map(max_float);
            const good_scale = (
                @intFromEnum(unit_min) >= -32768.0 and
                @intFromEnum(unit_max) <= 32767
            );
            std.log.info(
                "number range is {d} - {d} at scale {s} ({s})",
                .{
                    @intFromEnum(unit_min),
                    @intFromEnum(unit_max),
                    @tagName(scale),
                    @as([]const u8, if (good_scale) "OK" else "overflow"),
                },
            );
            if (good_scale)
                break :blk scale;
            if (scale == .@"1/1") fatal("svg too big to represent accurately?", .{});
            scale = @enumFromInt(@intFromEnum(scale)-1);
        }
    };
    std.log.warn("TODO: analyze the SVG to see what color encoding we should use", .{});
    std.log.warn("TODO: analyze the SVG to see what range we should use if the user didn't provide one (8/16/32 bit)", .{});
    const range: tvg.Range= .default;

    // the header
    try writer.print("({} {} {s} u8888 {s}) (\n", .{
        viewbox.size.x,
        viewbox.size.y,
        @tagName(scale),
        @tagName(range),
    });
    // the color table
    for (analysis.color_set.keys()) |color| {
        if (color.a != 1.0) try writer.print("   ({d} {d} {d} {d})", .{
            color.r, color.g, color.b, color.a
        }) else try writer.print("  ({d} {d} {d})\n", .{
            color.r, color.g, color.b
        });
    }
    try writer.writeAll(")(\n");
    // the draw commands
    for (document.root.children()) |child_node_index| {
        try writeTvgtCommands(
            writer,
            child_node_index.v().element,
            svg_default_fill,
            analysis,
        );
    }
    try writer.writeAll("))\n");
}

fn writeTvgtCommands(
    writer: anytype,
    element: xml.Element,
    default_fill: ?tvg.Color,
    analysis: Analysis,
) !void {
    const element_name = element.tag_name.slice();
    const kind = element_map.get(element_name) orelse fatal(
        "unhandled element <{s}>", .{element_name}
    );
    if (element.children().len > 0) fatal("todo: handle xml children", .{});

    switch (kind) {
        .path => {
            var maybe_d: ?[]const u8 = null;
            var opacity: Number = .{ .bits = 1, .frac_digits = 0 };
            var maybe_fill_raw = default_fill;
            var maybe_stroke: ?[]const u8 = null;
            var stroke_width: Number = .{ .bits = 1, .frac_digits = 0 };
            var stroke_linecap: []const u8 = "butt";

            for (elementAttrs(element)) |attr| {
                const attr_name = attr.name.slice();
                const attr_value = attr.value.slice();
                switch (path_attr_map.get(attr_name) orelse fatal(
                    "<{s}> element has unknown attribute '{s}", .{element_name, attr_name}
                )) {
                    .d => maybe_d = attr_value,
                    .opacity => opacity = Number.parse(attr_value) catch |err| fatal(
                        "bad opacity '{s}': {s}", .{attr_value, @errorName(err)}
                    ),
                    .fill => maybe_fill_raw = parseColorOrNone(attr_value),
                    .stroke => maybe_stroke = attr_value,
                    .@"stroke-width" => stroke_width = try Number.parse(attr_value),
                    .@"stroke-linecap" => stroke_linecap = attr_value,
                    .@"stroke-linejoin" => {},
                }
            }

            const d = maybe_d orelse return;
            if (maybe_fill_raw) |fill_raw| {
                if (maybe_stroke) |_| {
                    if (true) @panic("todo: handle stroke linecap");
                    if (true) @panic("todo: handle opacity");
                    @panic("handle fill and stroke");
                }
                const fill = resolveFillOpacity(fill_raw, opacity);
                try writer.print("  (fill_path {} (\n", .{analysis.fmtColorStyle(fill)});
                try writePath(writer, d);
                try writer.print("  ))\n", .{});
            } else if (maybe_stroke) |stroke| {
                if (!opacity.equalsInteger(1))
                    @panic("todo: handle opacity");


                if (!std.mem.eql(u8, stroke_linecap, "round")) {
                    std.log.warn(
                        "stroke-linecap is '{s}', can't render this, can only render 'round'. " ++
                        "We should check if the path we are rendering is going to have an exposed linecap.",
                        .{stroke_linecap},
                    );
                }
                try writer.print(
                    "  (draw_line_path {} {} (\n",
                    .{analysis.fmtColorStyle(parseColor(stroke)), stroke_width},
                );
                try writePath(writer, d);
                try writer.print("  ))\n", .{});
            }
        },
        .circle => {
            @panic("todo: circle");
        },
    }
}

fn resolveFillOpacity(fill: tvg.Color, opacity: Number) tvg.Color {
    if (opacity.equalsInteger(1))
        return fill;
    if (fill.a != 1.0)
        @panic("TODO: implementing combining fill with alpha and opacity");
    return .{
        .r = fill.r,
        .g = fill.g,
        .b = fill.b,
        .a = opacity.toFloat(f32),
    };
}

fn writePath(
    writer: anytype,
    d: []const u8,
) !void {
    var it = PathDrawIterator{ .draw = d };
    var current_path = false;
    var position: XY(Number) = .{
        .x = .{.bits = 0, .frac_digits = 0 },
        .y = .{.bits = 0, .frac_digits = 0 },
    };
    var maybe_last_open_point: ?XY(Number) = null;
    while (try it.next()) |cmd| {
        const previous_position = position;
        position = cmd.calcPosition(
            previous_position,
            &maybe_last_open_point,
        );
        switch (cmd) {
            .m => {
                if (current_path) {
                    try writer.print("    )\n", .{});
                }
                try writer.print("    ({} {}) (\n", .{position.x, position.y});
                current_path = true;
            },
            .z => try writer.print("      (close - )\n", .{}),
            .h => try writer.print("      (horiz - {})\n", .{position.x}),
            .v => try writer.print("      (vert - {})\n", .{position.y}),
            .l => try writer.print("      (line - {} {})\n", .{position.x, position.y}),
            .a => |a| {
                try writer.print("      (arc_ellipse -", .{});
                try writer.print(" {}", .{a.arc.radius.x.abs()});
                try writer.print(" {}", .{a.arc.radius.y.abs()});
                try writer.print(" {}", .{a.arc.angle});
                try writer.print(" {}", .{a.arc.large_arc});
                try writer.print(" {}", .{switch (a.arc.sweep) { .left => true, .right => false }});
                try writer.print(" ({} {})", .{position.x, position.y});
                try writer.print(")\n", .{});
            },
            .s => |s| switch (s.kind) {
                .relative => try writer.print("      (quadratic_bezier - ({} {}) ({} {}))\n", .{
                    previous_position.x.add(s.bezier[0].x),
                    previous_position.y.add(s.bezier[0].y),
                    position.x,
                    position.y,
                }),
                .absolute => try writer.print("      (quadratic_bezier - ({} {}) ({} {}))\n", .{
                    s.bezier[0].x, s.bezier[0].y,
                    s.bezier[1].x, s.bezier[1].y,
                }),
            },
            .c => |c| switch (c.kind) {
                .relative => try writer.print("      (bezier - ({} {}) ({} {}) ({} {}))\n", .{
                    previous_position.x.add(c.bezier[0].x),
                    previous_position.y.add(c.bezier[0].y),
                    previous_position.x.add(c.bezier[1].x),
                    previous_position.y.add(c.bezier[1].y),
                    position.x,
                    position.y,
                }),
                .absolute => try writer.print("      (bezier - ({} {}) ({} {}) ({} {}))\n", .{
                    c.bezier[0].x, c.bezier[0].y,
                    c.bezier[1].x, c.bezier[1].y,
                    c.bezier[2].x, c.bezier[2].y,
                }),
            },
        }
    }
    if (current_path) {
        try writer.print("    )\n", .{});
    }
}

const ElementKind = enum {
    path,
    circle,
};
const element_map = std.StaticStringMap(ElementKind).initComptime(.{
    .{ "path", .path },
    .{ "circle", .circle },
});
const PathAttr = enum {
    d,
    opacity,
    fill,
    stroke,
    @"stroke-width",
    @"stroke-linecap",
    @"stroke-linejoin",
};
const path_attr_map = std.StaticStringMap(PathAttr).initComptime(.{
    .{ "d", .d },
    .{ "opacity", .opacity },
    .{ "fill", .fill },
    .{ "stroke", .stroke },
    .{ "stroke-width", .@"stroke-width" },
    .{ "stroke-linecap", .@"stroke-linecap" },
    .{ "stroke-linejoin", .@"stroke-linejoin" },
});
const CircleAttr = enum {
    fill,
    cx,
    cy,
    r,
};
const circle_attr_map = std.StaticStringMap(CircleAttr).initComptime(.{
    .{ "fill", .fill },
    .{ "cx", .cx },
    .{ "cy", .cy },
    .{ "r", .r },
});

fn parseColorOrNone(str: []const u8) ?tvg.Color {
    if (std.mem.eql(u8, str, "none"))
        return null;
    return parseColor(str);
}
fn parseColor(str: []const u8) tvg.Color {
    if (str.len > 0 and str[0] == '#') return tvg.Color.fromString(
        str[1..],
    ) catch |err| switch (err) {
        error.Overflow,
        error.InvalidCharacter,
        error.InvalidFormat,
        => |e| std.debug.panic("invalid hex color '{s}' ({s})", .{str, @errorName(e)}),
    };
    if (named_colors.get(str)) |color|
        return color;
    std.debug.panic("todo handle color '{s}'", .{str});
}

const ColorHashMapUnmanaged = std.ArrayHashMapUnmanaged(
    tvg.Color,
    u32,
    ColorHashContext,
    false,
);

const Analysis = struct {
    allocator: std.mem.Allocator,
    number_set: std.AutoHashMapUnmanaged(Number, void) = .{},
    color_set: ColorHashMapUnmanaged = .{},
    pub fn addNumber(self: *Analysis, num: Number) void {
        self.number_set.put(self.allocator, num, {}) catch |e| oom(e);
    }
    pub fn addColor(self: *Analysis, color: tvg.Color) void {
        const entry = self.color_set.getOrPut(self.allocator, color) catch |e| oom(e);
        if (!entry.found_existing) {
            const color_index = std.math.cast(u32, self.color_set.count() - 1) orelse fatal(
                "too many colors", .{}
            );
            entry.value_ptr.* = color_index;
        }
    }

    const MinMax = struct {
        min: Number,
        max: Number,
    };
    pub fn getMinMax(self: Analysis) MinMax {
        var result: MinMax = .{
            .min = .{ .bits = 0, .frac_digits = 0 },
            .max = .{ .bits = 0, .frac_digits = 0 },
        };
        var it = self.number_set.iterator();
        while (it.next()) |entry| {
            if (entry.key_ptr.order(result.min) == .lt) {
                result.min = entry.key_ptr.*;
            }
            if (entry.key_ptr.order(result.max) == .gt) {
                result.max = entry.key_ptr.*;
            }
        }
        return result;
    }

    const FormatColorStyle = struct {
        index: u32,
        pub fn format(
            self: FormatColorStyle,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            try writer.print("(flat {})", .{self.index});
        }
    };
    pub fn fmtColorStyle(self: Analysis, color: tvg.Color) FormatColorStyle {
        const index = self.color_set.get(color) orelse std.debug.panic(
            "color '{}' was missed in analysis",
            .{ color },
        );
        return .{ .index = index };
    }
};

fn analyze(
    analysis: *Analysis,
    element: xml.Element,
    default_fill: ?tvg.Color,
) !void {
    const element_name = element.tag_name.slice();
    const kind = element_map.get(element_name) orelse fatal(
        "unhandled element <{s}>", .{element_name}
    );

    var opacity: Number = .{ .bits = 1, .frac_digits = 0 };
    var maybe_fill_raw = default_fill;
    for (elementAttrs(element)) |attr| {
        const attr_name = attr.name.slice();
        const attr_value = attr.value.slice();
        switch (kind) {
            .path => switch (path_attr_map.get(attr_name) orelse fatal(
                "<{s}> element has unknown attribute '{s}", .{element_name, attr_name}
            )) {
                .d => try analyzePathDraw(analysis, attr_value),
                .opacity => opacity = Number.parse(attr_value) catch |err| fatal(
                    "bad opacity '{s}': {s}", .{attr_value, @errorName(err)}
                ),
                .fill => maybe_fill_raw = parseColorOrNone(attr_value),
                .stroke => analysis.addColor(parseColor(attr_value)),
                .@"stroke-width" => {},
                .@"stroke-linecap" => {},
                .@"stroke-linejoin" => {},
            },
            .circle => switch (circle_attr_map.get(attr_name) orelse fatal(
                "<{s}> element has unknown attribute '{s}", .{element_name, attr_name}
            )) {
                .fill => maybe_fill_raw = parseColorOrNone(attr_value),
                .cx => analysis.addNumber(Number.parse(attr_value) catch |err| fatal(
                    "bad circle cx '{s}': {s}", .{attr_value, @errorName(err)},
                )),
                .cy => analysis.addNumber(Number.parse(attr_value) catch |err| fatal(
                    "bad circle cy '{s}': {s}", .{attr_value, @errorName(err)},
                )),
                .r => {},
            },
        }
    }

    if (maybe_fill_raw) |fill_raw| {
        const fill = resolveFillOpacity(fill_raw, opacity);
        analysis.addColor(fill);
    }

    //@panic("todo");
//    if (node.Fill != null)
//      style["fill"] = node.Fill;
//    if (node.Stroke != null)
//      style["stroke"] = node.Stroke;
//    if (node.Opacity != 1)
//      style["opacity"] = node.Opacity.ToString();
//
//    float opacity = ToFloat(style["opacity"] ?? "1");
//
//    foreach (string key in style.AllKeys)
//    {
//      switch (key)
//      {
//        case "fill":
//        case "opacity":
//        case "stroke":
//        case "fill-opacity":
//        case "stroke-opacity":
//        case "color":
//          break;
//        default:
//          if (!unknown_styles.TryGetValue(key, out var set))
//            unknown_styles.Add(key, set = new HashSet<string>());
//          set.Add(style[key]);
//          break;
//      }
//    }
//
//    var no_fill = false;
//    // var no_stroke = false;
//
//    var fill = style["fill"];
//    if (fill != null)
//    {
//      float fill_opacity = opacity * node.FillOpacity * ToFloat(style["fill-opacity"] ?? "1");
//      var color = AnalyzeStyleDef(buf, fill, fill_opacity);
//      if (color != null)
//      {
//        node.TvgFillStyle = new TvgFlatColor { Color = color.Value };
//      }
//      else
//      {
//        no_fill = true;
//      }
//    }
//
//    var stroke = style["stroke"] ?? style["color"];
//    if (stroke != null)
//    {
//      float stroke_opacity = opacity * ToFloat(style["stroke-opacity"] ?? "1");
//      var color = AnalyzeStyleDef(buf, stroke, stroke_opacity);
//      if (color != null)
//      {
//        node.TvgLineStyle = new TvgFlatColor { Color = color.Value };
//      }
//      else
//      {
//        // no_stroke = true;
//      }
//    }
//
//    if (node is SvgGroup group)
//    {
//      foreach (var child in group.Nodes ?? new SvgNode[0])
//      {
//        child.Parent = node;
//        AnalyzeNode(buf, child, indent + " ");
//      }
//    }
//    else
//    {
//      if (!no_fill && node.TvgFillStyle == null)
//      {
//        node.TvgFillStyle = new TvgFlatColor { Color = buf.InsertColor("#000", opacity) };
//      }
//    }
//
//    // Console.WriteLine(
//    //   "{5}Analyzed {0} with {1} ({2}) and {3} ({4})",
//    //   node.GetType().Name,
//    //   node.TvgFillStyle?.ToString() ?? "<null>", fill ?? "<null>",
//    //   node.TvgLineStyle?.ToString() ?? "<null>", stroke ?? "<null>",
//    //   indent);
}

fn XY(comptime T: type) type {
    return struct {
        x: T,
        y: T,
    };
}

const DrawPositionKind = enum { absolute, relative };

const PathDraw = union(enum) {
    m: struct {
        kind: DrawPositionKind,
        point: XY(Number),
    },
    z: void,
    h: struct {
        kind: DrawPositionKind,
        len: Number,
    },
    v: struct {
        kind: DrawPositionKind,
        len: Number,
    },
    l: struct {
        kind: DrawPositionKind,
        point: XY(Number),
    },
    a: struct {
        kind: DrawPositionKind,
        arc: Arc,
    },
    s: struct {
        kind: DrawPositionKind,
        bezier: [2]XY(Number),
    },
    c: struct {
        kind: DrawPositionKind,
        bezier: [3]XY(Number),
    },
    pub fn getExtraParamsCmd(self: PathDraw) SvgCmd {
        return switch (self) {
            .m => |m| switch (m.kind) { .absolute => .L, .relative => .l },
            .z => .Z,
            .h => |h| switch (h.kind) { .absolute => .H, .relative => .h },
            .v => |v| switch (v.kind) { .absolute => .V, .relative => .v },
            .l => |l| switch (l.kind) { .absolute => .L, .relative => .l },
            .a => |a| switch (a.kind) { .absolute => .A, .relative => .a },
            .s => |s| switch (s.kind) { .absolute => .S, .relative => .s },
            .c => |c| switch (c.kind) { .absolute => .C, .relative => .c },
        };
    }
    pub fn calcPosition(
        self: PathDraw,
        current: XY(Number),
        maybe_last_open_point: *?XY(Number),
    ) XY(Number) {
        switch (self) {
            .m => |m| {
                const point: XY(Number) = switch (m.kind) {
                    .absolute => m.point,
                    .relative => .{
                        .x = current.x.add(m.point.x),
                        .y = current.y.add(m.point.y),
                    },
                };
                maybe_last_open_point.* = point;
                return point;
            },
            .z => {
                const last_open_point = maybe_last_open_point.* orelse fatal(
                    "svg closed the path without opening one", .{}
                );
                return last_open_point;
            },
            .h => |h| switch (h.kind) {
                .absolute => return .{
                    .x = h.len,
                    .y = current.y,
                },
                .relative => return .{
                    .x = current.x.add(h.len),
                    .y = current.y,
                },
            },
            .v => |v| switch (v.kind) {
                .absolute => return .{
                    .x = current.x,
                    .y = v.len,
                },
                .relative => return .{
                    .x = current.x,
                    .y = current.y.add(v.len),
                },
            },
            .l => |l| switch (l.kind) {
                .absolute => return l.point,
                .relative => return .{
                    .x = current.x.add(l.point.x),
                    .y = current.y.add(l.point.y),
                },
            },
            .a => |a| switch (a.kind) {
                .absolute => return a.arc.target,
                .relative => return .{
                    .x = current.x.add(a.arc.target.x),
                    .y = current.y.add(a.arc.target.y),
                },
            },
            .s => |s| switch (s.kind) {
                // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                // NOTE: we need to add the extra points to our number set
                //       probably add a second function addExtraPoints or something
                .absolute => return s.bezier[1],
                .relative => return .{
                    .x = current.x.add(s.bezier[1].x),
                    .y = current.y.add(s.bezier[1].y),
                },
            },
            .c => |c| switch (c.kind) {
                // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                // NOTE: we need to add the extra points to our number set
                //       probably add a second function addExtraPoints or something
                .absolute => return c.bezier[2],
                .relative => return .{
                    .x = current.x.add(c.bezier[2].x),
                    .y = current.y.add(c.bezier[2].y),
                },
            },
        }
    }
};
const PathDrawIterator = struct {
    draw: []const u8,
    index: usize = 0,
    last_command: ?SvgCmd = null,

    pub fn next(self: *PathDrawIterator) !?PathDraw {
        while (true) {
            const result = blk: {
                const first_result = try scanDrawCommand(self.draw, self.index);
                if (first_result.after != self.index)
                    break :blk first_result;
                if (self.index == self.draw.len)
                    return null;
                if (self.last_command) |last_command| {
                    const second_result = try parseCommand(self.draw, self.index, last_command);
                    if (second_result.after != self.index)
                        break :blk second_result;
                }
                std.debug.panic(
                    "invalid path draw \"{s}\" unknown character '{c}' at index {}",
                    .{self.draw, self.draw[self.index], self.index },
                );
            };
            self.index = result.after;
            if (result.command) |c| {
                self.last_command = c.getExtraParamsCmd();
                return c;
            }
        }
    }
};

const SvgCmdParams = enum {
    none,
    number,
    point,
    arc,
    quadratic_bezier,
    cubic_bezier,
};
const SvgCmd = enum {
    M, H, V, L, A, S, C, Z,
    m, h, v, l, a, s, c,

    pub fn initNumber(self: SvgCmd, n: Number) PathDraw {
        return switch (self) {
            .H => .{ .h = .{ .kind = .absolute, .len = n } },
            .V => .{ .v = .{ .kind = .absolute, .len = n } },
            .h => .{ .h = .{ .kind = .relative, .len = n } },
            .v => .{ .v = .{ .kind = .relative, .len = n } },
            .M, .L, .A, .S, .C, .Z,
            .m, .l, .a, .s, .c,
            => @panic("codebug"),
        };
    }
    pub fn initPoint(self: SvgCmd, p: XY(Number)) PathDraw {
        return switch (self) {
            .M => .{ .m = .{ .kind = .absolute, .point = p } },
            .L => .{ .l = .{ .kind = .absolute, .point = p } },
            .m => .{ .m = .{ .kind = .relative, .point = p } },
            .l => .{ .l = .{ .kind = .relative, .point = p } },
            .H, .V, .A, .S, .C, .Z,
            .h, .v, .a, .s, .c,
            => @panic("codebug"),
        };
    }

    pub fn fromChar(c: u8) ?SvgCmd {
        return switch (c) {
            'A' => .A,
            'C' => .C,
            'H' => .H,
            'L' => .L,
            'M' => .M,
            'S' => .S,
            'V' => .V,
            'Z' => .Z,
            'a' => .a,
            'c' => .c,
            'h' => .h,
            'l' => .l,
            'm' => .m,
            's' => .s,
            'v' => .v,
            'z' => .Z,
            else => null,
        };
    }
    pub fn getPositionKind(self: SvgCmd) DrawPositionKind {
        return switch (self) {
            .M, .H, .V, .L, .A, .S, .C, .Z, => .absolute,
            .m, .h, .v, .l, .a, .s, .c => .relative,
        };
    }
    pub fn getParams(self: SvgCmd) SvgCmdParams {
        return switch (self) {
            .M => .point,
            .H => .number,
            .V => .number,
            .L => .point,
            .A => .arc,
            .S => .quadratic_bezier,
            .C => .cubic_bezier,
            .Z => .none,
            .m => .point,
            .h => .number,
            .v => .number,
            .l => .point,
            .a => .arc,
            .s => .quadratic_bezier,
            .c => .cubic_bezier,
        };
    }
};

const ScanCommandResult = struct {
    after: usize,
    command: ?PathDraw,
};
fn scanDrawCommand(draw: []const u8, start: usize) !ScanCommandResult {
    var index = scanWhitespace(draw, start);
    if (index >= draw.len) return .{
        .after = index,
        .command = null,
    };

    if (SvgCmd.fromChar(draw[index])) |cmd| {
        index += 1;
        return try parseCommand(draw, index, cmd);
    }

    return .{ .after = index, .command = null };
}

fn parseCommand(draw: []const u8, start: usize, cmd: SvgCmd) !ScanCommandResult {
    switch (cmd.getParams()) {
        .none => return .{
            .after = start,
            .command = .z,
        },
        .number => if (try parseNumber(draw, start)) |result| return .{
            .after = result.after,
            .command = cmd.initNumber(result.value),
        },
        .point => if (try parsePoint(draw, start)) |result| return .{
            .after = result.after,
            .command = cmd.initPoint(result.point),
        },
        .arc => if (try parseArc(draw, start)) |result| return .{
            .after = result.after,
            .command = .{ .a = .{
                .kind = cmd.getPositionKind(),
                .arc = result.arc,
            }},
        },
        .quadratic_bezier => if (try parseQuadraticBezier(draw, start)) |result| return .{
            .after = result.after,
            .command = .{ .s = .{
                .kind = cmd.getPositionKind(),
                .bezier = result.bezier,
            }},
        },
        .cubic_bezier => if (try parseCubicBezier(draw, start)) |result| return .{
            .after = result.after,
            .command = .{ .c = .{
                .kind = cmd.getPositionKind(),
                .bezier = result.bezier,
            }},
        },
    }
    return .{ .after = start, .command = null };
}

fn analyzePathDraw(
    analysis: *Analysis,
    draw: []const u8,
) !void {
    var position: XY(Number) = .{
        .x = .{.bits = 0, .frac_digits = 0 },
        .y = .{.bits = 0, .frac_digits = 0 },
    };
    var maybe_last_open_point: ?XY(Number) = null;

    var it = PathDrawIterator{ .draw = draw };
    while (try it.next()) |cmd| {
        // avoid "attack of the kill features"
        const copy = position;
        position = cmd.calcPosition(copy, &maybe_last_open_point);
        analysis.addNumber(position.x);
        analysis.addNumber(position.y);
    }
}

fn parseNumber(s: []const u8, start: usize) !?struct {
    after: usize,
    value: Number,
} {
    var index = start;

    index = scanWhitespace(s, index);
    const parsed = (try Number.parseLazy(s[index..])) orelse return null;
    index += parsed.char_count;

    return .{ .after = index, .value = parsed.num };
}

fn parsePoint(s: []const u8, start: usize) !?struct {
    after: usize,
    point: XY(Number),
} {
    var index = start;
    index = scanWhitespace(s, index);
    const first = (try Number.parseLazy(s[index..])) orelse return null;
    index += first.char_count;

    index = scanWhitespace(s, index);
    const second = (try Number.parseLazy(s[index..])) orelse fatal(
        "expected a number pair but only got a single number: '{s}'", .{s[index..]}
    );
    index += second.char_count;
    return .{
        .after = index,
        .point = .{ .x = first.num, .y = second.num },
    };
}

const Sweep = enum { left, right };
const Arc = struct {
    radius: XY(Number),
    angle: Number,
    large_arc: bool,
    sweep: Sweep,
    target: XY(Number),
};
fn parseArc(s: []const u8, start: usize) !?struct {
    after: usize,
    arc: Arc,
} {
    const radius_x = (try parseNumber(s, start)) orelse return null;
    const radius_y = (try parseNumber(s, radius_x.after)) orelse fatal(
        "arc missing y radius: '{s}'", .{s[start..]}
    );
    const angle = (try parseNumber(s, radius_y.after)) orelse fatal(
        "arc missing angle: '{s}'", .{s[start..]}
    );
    const large_arc = try parseFlag(s, angle.after);
    const sweep = try parseFlag(s, large_arc.after);
    const target_x = (try parseNumber(s, sweep.after)) orelse fatal(
        "arc missing target x: '{s}'", .{s[start..]}
    );
    const target_y = (try parseNumber(s, target_x.after)) orelse fatal(
        "arc missing target y: '{s}'", .{s[start..]}
    );
    return .{
        .after = target_y.after,
        .arc = .{
            .radius = .{ .x = radius_x.value, .y = radius_y.value },
            .angle = angle.value,
            .large_arc = large_arc.enabled,
            .sweep = if (sweep.enabled) .right else .left,
            .target = .{ .x = target_x.value, .y = target_y.value },
        },
    };
}

fn parseQuadraticBezier(s: []const u8, start: usize) !?struct {
    after: usize,
    bezier: [2]XY(Number),
} {
    const first_x = (try parseNumber(s, start)) orelse return null;
    const first_y = (try parseNumber(s, first_x.after)) orelse fatal(
        "bezier missing second coordinate: '{s}'", .{s[start..]}
    );
    const second = (try parsePoint(s, first_y.after)) orelse fatal(
        "bezier missing second point: '{s}'", .{s[start..]}
    );
    return .{
        .after = second.after,
        .bezier = [2]XY(Number){
            .{ .x = first_x.value, .y = first_y.value },
            second.point,
        },
    };
}

fn parseCubicBezier(s: []const u8, start: usize) !?struct {
    after: usize,
    bezier: [3]XY(Number),
} {
    const first_x = (try parseNumber(s, start)) orelse return null;
    const first_y = (try parseNumber(s, first_x.after)) orelse fatal(
        "bezier missing second coordinate: '{s}'", .{s[start..]}
    );
    const second = (try parsePoint(s, first_y.after)) orelse fatal(
        "bezier missing second point: '{s}'", .{s[start..]}
    );
    const third = (try parsePoint(s, second.after)) orelse fatal(
        "bezier missing third point: '{s}'", .{s[start..]}
    );
    return .{
        .after = third.after,
        .bezier = [3]XY(Number){
            .{ .x = first_x.value, .y = first_y.value },
            second.point,
            third.point,
        },
    };
}

fn scanWhitespace(s: []const u8, start: usize) usize {
    var index = start;
    while (index < s.len and s[index] == ' ') : (index += 1) {
    }
    return index;
}

fn parseFlag(s: []const u8, start: usize) !struct {
    after: usize,
    enabled: bool,
}{
    const result = (try parseNumber(s, start)) orelse fatal(
        "expected 0/1 flag but got '{s}'", .{s[start..]}
    );
    if (result.value.frac_digits != 0 or (result.value.bits != 0 and result.value.bits != 1)) fatal(
        "expected 0/1 flag but got '{s}'", .{s[start..result.after]}
    );
    return .{ .after = result.after, .enabled = (result.value.bits == 1) };
}

const ColorHashContext = struct {
    pub fn eql(ctx: ColorHashContext, a: tvg.Color, b: tvg.Color, index: usize) bool {
        _ = ctx;
        _ = index;
        return
            a.r == b.r and
            a.g == b.g and
            a.b == b.b and
            a.a == b.a;
    }
    pub fn hash(ctx: ColorHashContext, key: tvg.Color) u32 {
        _ = ctx;
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(@as([*]const u8, @ptrCast(&key.r))[0..4]);
        hasher.update(@as([*]const u8, @ptrCast(&key.g))[0..4]);
        hasher.update(@as([*]const u8, @ptrCast(&key.b))[0..4]);
        hasher.update(@as([*]const u8, @ptrCast(&key.a))[0..4]);
        return @truncate(hasher.final());
    }
};

pub const Attribute = extern struct {
    name: xml.StringIndex align(1),
    value: xml.StringIndex align(1),
};

pub fn elementAttrs(elem: xml.Element) []const Attribute {
    const eidx = elem.attributes;
    if (eidx == .empty) return &.{};
    const handle = xml.doc.?.data[@intFromEnum(eidx)..];
    const len = handle[0] / 2;
    const ptr: [*]const Attribute = @ptrCast(handle[1..].ptr);
    return ptr[0 .. len];
}

// placeholder until tvg updates to 0.13.0
const tvg = struct {
/// A TinyVG scale value. Defines the scale for all units inside a graphic.
/// The scale is defined by the number of decimal bits in a `i32`, thus scaling
/// can be trivially implemented by shifting the integers right by the scale bits.
pub const Scale = enum(u4) {
    const Self = @This();

    @"1/1" = 0,
    @"1/2" = 1,
    @"1/4" = 2,
    @"1/8" = 3,
    @"1/16" = 4,
    @"1/32" = 5,
    @"1/64" = 6,
    @"1/128" = 7,
    @"1/256" = 8,
    @"1/512" = 9,
    @"1/1024" = 10,
    @"1/2048" = 11,
    @"1/4096" = 12,
    @"1/8192" = 13,
    @"1/16384" = 14,
    @"1/32768" = 15,

    pub fn map(self: *const Self, value: f64) Unit {
        return Unit.init64(self.*, value);
    }

    pub fn getShiftBits(self: *const Self) u4 {
        return @intFromEnum(self.*);
    }

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // Original is using u15 instead of u16?
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    pub fn getScaleFactor(self: *const Self) u16 {
        return @as(u16, 1) << self.getShiftBits();
    }
};
pub const Unit = enum(i32) {
    const Self = @This();

    _,

    pub fn init(scale: Scale, value: f32) Self {
        return @enumFromInt(@as(i32, @intFromFloat(value * @as(f32, @floatFromInt(scale.getScaleFactor())) + 0.5)));
    }
    pub fn init64(scale: Scale, value: f64) Self {
        return @enumFromInt(@as(i32, @intFromFloat(value * @as(f64, @floatFromInt(scale.getScaleFactor())) + 0.5)));
    }

    pub fn raw(self: *const Self) i32 {
        return @intFromEnum(self.*);
    }

    pub fn toFloat(self: *const Self, scale: Scale) f32 {
        return @as(f32, @floatFromInt(@intFromEnum(self.*))) / @as(f32, @floatFromInt(scale.getScaleFactor()));
    }

    pub fn toInt(self: *const Self, scale: Scale) i32 {
        const factor = scale.getScaleFactor();
        return @divFloor(@intFromEnum(self.*) + (@divExact(factor, 2)), factor);
    }

    pub fn toUnsignedInt(self: *const Self, scale: Scale) !u31 {
        const i = toInt(self, scale);
        if (i < 0)
            return error.InvalidData;
        return @intCast(i);
    }
};

/// The value range used in the encoding.
pub const Range = enum(u2) {
    /// unit uses 16 bit,
    default = 0,

    /// unit takes only 8 bit
    reduced = 1,

    // unit uses 32 bit,
    enhanced = 2,
};

pub const Color = extern struct {
    const Self = @This();

    r: f32,
    g: f32,
    b: f32,
    a: f32,

    pub fn toRgba8(self: *const Self) [4]u8 {
        return [4]u8{
            @intFromFloat(std.math.clamp(255.0 * self.r, 0.0, 255.0)),
            @intFromFloat(std.math.clamp(255.0 * self.g, 0.0, 255.0)),
            @intFromFloat(std.math.clamp(255.0 * self.b, 0.0, 255.0)),
            @intFromFloat(std.math.clamp(255.0 * self.a, 0.0, 255.0)),
        };
    }

    pub fn lerp(lhs: Self, rhs: Self, factor: f32) Self {
        const l = struct {
            fn l(a: f32, b: f32, c: f32) u8 {
                return @intFromFloat(@as(f32, @floatFromInt(a)) + (@as(f32, @floatFromInt(b)) - @as(f32, @floatFromInt(a))) * std.math.clamp(c, 0, 1));
            }
        }.l;

        return Self{
            .r = l(lhs.r, rhs.r, factor),
            .g = l(lhs.g, rhs.g, factor),
            .b = l(lhs.b, rhs.b, factor),
            .a = l(lhs.a, rhs.a, factor),
        };
    }

    pub fn fromString(str: []const u8) !Self {
        return switch (str.len) {
            3 => Self{
                .r = @as(f32, @floatFromInt(try parseHexSingle(str[0]))) / 255.0,
                .g = @as(f32, @floatFromInt(try parseHexSingle(str[1]))) / 255.0,
                .b = @as(f32, @floatFromInt(try parseHexSingle(str[2]))) / 255.0,
                .a = 1.0,
            },
            6 => Self{
                .r = @as(f32, @floatFromInt(try std.fmt.parseInt(u8, str[0..2], 16))) / 255.0,
                .g = @as(f32, @floatFromInt(try std.fmt.parseInt(u8, str[2..4], 16))) / 255.0,
                .b = @as(f32, @floatFromInt(try std.fmt.parseInt(u8, str[4..6], 16))) / 255.0,
                .a = 1.0,
            },
            else => error.InvalidFormat,
        };
    }
};


fn parseHexSingle(c: u8) !u8 {
    switch (c) {
        '0'...'9' => |v| {
            const val: u8 = (v - '0');
            return val << 4 | val;
        },
        'A'...'F' => |v| {
            const val: u8 = (v - ('A' - 10));
            return val << 4 | val;
        },
        'a'...'f' => |v| {
            const val: u8 = (v - ('a' - 10));
            return val << 4 | val;
        },
        else => return error.InvalidCharacter,
    }
}

};

fn fromRgb(r: u8, g: u8, b: u8) tvg.Color {
    return .{
        .a = 1.0,
        .r = @as(f32, @floatFromInt(r)) / 255.0,
        .g = @as(f32, @floatFromInt(g)) / 255.0,
        .b = @as(f32, @floatFromInt(b)) / 255.0,
    };
}

const named_colors = std.StaticStringMap(tvg.Color).initComptime(.{
    .{ "aliceblue", fromRgb(240, 248, 255) },
    .{ "antiquewhite", fromRgb(250, 235, 215) },
    .{ "aqua", fromRgb(0, 255, 255) },
    .{ "aquamarine", fromRgb(127, 255, 212) },
    .{ "azure", fromRgb(240, 255, 255) },
    .{ "beige", fromRgb(245, 245, 220) },
    .{ "bisque", fromRgb(255, 228, 196) },
    .{ "black", fromRgb(0, 0, 0) },
    .{ "blanchedalmond", fromRgb(255, 235, 205) },
    .{ "blue", fromRgb(0, 0, 255) },
    .{ "blueviolet", fromRgb(138, 43, 226) },
    .{ "brown", fromRgb(165, 42, 42) },
    .{ "burlywood", fromRgb(222, 184, 135) },
    .{ "cadetblue", fromRgb(95, 158, 160) },
    .{ "chartreuse", fromRgb(127, 255, 0) },
    .{ "chocolate", fromRgb(210, 105, 30) },
    .{ "coral", fromRgb(255, 127, 80) },
    .{ "cornflowerblue", fromRgb(100, 149, 237) },
    .{ "cornsilk", fromRgb(255, 248, 220) },
    .{ "crimson", fromRgb(220, 20, 60) },
    .{ "cyan", fromRgb(0, 255, 255) },
    .{ "darkblue", fromRgb(0, 0, 139) },
    .{ "darkcyan", fromRgb(0, 139, 139) },
    .{ "darkgoldenrod", fromRgb(184, 134, 11) },
    .{ "darkgray", fromRgb(169, 169, 169) },
    .{ "darkgreen", fromRgb(0, 100, 0) },
    .{ "darkgrey", fromRgb(169, 169, 169) },
    .{ "darkkhaki", fromRgb(189, 183, 107) },
    .{ "darkmagenta", fromRgb(139, 0, 139) },
    .{ "darkolivegreen", fromRgb(85, 107, 47) },
    .{ "darkorange", fromRgb(255, 140, 0) },
    .{ "darkorchid", fromRgb(153, 50, 204) },
    .{ "darkred", fromRgb(139, 0, 0) },
    .{ "darksalmon", fromRgb(233, 150, 122) },
    .{ "darkseagreen", fromRgb(143, 188, 143) },
    .{ "darkslateblue", fromRgb(72, 61, 139) },
    .{ "darkslategray", fromRgb(47, 79, 79) },
    .{ "darkslategrey", fromRgb(47, 79, 79) },
    .{ "darkturquoise", fromRgb(0, 206, 209) },
    .{ "darkviolet", fromRgb(148, 0, 211) },
    .{ "deeppink", fromRgb(255, 20, 147) },
    .{ "deepskyblue", fromRgb(0, 191, 255) },
    .{ "dimgray", fromRgb(105, 105, 105) },
    .{ "dimgrey", fromRgb(105, 105, 105) },
    .{ "dodgerblue", fromRgb(30, 144, 255) },
    .{ "firebrick", fromRgb(178, 34, 34) },
    .{ "floralwhite", fromRgb(255, 250, 240) },
    .{ "forestgreen", fromRgb(34, 139, 34) },
    .{ "fuchsia", fromRgb(255, 0, 255) },
    .{ "gainsboro", fromRgb(220, 220, 220) },
    .{ "ghostwhite", fromRgb(248, 248, 255) },
    .{ "gold", fromRgb(255, 215, 0) },
    .{ "goldenrod", fromRgb(218, 165, 32) },
    .{ "gray", fromRgb(128, 128, 128) },
    .{ "green", fromRgb(0, 128, 0) },
    .{ "greenyellow", fromRgb(173, 255, 47) },
    .{ "grey", fromRgb(128, 128, 128) },
    .{ "honeydew", fromRgb(240, 255, 240) },
    .{ "hotpink", fromRgb(255, 105, 180) },
    .{ "indianred", fromRgb(205, 92, 92) },
    .{ "indigo", fromRgb(75, 0, 130) },
    .{ "ivory", fromRgb(255, 255, 240) },
    .{ "khaki", fromRgb(240, 230, 140) },
    .{ "lavender", fromRgb(230, 230, 250) },
    .{ "lavenderblush", fromRgb(255, 240, 245) },
    .{ "lawngreen", fromRgb(124, 252, 0) },
    .{ "lemonchiffon", fromRgb(255, 250, 205) },
    .{ "lightblue", fromRgb(173, 216, 230) },
    .{ "lightcoral", fromRgb(240, 128, 128) },
    .{ "lightcyan", fromRgb(224, 255, 255) },
    .{ "lightgoldenrodyellow", fromRgb(250, 250, 210) },
    .{ "lightgray", fromRgb(211, 211, 211) },
    .{ "lightgreen", fromRgb(144, 238, 144) },
    .{ "lightgrey", fromRgb(211, 211, 211) },
    .{ "lightpink", fromRgb(255, 182, 193) },
    .{ "lightsalmon", fromRgb(255, 160, 122) },
    .{ "lightseagreen", fromRgb(32, 178, 170) },
    .{ "lightskyblue", fromRgb(135, 206, 250) },
    .{ "lightslategray", fromRgb(119, 136, 153) },
    .{ "lightslategrey", fromRgb(119, 136, 153) },
    .{ "lightsteelblue", fromRgb(176, 196, 222) },
    .{ "lightyellow", fromRgb(255, 255, 224) },
    .{ "lime", fromRgb(0, 255, 0) },
    .{ "limegreen", fromRgb(50, 205, 50) },
    .{ "linen", fromRgb(250, 240, 230) },
    .{ "magenta", fromRgb(255, 0, 255) },
    .{ "maroon", fromRgb(128, 0, 0) },
    .{ "mediumaquamarine", fromRgb(102, 205, 170) },
    .{ "mediumblue", fromRgb(0, 0, 205) },
    .{ "mediumorchid", fromRgb(186, 85, 211) },
    .{ "mediumpurple", fromRgb(147, 112, 219) },
    .{ "mediumseagreen", fromRgb(60, 179, 113) },
    .{ "mediumslateblue", fromRgb(123, 104, 238) },
    .{ "mediumspringgreen", fromRgb(0, 250, 154) },
    .{ "mediumturquoise", fromRgb(72, 209, 204) },
    .{ "mediumvioletred", fromRgb(199, 21, 133) },
    .{ "midnightblue", fromRgb(25, 25, 112) },
    .{ "mintcream", fromRgb(245, 255, 250) },
    .{ "mistyrose", fromRgb(255, 228, 225) },
    .{ "moccasin", fromRgb(255, 228, 181) },
    .{ "navajowhite", fromRgb(255, 222, 173) },
    .{ "navy", fromRgb(0, 0, 128) },
    .{ "oldlace", fromRgb(253, 245, 230) },
    .{ "olive", fromRgb(128, 128, 0) },
    .{ "olivedrab", fromRgb(107, 142, 35) },
    .{ "orange", fromRgb(255, 165, 0) },
    .{ "orangered", fromRgb(255, 69, 0) },
    .{ "orchid", fromRgb(218, 112, 214) },
    .{ "palegoldenrod", fromRgb(238, 232, 170) },
    .{ "palegreen", fromRgb(152, 251, 152) },
    .{ "paleturquoise", fromRgb(175, 238, 238) },
    .{ "palevioletred", fromRgb(219, 112, 147) },
    .{ "papayawhip", fromRgb(255, 239, 213) },
    .{ "peachpuff", fromRgb(255, 218, 185) },
    .{ "peru", fromRgb(205, 133, 63) },
    .{ "pink", fromRgb(255, 192, 203) },
    .{ "plum", fromRgb(221, 160, 221) },
    .{ "powderblue", fromRgb(176, 224, 230) },
    .{ "purple", fromRgb(128, 0, 128) },
    .{ "rebeccapurple", fromRgb(102, 51, 153) },
    .{ "red", fromRgb(255, 0, 0) },
    .{ "rosybrown", fromRgb(188, 143, 143) },
    .{ "royalblue", fromRgb(65, 105, 225) },
    .{ "saddlebrown", fromRgb(139, 69, 19) },
    .{ "salmon", fromRgb(250, 128, 114) },
    .{ "sandybrown", fromRgb(244, 164, 96) },
    .{ "seagreen", fromRgb(46, 139, 87) },
    .{ "seashell", fromRgb(255, 245, 238) },
    .{ "sienna", fromRgb(160, 82, 45) },
    .{ "silver", fromRgb(192, 192, 192) },
    .{ "skyblue", fromRgb(135, 206, 235) },
    .{ "slateblue", fromRgb(106, 90, 205) },
    .{ "slategray", fromRgb(112, 128, 144) },
    .{ "slategrey", fromRgb(112, 128, 144) },
    .{ "snow", fromRgb(255, 250, 250) },
    .{ "springgreen", fromRgb(0, 255, 127) },
    .{ "steelblue", fromRgb(70, 130, 180) },
    .{ "tan", fromRgb(210, 180, 140) },
    .{ "teal", fromRgb(0, 128, 128) },
    .{ "thistle", fromRgb(216, 191, 216) },
    .{ "tomato", fromRgb(255, 99, 71) },
    .{ "turquoise", fromRgb(64, 224, 208) },
    .{ "violet", fromRgb(238, 130, 238) },
    .{ "wheat", fromRgb(245, 222, 179) },
    .{ "white", fromRgb(255, 255, 255) },
    .{ "whitesmoke", fromRgb(245, 245, 245) },
    .{ "yellow", fromRgb(255, 255, 0) },
    .{ "yellowgreen", fromRgb(154, 205, 5) },
});
