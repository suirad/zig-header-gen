const std = @import("std");
const Dir = std.fs.Dir;
const warn = std.debug.warn;
const rt = @import("../runtime.zig");
const FnDecl = rt.TypeInfo.Declaration.Data.FnDecl;
const FnMeta = rt.TypeInfo.Fn;
const StructMeta = rt.TypeInfo.Struct;
const EnumMeta = rt.TypeInfo.Enum;
const UnionMeta = rt.TypeInfo.Union;
const SymbolPhase = @import("ordered.zig").SymbolPhase;

pub const Python_Generator = struct {
    pub const symbols_order: bool = false;

    file: std.fs.File,

    const Self = @This();

    pub fn init(comptime src_file: []const u8, dst_dir: *Dir) Self {
        comptime const filebaseext = std.fs.path.basename(src_file);
        // The .len - 4 assumes a .zig extension
        comptime const filebase = filebaseext[0 .. filebaseext.len - 4];

        var file = dst_dir.createFile(filebase ++ ".py", .{}) catch
            @panic("Failed to create header file for source: " ++ src_file);

        var res = Self{ .file = file };

        res.write(
            \\import ctypes            
            \\import enum
            \\
        );

        res.write("lib = ctypes.cdll.LoadLibrary(\"" ++ filebase ++ ".dll\")\n\n");

        return res;
    }

    pub fn deinit(self: *Self) void {
        self.file.close();
    }

    pub fn gen_func(self: *Self, name: []const u8, meta: FnMeta) void {
        self.print("lib.{}.argtypes = [", .{name});

        for (meta.args) |arg, i| {
            if (arg.arg_type) |t| {
                self.writeType(t.*);
            } else {
                self.write("None");
            }

            if (i != meta.args.len - 1) {
                self.write(", ");
            }
        }

        self.write("]\n");

        self.print("lib.{}.restype = ", .{name});
        if (meta.return_type) |return_type| {
            self.writeType(return_type.*);
        } else {
            self.write("None");
        }
        self.write("\n\n");
    }

    pub fn _gen_fields(self: *Self, name: []const u8, fields: var, phase: SymbolPhase) void {
        comptime const prefix = "\t            ";

        if (phase == .Body) {
            self.print("{}._fields_ = [", .{name});
        } else {
            self.write("\t_fields_ = [");
        }

        for (fields) |field, i| {
            if (i > 0) {
                self.write(prefix);
            }

            self.print("(\"{}\", ", .{field.name});

            const info = field.field_type.*;

            if (info == .Array) {
                self.writeType(info.Array.child.*);
            } else {
                self.writeType(info);
            }

            if (info == .Array) {
                self.print(" * {}", .{info.Array.len});
            }

            self.write(")");

            if (i != fields.len - 1) {
                self.write(",\n");
            }
        }

        self.write("]\n");
    }

    pub fn gen_struct(self: *Self, name: []const u8, meta: StructMeta, phase: SymbolPhase) void {
        if (phase != .Body) {
            self.print("class {}(ctypes.Structure):\n", .{name});

            if (meta.layout == .Packed) {
                self.write("\t_pack_ = 1\n");
            }
        }

        if (phase != .Signature) {
            self._gen_fields(name, meta.fields, phase);
        } else if (meta.layout != .Packed) {
            self.write("\tpass\n");
        }

        self.write("\n");
    }

    pub fn gen_enum(self: *Self, name: []const u8, meta: EnumMeta, phase: SymbolPhase) void {
        self.print("class {}(enum.IntEnum):\n", .{name});

        for (meta.fields) |field, i| {
            self.write("\t");
            self.writeScreamingSnakeCase(field.name);
            self.print(" = {}\n", .{field.value});
        }

        if (meta.fields.len == 0) {
            self.write("\tpass");
        }

        self.write("\n");
    }

    pub fn gen_union(self: *Self, name: []const u8, meta: UnionMeta, phase: SymbolPhase) void {
        if (phase != .Body) {
            self.print("class {}(ctypes.Union):\n", .{name});
        }

        if (phase != .Signature) {
            self._gen_fields(name, meta.fields, phase);
        } else {
            self.write("\tpass\n");
        }

        self.write("\n");
    }

    fn writeType(self: *Self, meta: rt.TypeInfo) void {
        switch (meta) {
            .Void => self.write("None"),
            .Bool => self.write("ctypes.c_bool"),
            // .usize => self.writeCtype("c_usize"), // TODO
            // .isize => self.writeCtype("c_isize"), // TODO
            .Int => |i| {
                switch (i.is_signed) {
                    true => self.print("ctypes.c_int{}", .{i.bits}),
                    false => self.print("ctypes.c_uint{}", .{i.bits}),
                }
            },
            .Float => |f| {
                switch (f.bits) {
                    32 => self.write("c_float"),
                    64 => self.write("c_double"),
                    128 => self.write("c_longdouble"),
                    else => self.print("ctypes.c_f{}", .{f.bits}),
                }
            },
            .Struct => |s| self.write(s.name orelse "__unknown__"),
            .Union => |s| self.write(s.name orelse "__unknown__"),
            .Enum => |s| self.write(s.name orelse "__unknown__"),
            .Pointer => |p| {
                const childmeta = p.child.*;
                self.writeCtype("POINTER(");
                if (childmeta == .Struct and childmeta.Struct.layout != .Extern) {
                    self.writeCtype("c_size_t");
                } else {
                    self.writeType(childmeta);
                }
                self.write(")");
            },
            .Optional => self.writeType(meta.Optional.child.*),
            .Array => {}, // TODO @compileError("Handle goofy looking C Arrays in the calling function"),
            else => self.write(@tagName(meta)), // TODO!!!!!
        }
    }

    fn writeScreamingSnakeCase(self: *Self, str: []const u8) void {
        var new_word: bool = false;
        var was_lower: bool = false;
        var is_upper: bool = undefined;

        for (str) |char, i| {
            is_upper = std.ascii.isUpper(char);

            if (char == '_' and i > 0) {
                new_word = true;
                continue;
            }

            if (new_word == true or (is_upper and was_lower)) {
                new_word = false;
                was_lower = false;

                self.writeChar('_');
            } else {
                was_lower = !is_upper;
            }

            self.writeChar(std.ascii.toUpper(char));
        }
    }

    fn writeCtype(self: *Self, comptime str: []const u8) void {
        self.write("ctypes." ++ str);
    }

    fn writeChar(self: *Self, char: u8) void {
        self.write(&[1]u8{char});
    }

    fn print(self: *Self, comptime fmt: []const u8, args: var) void {
        self.file.writer().print(fmt, args) catch unreachable;
    }

    fn write(self: *Self, str: []const u8) void {
        _ = self.file.writeAll(str) catch unreachable;
    }
};
