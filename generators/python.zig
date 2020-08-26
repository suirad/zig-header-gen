const std = @import("std");
const Dir = std.fs.Dir;
const FnMeta = std.builtin.TypeInfo.Fn;
const FnDecl = std.builtin.TypeInfo.Declaration.Data.FnDecl;
const StructMeta = std.builtin.TypeInfo.Struct;
const EnumMeta = std.builtin.TypeInfo.Enum;
const UnionMeta = std.builtin.TypeInfo.Union;
const warn = std.debug.warn;

pub const Python_Generator = struct {
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

    pub fn gen_func(self: *Self, comptime name: []const u8, comptime func: FnDecl, comptime meta: FnMeta) void {
        self.write("lib." ++ name ++ ".argtypes = [");

        inline for (meta.args) |arg, i| {
            self.writeType(arg.arg_type.?);

            if (i != meta.args.len - 1) {
                self.write(", ");
            }
        }

        self.write("]\n");

        self.write("lib." ++ name ++ ".restype = ");
        self.writeType(func.return_type);
        self.write("\n\n");
    }

    pub fn _gen_fields(self: *Self, comptime fields: var) void {
        comptime const prefix = "\t            ";

        self.write("\t_fields_ = [");

        inline for (fields) |field, i| {
            if (i > 0) {
                self.write(prefix);
            }

            self.write("(");

            self.write("\"" ++ field.name ++ "\", ");

            const info = @typeInfo(field.field_type);

            if (info == .Array) {
                self.writeType(info.Array.child);
            } else {
                self.writeType(field.field_type);
            }

            if (info == .Array) {
                _ = self.file.writer().print(" * {}", .{info.Array.len}) catch unreachable;
            }

            self.write(")");

            if (i != fields.len - 1) {
                self.write(",\n");
            }
        }

        self.write("]\n");
    }

    pub fn gen_struct(self: *Self, comptime name: []const u8, comptime meta: StructMeta) void {
        self.write("class " ++ name ++ "(ctypes.Structure):\n");

        if (meta.layout == .Packed) {
            self.write("\t_pack_ = 1\n");
        }

        self._gen_fields(meta.fields);

        self.write("\n");
    }

    pub fn gen_enum(self: *Self, comptime name: []const u8, comptime meta: EnumMeta) void {
        self.write("class " ++ name ++ "(enum.IntEnum):\n");

        inline for (meta.fields) |field, i| {
            self.write("\t");
            self.writeScreamingSnakeCase(field.name);
            _ = self.file.writer().print(" = {}", .{field.value}) catch unreachable;
            self.write("\n");
        }

        self.write("\n");
    }

    pub fn gen_union(self: *Self, comptime name: []const u8, comptime meta: UnionMeta) void {
        self.write("class " ++ name ++ "(ctypes.Union):\n");

        self._gen_fields(meta.fields);

        self.write("\n");
    }

    fn writeType(self: *Self, comptime T: type) void {
        switch (T) {
            void => self.write("None"),
            bool => self.writeCtype("c_bool"),
            usize => self.writeCtype("c_size_t"),
            isize => self.writeCtype("c_int"), // TODO
            u8 => self.writeCtype("c_uint8"),
            u16 => self.writeCtype("c_uint16"),
            u32 => self.writeCtype("c_uint32"),
            u64 => self.writeCtype("c_uint64"),
            i8 => self.writeCtype("c_int8"),
            i16 => self.writeCtype("c_int16"),
            i24 => self.writeCtype("int24_t"), // TODO
            i32 => self.writeCtype("c_int32"),
            i64 => self.writeCtype("c_int54"),
            f32 => self.writeCtype("c_float"),
            f64 => self.writeCtype("c_double"),
            f128 => self.writeCtype("c_longdouble"),
            else => {
                const meta = @typeInfo(T);
                switch (meta) {
                    .Pointer => {
                        const child = meta.Pointer.child;
                        const childmeta = @typeInfo(child);
                        self.writeCtype("POINTER(");
                        if (childmeta == .Struct and childmeta.Struct.layout != .Extern) {
                            self.writeCtype("c_size_t");
                        } else {
                            self.writeType(child);
                        }
                        self.write(")");
                    },
                    .Optional => self.writeType(meta.Optional.child),
                    .Array => @compileError("Handle goofy looking C Arrays in the calling function"),
                    else => self.write(@typeName(T)),
                }
            },
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

    fn write(self: *Self, str: []const u8) void {
        _ = self.file.writeAll(str) catch unreachable;
    }
};
