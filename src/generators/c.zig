const std = @import("std");
const Dir = std.fs.Dir;
const FnMeta = std.builtin.TypeInfo.Fn;
const FnDecl = std.builtin.TypeInfo.Declaration.Data.FnDecl;
const StructMeta = std.builtin.TypeInfo.Struct;
const EnumMeta = std.builtin.TypeInfo.Enum;
const UnionMeta = std.builtin.TypeInfo.Union;
const warn = std.debug.warn;

pub const C_Generator = struct {
    file: std.fs.File,

    const Self = @This();

    pub fn init(comptime src_file: []const u8, dst_dir: *Dir) Self {
        comptime const filebaseext = std.fs.path.basename(src_file);
        comptime const filebase = filebaseext[0 .. filebaseext.len - 4];

        var file = dst_dir.createFile(filebase ++ ".h", .{}) catch
            @panic("Failed to create header file for source: " ++ src_file);

        var res = Self{ .file = file };

        // write the header's header, lol
        res.write("#ifndef _" ++ filebase ++ "_H\n\n#define _" ++ filebase ++ "_H\n");
        res.write("#include <stddef.h>\n#include <stdint.h>\n#include <stdbool.h>\n\n");

        return res;
    }

    pub fn deinit(self: *Self) void {
        self.write("\n#endif\n");
        self.file.close();
    }

    pub fn gen_func(self: *Self, comptime name: []const u8, comptime meta: FnMeta) void {
        switch (meta.calling_convention) {
            .Naked => self.write("__attribute__((naked)) "),
            .Stdcall => self.write("__attribute__((stdcall)) "),
            .Fastcall => self.write("__attribute__((fastcall)) "),
            .Thiscall => self.write("__attribute__((thiscall)) "),
            else => {},
        }

        self.writeType(meta.return_type.?);
        self.write(" " ++ name ++ "(");

        inline for (meta.args) |arg, i| {
            self.writeType(arg.arg_type.?);
            //TODO: Figure out how to get arg names; for now just do arg0..argN
            _ = self.file.writer().print(" arg{}", .{i}) catch unreachable;
            if (i != meta.args.len - 1)
                self.write(", ");
        }

        self.write(");\n\n");
    }

    pub fn gen_struct(self: *Self, comptime name: []const u8, comptime meta: StructMeta) void {
        self.write("typedef struct ");

        if (meta.layout == .Packed)
            self.write("__attribute__((__packed__)) ");

        self.write(name ++ " {\n");

        inline for (meta.fields) |field| {
            self.write("   ");

            const info = @typeInfo(field.field_type);

            if (info == .Array) {
                self.writeType(info.Array.child);
            } else {
                self.writeType(field.field_type);
            }

            self.write(" " ++ field.name);

            if (info == .Array) {
                _ = self.file.writer().print("[{}]", .{info.Array.len}) catch unreachable;
            }

            self.write(";\n");
        }
        self.write("} " ++ name ++ "_t;\n\n");
    }

    pub fn gen_enum(self: *Self, comptime name: []const u8, comptime meta: EnumMeta) void {
        self.write("enum " ++ name ++ " {\n");

        comptime var last = 0;
        inline for (meta.fields) |field, i| {
            self.write("    " ++ field.name);

            // if field value is unexpected/custom, manually define it
            if ((i == 0 and field.value != 0) or (i > 0 and field.value > last + 1)) {
                _ = self.file.writer().print(" = {}", .{field.value}) catch unreachable;
            }

            self.write(",\n");

            last = field.value;
        }

        self.write("};\n\n");
    }

    pub fn gen_union(self: *Self, comptime name: []const u8, comptime meta: UnionMeta) void {
        self.write("typedef union ");

        self.write(name ++ " {\n");

        inline for (meta.fields) |field| {
            self.write("   ");
            self.writeType(field.field_type);
            self.write(" " ++ field.name ++ ";\n");
        }
        self.write("} " ++ name ++ "_t;\n\n");
    }

    fn writeType(self: *Self, comptime T: type) void {
        switch (T) {
            void => self.write("void"),
            bool => self.write("bool"),
            usize => self.write("size_t"),
            isize => self.write("int"),
            u8 => self.write("uint8_t"),
            u16 => self.write("uint16_t"),
            u32 => self.write("uint32_t"),
            u64 => self.write("uint64_t"),
            i8 => self.write("int8_t"),
            i16 => self.write("int16_t"),
            i24 => self.write("int24_t"),
            i32 => self.write("int32_t"),
            i64 => self.write("int64_t"),
            [*]bool => self.write("bool*"),
            [*]usize => self.write("size_t*"),
            [*]isize => self.write("int*"),
            [*]u8 => self.write("uint8_t*"),
            [*]u16 => self.write("uint16_t*"),
            [*]u32 => self.write("uint32_t*"),
            [*]u64 => self.write("uint64_t*"),
            [*]i8 => self.write("int8_t*"),
            [*]i16 => self.write("int16_t*"),
            [*]i32 => self.write("int32_t*"),
            [*]i64 => self.write("int64_t*"),
            else => {
                const meta = @typeInfo(T);
                switch (meta) {
                    .Pointer => {
                        const child = meta.Pointer.child;
                        const childmeta = @typeInfo(child);
                        if (childmeta == .Struct and childmeta.Struct.layout != .Extern) {
                            self.write("void");
                        } else {
                            self.writeType(child);
                        }
                        self.write("*");
                    },
                    .Optional => self.writeType(meta.Optional.child),
                    .Array => @compileError("Handle goofy looking C Arrays in the calling function"),
                    else => self.write(@typeName(T) ++ "_t"),
                }
            },
        }
    }

    fn write(self: *Self, str: []const u8) void {
        _ = self.file.writeAll(str) catch unreachable;
    }
};
