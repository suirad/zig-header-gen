const std = @import("std");
const builtin = std.builtin;
const TypeInfo = builtin.TypeInfo;
const Declaration = TypeInfo.Declaration;
const warn = std.debug.warn;

// Provided generators
pub const C_Generator = @import("generators/c.zig").C_Generator;
pub const Python_Generator = @import("generators/python.zig").Python_Generator;

const GeneratorInterface = struct {
    fn init() void {}
    fn deinit() void {}
    fn gen_func() void {}
    fn gen_struct() void {}
    fn gen_enum() void {}
    fn gen_union() void {}
};

fn validateGenerator(comptime Generator: type) void {
    comptime {
        const interface = @typeInfo(GeneratorInterface).Struct.decls;

        for (interface) |decl| {
            if (@hasDecl(Generator, decl.name) == false) {
                @compileError("Generator: '" ++
                    @typeName(Generator) ++
                    "' is missing function: " ++
                    decl.name);
            }
        }
    }
}

pub fn HeaderGen(comptime fname: []const u8) type {
    comptime var all_decls: []const Declaration = undefined;
    comptime {
        const import = @typeInfo(@import(fname));
        all_decls = import.Struct.decls;
    }
    return struct {
        decls: @TypeOf(all_decls) = all_decls,
        source_file: []const u8 = fname,

        const Self = @This();

        pub fn init() Self {
            return Self{};
        }

        pub fn exec(comptime self: Self, comptime Generator: type) void {
            validateGenerator(Generator);

            var cwd = std.fs.cwd();
            cwd.makeDir("headers") catch |e| switch (e) {
                error.PathAlreadyExists => {},
                else => @panic("Failed to init header folder"),
            };

            var hdr_dir = cwd.openDir("headers", .{}) catch @panic("Failed to open header dir");
            defer hdr_dir.close();

            var gen = Generator.init(self.source_file, &hdr_dir);
            defer gen.deinit();

            // iterate exported enums
            // do this first in case target lang needs enums defined before use
            inline for (self.decls) |decl| {
                if (decl.data == .Type) {
                    const T = decl.data.Type;
                    const info = @typeInfo(T);
                    if (info == .Enum) {
                        const layout = info.Enum.layout;
                        if (layout == .Extern) {
                            gen.gen_enum(decl.name, info.Enum);
                        }
                    }
                }
            }

            // iterate exported structs
            inline for (self.decls) |decl| {
                if (decl.data == .Type) {
                    const T = decl.data.Type;
                    const info = @typeInfo(T);
                    if (info == .Struct) {
                        const layout = info.Struct.layout;
                        if (layout == .Extern or layout == .Packed) {
                            gen.gen_struct(decl.name, info.Struct);
                        }
                    }
                }
            }

            inline for (self.decls) |decl| {
                if (decl.data == .Type) {
                    const T = decl.data.Type;
                    const info = @typeInfo(T);
                    if (info == .Union) {
                        const layout = info.Union.layout;
                        if (layout == .Extern) {
                            gen.gen_union(decl.name, info.Union);
                        }
                    }
                }
            }

            // iterate exported fns
            inline for (self.decls) |decl| {
                if (decl.data == .Fn) {
                    const func = decl.data.Fn;
                    if (func.is_export) {
                        //TODO: Look into parsing file for argument names
                        const fn_meta = @typeInfo(func.fn_type).Fn;
                        gen.gen_func(decl.name, func, fn_meta);
                    }
                }
            }
        }
    };
}
