const std = @import("std");
const Allocator = std.mem.Allocator;

pub const TypeId = @TagType(TypeInfo);

pub const TypeInfo = union(enum) {
    Type: void,
    Void: void,
    Bool: void,
    NoReturn: void,
    Int: Int,
    Float: Float,
    Pointer: Pointer,
    Array: Array,
    Struct: Struct,
    ComptimeFloat: void,
    ComptimeInt: void,
    Undefined: void,
    Null: void,
    Optional: Optional,
    ErrorUnion: ErrorUnion,
    ErrorSet: ErrorSet,
    Enum: Enum,
    Union: Union,
    Fn: Fn,
    BoundFn: Fn,
    Opaque: void,
    Frame: Frame,
    AnyFrame: AnyFrame,
    Vector: Vector,
    EnumLiteral: void,

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Int = struct {
        is_signed: bool,
        bits: i32,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Float = struct {
        bits: i32,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Pointer = struct {
        size: Size,
        is_const: bool,
        is_volatile: bool,
        alignment: i32,
        child: *TypeInfo,
        is_allowzero: bool,
        /// This field is an optional type.
        /// The type of the sentinel is the element type of the pointer, which is
        /// the value of the `child` field in this struct. However there is no way
        /// to refer to that type here, so we use `var`.
        // sentinel: var,
        /// This data structure is used by the Zig language code generation and
        /// therefore must be kept in sync with the compiler implementation.
        pub const Size = enum {
            One,
            Many,
            Slice,
            C,
        };

        pub fn deinit(self: *const Pointer, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Array = struct {
        len: i32,
        child: *TypeInfo,
        /// This field is an optional type.
        /// The type of the sentinel is the element type of the array, which is
        /// the value of the `child` field in this struct. However there is no way
        /// to refer to that type here, so we use `var`.
        // sentinel: var,
        pub fn deinit(self: *const Array, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ContainerLayout = enum {
        Auto,
        Extern,
        Packed,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const StructField = struct {
        name: []const u8,
        field_type: *TypeInfo,
        // default_value: var,

        pub fn deinit(self: *const StructField, allocator: *Allocator) void {
            allocator.free(self.name);

            self.field_type.deinit(allocator);

            allocator.destroy(self.field_type);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Struct = struct {
        name: ?[]const u8,
        layout: ContainerLayout,
        fields: []const StructField,
        decls: []const Declaration,

        pub fn deinit(self: *const Struct, allocator: *Allocator) void {
            for (self.fields) |f| f.deinit(allocator);
            for (self.decls) |f| f.deinit(allocator);

            allocator.free(self.fields);
            allocator.free(self.decls);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Optional = struct {
        child: *TypeInfo,

        pub fn deinit(self: *const Optional, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ErrorUnion = struct {
        error_set: *TypeInfo,
        payload: *TypeInfo,

        pub fn deinit(self: *const ErrorUnion, allocator: *Allocator) void {
            self.error_set.deinit(allocator);
            allocator.destroy(self.error_set);

            self.payload.deinit(allocator);
            allocator.destroy(self.payload);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Error = struct {
        name: []const u8,

        pub fn deinit(self: *const Error, allocator: *Allocator) void {
            allocator.free(self.name);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ErrorSet = ?[]const Error;

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const EnumField = struct {
        name: []const u8,
        value: i32,

        pub fn deinit(self: *const EnumField, allocator: *Allocator) void {
            allocator.free(self.name);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Enum = struct {
        name: ?[]const u8,
        layout: ContainerLayout,
        tag_type: *TypeInfo,
        fields: []const EnumField,
        decls: []const Declaration,
        is_exhaustive: bool,

        pub fn deinit(self: *const Enum, allocator: *Allocator) void {
            for (self.fields) |f| f.deinit(allocator);
            for (self.decls) |f| f.deinit(allocator);

            allocator.free(self.fields);
            allocator.free(self.decls);

            self.tag_type.deinit(allocator);
            allocator.destroy(self.tag_type);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const UnionField = struct {
        name: []const u8,
        enum_field: ?EnumField,
        field_type: *TypeInfo,

        pub fn deinit(self: *const UnionField, allocator: *Allocator) void {
            allocator.free(self.name);

            self.field_type.deinit(allocator);

            allocator.destroy(self.field_type);

            if (self.enum_field) |ef| {
                ef.deinit(allocator);
            }
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Union = struct {
        name: ?[]const u8,
        layout: ContainerLayout,
        tag_type: ?*TypeInfo,
        fields: []const UnionField,
        decls: []const Declaration,

        pub fn deinit(self: *const Union, allocator: *Allocator) void {
            for (self.fields) |f| f.deinit(allocator);
            for (self.decls) |f| f.deinit(allocator);

            allocator.free(self.fields);
            allocator.free(self.decls);

            if (self.tag_type) |tag_type| {
                tag_type.deinit(allocator);

                allocator.destroy(tag_type);
            }
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const FnArg = struct {
        is_generic: bool,
        is_noalias: bool,
        arg_type: ?*TypeInfo,

        pub fn deinit(self: *const FnArg, allocator: *Allocator) void {
            if (self.arg_type) |t| {
                t.deinit(allocator);

                allocator.destroy(self.arg_type);
            }
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Fn = struct {
        calling_convention: CallingConvention,
        is_generic: bool,
        is_var_args: bool,
        return_type: ?*TypeInfo,
        args: []const FnArg,

        pub fn deinit(self: *const Fn, allocator: *Allocator) void {
            if (self.return_type) |r| {
                r.deinit(allocator);

                allocator.destroy(r);
            }

            for (self.args) |arg| arg.deinit(allocator);

            allocator.free(self.args);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Frame = struct {
        // function: var,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const AnyFrame = struct {
        child: ?*TypeInfo,

        pub fn deinit(self: *const AnyFrame, allocator: *Allocator) void {
            if (self.child) |child| {
                child.deinit(allocator);

                allocator.destroy(child);
            }
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Vector = struct {
        len: i32,
        child: *TypeInfo,

        pub fn deinit(self: *const Vector, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Declaration = struct {
        name: []const u8,
        is_pub: bool,
        data: Data,

        pub fn deinit(self: *const Declaration, allocator: *Allocator) void {
            self.data.deinit(allocator);

            allocator.free(self.name);
        }

        /// This data structure is used by the Zig language code generation and
        /// therefore must be kept in sync with the compiler implementation.
        pub const Data = union(enum) {
            Type: *TypeInfo,
            Var: *TypeInfo,
            Fn: FnDecl,

            /// This data structure is used by the Zig language code generation and
            /// therefore must be kept in sync with the compiler implementation.
            pub const FnDecl = struct {
                fn_type: *TypeInfo,
                inline_type: Inline,
                is_var_args: bool,
                is_extern: bool,
                is_export: bool,
                lib_name: ?[]const u8,
                return_type: *TypeInfo,
                arg_names: []const []const u8,

                /// This data structure is used by the Zig language code generation and
                /// therefore must be kept in sync with the compiler implementation.
                pub const Inline = enum {
                    Auto,
                    Always,
                    Never,
                };

                pub fn deinit(self: *const FnDecl, allocator: *Allocator) void {
                    self.fn_type.deinit(allocator);
                    self.return_type.deinit(allocator);

                    allocator.destroy(self.fn_type);
                    allocator.destroy(self.return_type);

                    for (self.arg_names) |a| allocator.free(a);
                    allocator.free(self.arg_names);

                    if (self.lib_name) |lib_name| {
                        allocator.free(lib_name);
                    }
                }
            };

            pub fn deinit(self: *const Data, allocator: *Allocator) void {
                switch (self.*) {
                    .Type, .Var => |t| {
                        t.deinit(allocator);

                        allocator.destroy(t);
                    },
                    .Fn => |f| f.deinit(allocator),
                }
            }
        };
    };

    pub fn init(allocator: *Allocator, comptime builtin: std.builtin.TypeInfo) TypeInfo {
        return TypeInfo.copy(TypeInfo, allocator, builtin);
    }

    pub fn init2(allocator: *Allocator, comptime builtin: std.builtin.TypeInfo, comptime depth: u32) TypeInfo {
        return TypeInfo.copy2(TypeInfo, allocator, builtin, depth);
    }

    pub fn deinit(self: *TypeInfo, allocator: *Allocator) void {
        switch (self.*) {
            .Array => |a| a.deinit(allocator),
            .Pointer => |p| p.deinit(allocator),
            .Struct => |s| s.deinit(allocator),
            .Union => |u| u.deinit(allocator),
            .Enum => |e| e.deinit(allocator),
            .Optional => |o| o.deinit(allocator),
            .Fn => |f| f.deinit(allocator),
            .ErrorUnion => |e| e.deinit(allocator),
            .ErrorSet => |maybe_set| {
                if (maybe_set) |set| {
                    for (set) |err| err.deinit(allocator);

                    allocator.free(set);
                }
            },
            .AnyFrame => |a| a.deinit(allocator),
            .Vector => |v| v.deinit(allocator),
            else => {},
        }
    }

    pub fn copy(comptime T: type, allocator: *Allocator, comptime src: var) T {
        return TypeInfo.copy2(T, allocator, src, 5);
    }

    pub fn copy2(comptime T: type, allocator: *Allocator, comptime src: var, comptime depth: u32) T {
        comptime const info = @typeInfo(T);

        if (info == .Void) {
            return;
        } else if (info == .Pointer and info.Pointer.size == .One and info.Pointer.child == TypeInfo) {
            var ptr = allocator.create(TypeInfo) catch unreachable;

            if (depth > 0) {
                ptr.* = TypeInfo.init2(allocator, @typeInfo(src), depth - 1);

                if (ptr.* == .Struct) ptr.*.Struct.name = @typeName(src)
                else if (ptr.* == .Union) ptr.*.Union.name = @typeName(src)
                else if (ptr.* == .Enum) ptr.*.Enum.name = @typeName(src);
            } else {
                // TODO Create special variant for max-depth
                ptr.* = TypeInfo{ .Void = {} };
            }

            return ptr;
        } else if (info == .Enum) {
            return @intToEnum(T, @enumToInt(src));
        } else if (info == .Pointer and info.Pointer.size == .Slice) {
            var dst_slice = allocator.alloc(info.Pointer.child, src.len) catch unreachable;

            inline for (src) |src_val, i| {
                dst_slice[i] = TypeInfo.copy2(info.Pointer.child, allocator, src_val, depth);
            }

            return dst_slice;
        } else if (info == .Struct) {
            var obj: T = undefined;

            inline for (info.Struct.fields) |struct_field| {
                if (comptime @hasField(@TypeOf(src), struct_field.name)) {
                    @field(obj, struct_field.name) = TypeInfo.copy2(struct_field.field_type, allocator, @field(src, struct_field.name), depth);
                }
            }

            return obj;
        } else if (info == .Union) {
            inline for (info.Union.fields) |union_field| {
                comptime var tag_type = @field(@TagType(@TypeOf(src)), union_field.enum_field.?.name);

                if (src == tag_type) {
                    var obj = TypeInfo.copy2(union_field.field_type, allocator, @field(src, union_field.name), depth);

                    return @unionInit(T, union_field.name, obj);
                }
            }

            @panic("Type not runtimable");
        } else if (info == .Optional) {
            if (src) |src_val| {
                return TypeInfo.copy2(info.Optional.child, allocator, src_val, depth);
            } else {
                return null;
            }
        } else {
            return @as(T, src);
        }
    }
};

pub const CallingConvention = enum {
    Unspecified,
    C,
    Cold,
    Naked,
    Async,
    Interrupt,
    Signal,
    Stdcall,
    Fastcall,
    Vectorcall,
    Thiscall,
    APCS,
    AAPCS,
    AAPCSVFP,
};

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const talloc = std.testing.allocator;

// TODO .Type

test "Runtime TypeInfo.Void" {
    var info_void = TypeInfo.init(talloc, @typeInfo(void));
    expect(info_void == .Void);
    info_void.deinit(talloc);
}
test "Runtime TypeInfo.Bool" {
    var info_bool = TypeInfo.init(talloc, @typeInfo(bool));
    expect(info_bool == .Bool);
    info_bool.deinit(talloc);
}

// TODO .NoReturn

test "Runtime TypeInfo.Int" {
    var info_i32 = TypeInfo.init(talloc, @typeInfo(i32));
    expect(info_i32 == .Int);
    expectEqual(@as(i32, 32), info_i32.Int.bits);
    expectEqual(true, info_i32.Int.is_signed);
    info_i32.deinit(talloc);
}

test "Runtime TypeInfo.Float" {
    var info_f64 = TypeInfo.init(talloc, @typeInfo(f64));
    expect(info_f64 == .Float);
    expectEqual(@as(i32, 64), info_f64.Float.bits);
    info_f64.deinit(talloc);
}

test "Runtime TypeInfo.Pointer" {
    var info_pointer_f64 = TypeInfo.init(talloc, @typeInfo(*f64));
    expect(info_pointer_f64 == .Pointer);
    expectEqual(TypeInfo.Pointer.Size.One, info_pointer_f64.Pointer.size);
    expectEqual(false, info_pointer_f64.Pointer.is_const);
    expectEqual(false, info_pointer_f64.Pointer.is_volatile);
    expectEqual(@as(i32, 8), info_pointer_f64.Pointer.alignment);
    expect(info_pointer_f64.Pointer.child.* == .Float);
    expectEqual(false, info_pointer_f64.Pointer.is_allowzero);
    info_pointer_f64.deinit(talloc);

    var info_pointer_many = TypeInfo.init(talloc, @typeInfo([*]f64));
    expect(info_pointer_many == .Pointer);
    expectEqual(TypeInfo.Pointer.Size.Many, info_pointer_many.Pointer.size);
    expectEqual(false, info_pointer_many.Pointer.is_const);
    expectEqual(false, info_pointer_many.Pointer.is_volatile);
    expectEqual(@as(i32, 8), info_pointer_many.Pointer.alignment);
    expect(info_pointer_many.Pointer.child.* == .Float);
    expectEqual(false, info_pointer_many.Pointer.is_allowzero);
    info_pointer_many.deinit(talloc);
}

test "Runtime TypeInfo.Array" {
    var info_array = TypeInfo.init(talloc, @typeInfo([2]i32));
    expect(info_array == .Array);
    expectEqual(@as(i32, 2), info_array.Array.len);
    expect(info_array.Array.child.* == .Int);
    info_array.deinit(talloc);
}

test "Runtime TypeInfo.Struct" {
    const FooStruct = struct {
        int: i32,

        pub fn bar() void {}
    };

    var info_struct = TypeInfo.init(talloc, @typeInfo(FooStruct));
    expect(info_struct == .Struct);
    expect(info_struct.Struct.layout == .Auto);
    expectEqual(@as(usize, 1), info_struct.Struct.fields.len);
    expectEqualStrings("int", info_struct.Struct.fields[0].name);
    expect(info_struct.Struct.fields[0].field_type.* == .Int);
    info_struct.deinit(talloc);
}

test "Runtime TypeInfo.ComptimeFloat" {
    var info_comptime_float = TypeInfo.init(talloc, @typeInfo(comptime_float));
    expect(info_comptime_float == .ComptimeFloat);
    info_comptime_float.deinit(talloc);
}

test "Runtime TypeInfo.ComptimeInt" {
    var info_comptime_int = TypeInfo.init(talloc, @typeInfo(comptime_int));
    expect(info_comptime_int == .ComptimeInt);
    info_comptime_int.deinit(talloc);
}

// TODO .Undefined
// TODO .Null

test "Runtime TypeInfo.Optional" {
    var info_optional = TypeInfo.init(talloc, @typeInfo(?i32));
    expect(info_optional == .Optional);
    expect(info_optional.Optional.child.* == .Int);
    info_optional.deinit(talloc);
}

// TODO .ErrorUnion
// TODO .ErrorSet

test "Runtime TypeInfo.Enum" {
    const FooEnum = enum {
        Foo, Bar
    };

    var info_enum = TypeInfo.init(talloc, @typeInfo(FooEnum));
    expect(info_enum == .Enum);
    info_enum.deinit(talloc);
}

test "Runtime TypeInfo.Union" {
    const FooUnion = union {
        Foo: void, Bar: i32
    };

    var info_union = TypeInfo.init(talloc, @typeInfo(FooUnion));
    expect(info_union == .Union);
    info_union.deinit(talloc);
}

test "Runtime TypeInfo.Fn" {
    // .Fn
    var info_fn = TypeInfo.init(talloc, @typeInfo(fn () void));
    expect(info_fn == .Fn);
    info_fn.deinit(talloc);
}

// TODO .BoundFn
// TODO .Opaque
// TODO .Frame
// TODO .AnyFrame
// TODO .Vector
// TODO .EnumLiteral
