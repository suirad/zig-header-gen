const std = @import("std");
const Allocator = std.mem.Allocator;

pub const TypeId = @TagType(TypeInfo);

const TypeInfoSingleton = struct {
    resolved: bool = false,
    info: TypeInfo = .{ .Void = {} },
};

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
    Opaque: void, // TODO Opaque
    Frame: Frame,
    AnyFrame: AnyFrame,
    Vector: Vector,
    EnumLiteral: void,

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Int = struct {
        signedness: Signedness,
        bits: i32,

        pub fn init(comptime m: std.builtin.TypeInfo.Int) Int {
            return comptime .{
                .signedness = @intToEnum(Signedness, @enumToInt(m.signedness)),
                .bits = m.bits,
            };
        }
    };
    comptime {
        validateSymbolInSync(Int, std.builtin.TypeInfo.Int, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Float = struct {
        bits: i32,

        pub fn init(comptime m: std.builtin.TypeInfo.Float) Float {
            return comptime .{
                .bits = m.bits,
            };
        }
    };
    comptime {
        validateSymbolInSync(Float, std.builtin.TypeInfo.Float, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Pointer = struct {
        size: Size,
        is_const: bool,
        is_volatile: bool,
        alignment: i32,
        child: *const TypeInfo,
        is_allowzero: bool,
        /// This field is an optional type.
        /// The type of the sentinel is the element type of the pointer, which is
        /// the value of the `child` field in this struct. However there is no way
        /// to refer to that type here, so we use `var`.
        // sentinel: anytype,
        /// This data structure is used by the Zig language code generation and
        /// therefore must be kept in sync with the compiler implementation.
        pub const Size = enum {
            One,
            Many,
            Slice,
            C,
        };

        pub fn init(comptime m: std.builtin.TypeInfo.Pointer) Pointer {
            return comptime .{
                .size = @intToEnum(TypeInfo.Pointer.Size, @enumToInt(m.size)),
                .is_const = m.is_const,
                .is_volatile = m.is_volatile,
                .alignment = m.alignment,
                .child = &TypeInfo.init(m.child),
                .is_allowzero = m.is_allowzero,
            };
        }

        pub fn deinit(self: *const Pointer, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };
    comptime {
        validateSymbolInSync(Pointer, std.builtin.TypeInfo.Pointer, .{
            .ignore_fields = .{"sentinel"},
        });
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Array = struct {
        len: i32,
        child: *const TypeInfo,
        /// This field is an optional type.
        /// The type of the sentinel is the element type of the array, which is
        /// the value of the `child` field in this struct. However there is no way
        /// to refer to that type here, so we use `var`.
        // sentinel: anytype,
        pub fn init(comptime m: std.builtin.TypeInfo.Array) Array {
            return comptime .{
                .len = m.len,
                .child = &TypeInfo.init(m.child),
            };
        }

        pub fn deinit(self: *const Array, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };
    comptime {
        validateSymbolInSync(Array, std.builtin.TypeInfo.Array, .{
            .ignore_fields = .{"sentinel"},
        });
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ContainerLayout = enum {
        Auto,
        Extern,
        Packed,
    };
    comptime {
        validateSymbolInSync(ContainerLayout, std.builtin.TypeInfo.ContainerLayout, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const StructField = struct {
        name: []const u8,
        field_type: *const TypeInfo,
        // default_value: anytype,
        is_comptime: bool,
        alignment: i32,

        pub fn init(comptime f: std.builtin.TypeInfo.StructField) StructField {
            return comptime .{
                .name = f.name,
                .field_type = &TypeInfo.init(f.field_type),
                .is_comptime = f.is_comptime,
                .alignment = f.alignment,
            };
        }

        pub fn deinit(self: *const StructField, allocator: *Allocator) void {
            allocator.free(self.name);

            self.field_type.deinit(allocator);

            allocator.destroy(self.field_type);
        }
    };
    comptime {
        validateSymbolInSync(StructField, std.builtin.TypeInfo.StructField, .{
            .ignore_fields = .{"default_value"},
        });
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Struct = struct {
        // Additional Field
        name: ?[]const u8,

        layout: ContainerLayout,
        fields: []const StructField,
        decls: []const Declaration,
        is_tuple: bool,

        pub fn init(comptime m: std.builtin.TypeInfo.Struct, comptime name: []const u8) Struct {
            return comptime .{
                .name = name,
                .layout = @intToEnum(TypeInfo.ContainerLayout, @enumToInt(m.layout)),
                .fields = fields: {
                    comptime var arr: [m.fields.len]StructField = undefined;

                    inline for (m.fields) |f, i| {
                        arr[i] = StructField.init(f);
                    }

                    break :fields &arr;
                },
                .decls = decls: {
                    comptime var arr: [m.decls.len]Declaration = undefined;

                    inline for (m.decls) |f, i| {
                        arr[i] = Declaration.init(f);
                    }

                    break :decls &arr;
                },
                .is_tuple = m.is_tuple,
            };
        }
        comptime {
            validateSymbolInSync(Struct, std.builtin.TypeInfo.Struct, .{});
        }

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
        child: *const TypeInfo,

        pub fn init(comptime m: std.builtin.TypeInfo.Optional) Optional {
            return comptime .{
                .child = &TypeInfo.init(m.child),
            };
        }

        pub fn deinit(self: *const Optional, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };
    comptime {
        validateSymbolInSync(Optional, std.builtin.TypeInfo.Optional, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ErrorUnion = struct {
        error_set: *const TypeInfo,
        payload: *const TypeInfo,

        pub fn init(comptime m: std.builtin.TypeInfo.ErrorUnion) ErrorUnion {
            return comptime .{
                .error_set = &TypeInfo.init(m.error_set),
                .payload = &TypeInfo.init(m.payload),
            };
        }

        pub fn deinit(self: *const ErrorUnion, allocator: *Allocator) void {
            self.error_set.deinit(allocator);
            allocator.destroy(self.error_set);

            self.payload.deinit(allocator);
            allocator.destroy(self.payload);
        }
    };
    comptime {
        validateSymbolInSync(ErrorUnion, std.builtin.TypeInfo.ErrorUnion, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Error = struct {
        name: []const u8,

        pub fn deinit(self: *const Error, allocator: *Allocator) void {
            allocator.free(self.name);
        }
    };
    comptime {
        validateSymbolInSync(Error, std.builtin.TypeInfo.Error, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ErrorSet = ?[]const Error;

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const EnumField = struct {
        name: []const u8,
        value: i32,

        pub fn init(comptime f: std.builtin.TypeInfo.EnumField) EnumField {
            return comptime .{
                .name = f.name,
                .value = f.value,
            };
        }

        pub fn deinit(self: *const EnumField, allocator: *Allocator) void {
            allocator.free(self.name);
        }
    };
    comptime {
        validateSymbolInSync(EnumField, std.builtin.TypeInfo.EnumField, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Enum = struct {
        // Additional Field
        name: ?[]const u8,

        layout: ContainerLayout,
        tag_type: *const TypeInfo,
        fields: []const EnumField,
        decls: []const Declaration,
        is_exhaustive: bool,

        pub fn init(comptime m: std.builtin.TypeInfo.Enum, comptime name: []const u8) Enum {
            return comptime .{
                .name = name,
                .layout = @intToEnum(TypeInfo.ContainerLayout, @enumToInt(m.layout)),
                .tag_type = &TypeInfo.init(m.tag_type),
                .fields = fields: {
                    comptime var arr: [m.fields.len]EnumField = undefined;

                    inline for (m.fields) |f, i| {
                        arr[i] = EnumField.init(f);
                    }

                    break :fields &arr;
                },
                .decls = decls: {
                    comptime var arr: [m.decls.len]Declaration = undefined;

                    inline for (m.decls) |f, i| {
                        arr[i] = Declaration.init(f);
                    }

                    break :decls &arr;
                },
                .is_exhaustive = m.is_exhaustive,
            };
        }

        pub fn deinit(self: *const Enum, allocator: *Allocator) void {
            for (self.fields) |f| f.deinit(allocator);
            for (self.decls) |f| f.deinit(allocator);

            allocator.free(self.fields);
            allocator.free(self.decls);

            self.tag_type.deinit(allocator);
            allocator.destroy(self.tag_type);
        }
    };
    comptime {
        validateSymbolInSync(Enum, std.builtin.TypeInfo.Enum, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const UnionField = struct {
        // Additional Field
        name: []const u8,

        field_type: *const TypeInfo,
        alignment: i32,

        pub fn init(comptime f: std.builtin.TypeInfo.UnionField) UnionField {
            return comptime .{
                .name = f.name,
                .field_type = &TypeInfo.init(f.field_type),
                .alignment = f.alignment,
            };
        }

        pub fn deinit(self: *const UnionField, allocator: *Allocator) void {
            allocator.free(self.name);

            self.field_type.deinit(allocator);

            allocator.destroy(self.field_type);

            if (self.enum_field) |ef| {
                ef.deinit(allocator);
            }
        }
    };
    comptime {
        validateSymbolInSync(UnionField, std.builtin.TypeInfo.UnionField, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Union = struct {
        // Additional Field
        name: ?[]const u8,

        layout: ContainerLayout,
        tag_type: ?*const TypeInfo,
        fields: []const UnionField,
        decls: []const Declaration,

        pub fn init(comptime m: std.builtin.TypeInfo.Union, comptime name: []const u8) Union {
            return comptime .{
                .name = name,
                .layout = @intToEnum(TypeInfo.ContainerLayout, @enumToInt(m.layout)),
                .tag_type = if (m.tag_type) |t| &TypeInfo.init(t) else null,
                .fields = fields: {
                    comptime var arr: [m.fields.len]UnionField = undefined;

                    inline for (m.fields) |f, i| {
                        arr[i] = UnionField.init(f);
                    }

                    break :fields &arr;
                },
                .decls = decls: {
                    comptime var arr: [m.decls.len]Declaration = undefined;

                    inline for (m.decls) |f, i| {
                        arr[i] = Declaration.init(f);
                    }

                    break :decls &arr;
                },
            };
        }

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
    comptime {
        validateSymbolInSync(Union, std.builtin.TypeInfo.Union, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const FnArg = struct {
        is_generic: bool,
        is_noalias: bool,
        arg_type: ?*const TypeInfo,

        pub fn init(comptime f: std.builtin.TypeInfo.FnArg) FnArg {
            return comptime .{
                .is_generic = f.is_generic,
                .is_noalias = f.is_noalias,
                .arg_type = if (f.arg_type) |t| &TypeInfo.init(t) else null,
            };
        }

        pub fn deinit(self: *const FnArg, allocator: *Allocator) void {
            if (self.arg_type) |t| {
                t.deinit(allocator);

                allocator.destroy(self.arg_type);
            }
        }
    };
    comptime {
        validateSymbolInSync(FnArg, std.builtin.TypeInfo.FnArg, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Fn = struct {
        calling_convention: CallingConvention,
        alignment: i32,
        is_generic: bool,
        is_var_args: bool,
        return_type: ?*const TypeInfo,
        args: []const FnArg,

        pub fn init(comptime m: std.builtin.TypeInfo.Fn) Fn {
            return comptime .{
                .calling_convention = @intToEnum(CallingConvention, @enumToInt(m.calling_convention)),
                .alignment = m.alignment,
                .is_generic = m.is_generic,
                .is_var_args = m.is_var_args,
                .return_type = if (m.return_type) |t| &TypeInfo.init(t) else null,
                .args = args: {
                    comptime var arr: [m.args.len]FnArg = undefined;

                    inline for (m.args) |f, i| {
                        arr[i] = FnArg.init(f);
                    }

                    break :args &arr;
                },
            };
        }

        pub fn deinit(self: *const Fn, allocator: *Allocator) void {
            if (self.return_type) |r| {
                r.deinit(allocator);

                allocator.destroy(r);
            }

            for (self.args) |arg| arg.deinit(allocator);

            allocator.free(self.args);
        }
    };
    comptime {
        validateSymbolInSync(Fn, std.builtin.TypeInfo.Fn, .{});
    }

    pub const Opaque = struct {
        decls: []const Declaration,

        pub fn init(comptime m: std.builtin.TypeInfo.Opaque) Opaque {
            return comptime .{
                .decls = decls: {
                    comptime var arr: [m.decls.len]Declaration = undefined;

                    inline for (m.decls) |f, i| {
                        arr[i] = Declaration.init(f);
                    }

                    break :decls &arr;
                },
            };
        }
    };
    comptime {
        validateSymbolInSync(Opaque, std.builtin.TypeInfo.Opaque, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Frame = struct {
        // function: anytype,
    };
    comptime {
        validateSymbolInSync(Frame, std.builtin.TypeInfo.Frame, .{
            .ignore_fields = .{"function"},
        });
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const AnyFrame = struct {
        child: ?*const TypeInfo,

        pub fn init(comptime m: std.builtin.TypeInfo.AnyFrame) AnyFrame {
            return comptime .{
                .child = if (m.child) |t| &TypeInfo.init(t) else null,
            };
        }

        pub fn deinit(self: *const AnyFrame, allocator: *Allocator) void {
            if (self.child) |child| {
                child.deinit(allocator);

                allocator.destroy(child);
            }
        }
    };
    comptime {
        validateSymbolInSync(AnyFrame, std.builtin.TypeInfo.AnyFrame, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Vector = struct {
        len: i32,
        child: *const TypeInfo,

        pub fn init(comptime m: std.builtin.TypeInfo.Vector) Vector {
            return comptime .{
                .len = m.len,
                .child = &TypeInfo.init(m.child),
            };
        }

        pub fn deinit(self: *const Vector, allocator: *Allocator) void {
            self.child.deinit(allocator);

            allocator.destroy(self.child);
        }
    };
    comptime {
        validateSymbolInSync(Vector, std.builtin.TypeInfo.Vector, .{});
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Declaration = struct {
        name: []const u8,
        is_pub: bool,
        data: Data,

        pub fn init(comptime f: std.builtin.TypeInfo.Declaration) Declaration {
            return comptime .{
                .name = f.name,
                .is_pub = f.is_pub,
                .data = Data.init(f.data),
            };
        }

        pub fn deinit(self: *const Declaration, allocator: *Allocator) void {
            self.data.deinit(allocator);

            allocator.free(self.name);
        }

        /// This data structure is used by the Zig language code generation and
        /// therefore must be kept in sync with the compiler implementation.
        pub const Data = union(enum) {
            Type: *const TypeInfo,
            Var: *const TypeInfo,
            Fn: FnDecl,

            pub fn init(comptime d: std.builtin.TypeInfo.Declaration.Data) Data {
                return comptime switch (d) {
                    .Type => |t| .{ .Type = &TypeInfo.init(t) },
                    .Var => |t| .{
                        .Var = &TypeInfo.init(t),
                    },
                    .Fn => |t| .{ .Fn = FnDecl.init(t) },
                };
            }

            /// This data structure is used by the Zig language code generation and
            /// therefore must be kept in sync with the compiler implementation.
            pub const FnDecl = struct {
                fn_type: *const TypeInfo,
                is_noinline: bool,
                is_var_args: bool,
                is_extern: bool,
                is_export: bool,
                lib_name: ?[]const u8,
                return_type: *const TypeInfo,
                arg_names: []const []const u8,

                pub fn init(comptime t: std.builtin.TypeInfo.Declaration.Data.FnDecl) FnDecl {
                    return comptime .{
                        .fn_type = &TypeInfo.init(t.fn_type),
                        .is_noinline = t.is_noinline,
                        .is_var_args = t.is_var_args,
                        .is_extern = t.is_extern,
                        .is_export = t.is_export,
                        .lib_name = t.lib_name,
                        .return_type = &TypeInfo.init(t.return_type),
                        .arg_names = t.arg_names,
                    };
                }

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
            comptime {
                validateSymbolInSync(FnDecl, std.builtin.TypeInfo.Declaration.Data.FnDecl, .{});
            }

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
        comptime {
            validateSymbolInSync(Data, std.builtin.TypeInfo.Declaration.Data, .{});
        }
    };
    comptime {
        validateSymbolInSync(Declaration, std.builtin.TypeInfo.Declaration, .{});
    }

    // Validate the whole TypeInfo sync
    comptime {
        @setEvalBranchQuota(2000);
        validateSymbolInSync(TypeInfo, std.builtin.TypeInfo, .{});
    }

    usingnamespace comptime blk: {
        var uniqueIdCounter: usize = 0;

        break :blk struct {
            pub fn uniqueId(comptime T: type) usize {
                comptime {
                    var id = uniqueIdCounter;

                    uniqueIdCounter += 1;

                    return id;
                }
            }
        };
    };

    pub fn alloc(comptime T: type) *TypeInfoSingleton {
        comptime var ptr = TypeInfoSingleton{};

        return &ptr;
    }

    pub fn init(comptime T: type) TypeInfo {
        return TypeInfo.initPtr(T).*;
    }

    pub fn initPtr(comptime T: type) *const TypeInfo {
        comptime var ptr = TypeInfo.alloc(T);

        if (ptr.resolved) {
            return &ptr.info;
        }

        ptr.resolved = true;

        comptime const info = @typeInfo(T);

        ptr.info = comptime switch (info) {
            .Type => .{ .Type = {} },
            .Void => .{ .Void = {} },
            .Bool => .{ .Bool = {} },
            .NoReturn => .{ .NoReturn = {} },
            .Int => |m| .{ .Int = Int.init(m) },
            .Float => |m| .{ .Float = Float.init(m) },
            .Pointer => |m| .{ .Pointer = Pointer.init(m) },
            .Array => |m| .{ .Array = Array.init(m) },
            .Struct => |m| .{ .Struct = Struct.init(m, @typeName(T)) },
            .ComptimeFloat => .{ .ComptimeFloat = {} },
            .ComptimeInt => .{ .ComptimeInt = {} },
            .Undefined => .{ .Undefined = {} },
            .Null => .{ .Null = {} },
            .Optional => |m| .{ .Optional = Optional.init(m) },
            .ErrorUnion => |m| .{ .ErrorUnion = ErrorUnion.init(m) }, // TODO
            .ErrorSet => |m| .{
                .ErrorSet = errorset: {
                    if (m == null) return null;

                    comptime var arr: [m.?.len]Error = undefined;

                    inline for (m.?) |f, i| {
                        arr[i] = .{
                            .name = f.name,
                        };
                    }

                    break :errorset &arr;
                },
            },
            .Enum => |m| .{ .Enum = Enum.init(m, @typeName(T)) },
            .Union => |m| .{ .Union = Union.init(m, @typeName(T)) },
            .Fn => |m| .{ .Fn = Fn.init(m) },
            .BoundFn => |m| .{ .BoundedFn = Fn.init(m) },
            .Opaque => .{ .Opaque = {} },
            .Frame => .{ .Frame = {} }, // TODO
            .AnyFrame => |m| .{ .AnyFrame = AnyFrame.init(m) },
            .Vector => |m| .{ .Vector = Vector.init(m) },
            .EnumLiteral => .{ .EnumLiteral = {} },
        };

        return &ptr.info;
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
};

pub const CallingConvention = enum {
    Unspecified,
    C,
    Naked,
    Async,
    Inline,
    Interrupt,
    Signal,
    Stdcall,
    Fastcall,
    Vectorcall,
    Thiscall,
    APCS,
    AAPCS,
    AAPCSVFP,
    SysV,
};

pub const Signedness = enum {
    signed,
    unsigned,
};

pub fn hasField(comptime T: type, comptime field_name: []const u8) bool {
    inline for (comptime std.meta.fields(T)) |field| {
        if (std.mem.eql(u8, field.name, field_name) == true) {
            return true;
        }
    }

    return false;
}

/// Function to be run in compile time, responsible for verifying if the 
/// structures/enums/unions defined in this file to represent the TypeInfo at
/// runtime in sync with the current Zig version's comptime structures/enums/unions
pub fn validateSymbolInSync(comptime runtime_type: type, comptime builtin_type: type, comptime options: anytype) void {
    const builtin_type_info = @typeInfo(builtin_type);
    const runtime_type_info = @typeInfo(runtime_type);

    // Make sure that the runtime type is a struct as well
    if (std.mem.eql(u8, @tagName(builtin_type_info), @tagName(runtime_type_info)) == false) {
        @compileError(
            "Type of " ++ @typeName(builtin_type) ++
                " is " ++ @tagName(builtin_type_info) ++
                " but runtime type is " ++ @tagName(runtime_type_info),
        );
    }

    switch (builtin_type_info) {
        .Struct, .Enum, .Union => {
            // Compare the fields
            inline for (std.meta.fields(builtin_type)) |builtin_field| {
                var missing_field: bool = false;

                if (hasField(runtime_type, builtin_field.name) == false) {
                    missing_field = true;

                    if (@hasField(@TypeOf(options), "ignore_fields")) {
                        inline for (options.ignore_fields) |ignore_field| {
                            if (std.mem.eql(u8, ignore_field, builtin_field.name) == true) {
                                missing_field = false;
                                break;
                            }
                        }
                    }

                    if (missing_field == true) {
                        @compileError(
                            "Field " ++ builtin_field.name ++
                                " is missing in type " ++ @typeName(runtime_type),
                        );
                    }
                }
            }
        },
        else => @compileError(
            "Cannot validate symbol in sync " ++ @typeName(builtin_type) ++
                " because type " ++ @tagName(builtin_type_info) ++
                " is not supported",
        ),
    }
}

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const talloc = std.testing.allocator;

// TODO .Type

test "Runtime TypeInfo.Void" {
    var info_void = TypeInfo.init(void);
    try expect(info_void == .Void);
}

test "Runtime TypeInfo.Bool" {
    var info_bool = TypeInfo.init(bool);
    try expect(info_bool == .Bool);
}

// TODO .NoReturn

test "Runtime TypeInfo.Int" {
    var info_i32 = TypeInfo.init(i32);
    try expect(info_i32 == .Int);
    try expectEqual(@as(i32, 32), info_i32.Int.bits);
    try expectEqual(true, info_i32.Int.signedness == .signed);
}

test "Runtime TypeInfo.Float" {
    var info_f64 = TypeInfo.init(f64);
    try expect(info_f64 == .Float);
    try expectEqual(@as(i32, 64), info_f64.Float.bits);
}

test "Runtime TypeInfo.Pointer" {
    var info_pointer_f64 = TypeInfo.init(*f64);
    try expect(info_pointer_f64 == .Pointer);
    try expectEqual(TypeInfo.Pointer.Size.One, info_pointer_f64.Pointer.size);
    try expectEqual(false, info_pointer_f64.Pointer.is_const);
    try expectEqual(false, info_pointer_f64.Pointer.is_volatile);
    try expectEqual(@as(i32, 8), info_pointer_f64.Pointer.alignment);
    try expect(info_pointer_f64.Pointer.child.* == .Float);
    try expectEqual(false, info_pointer_f64.Pointer.is_allowzero);

    var info_pointer_many = TypeInfo.init([*]f64);
    try expect(info_pointer_many == .Pointer);
    try expectEqual(TypeInfo.Pointer.Size.Many, info_pointer_many.Pointer.size);
    try expectEqual(false, info_pointer_many.Pointer.is_const);
    try expectEqual(false, info_pointer_many.Pointer.is_volatile);
    try expectEqual(@as(i32, 8), info_pointer_many.Pointer.alignment);
    try expect(info_pointer_many.Pointer.child.* == .Float);
    try expectEqual(false, info_pointer_many.Pointer.is_allowzero);
}

test "Runtime TypeInfo.Array" {
    var info_array = TypeInfo.init([2]i32);
    try expect(info_array == .Array);
    try expectEqual(@as(i32, 2), info_array.Array.len);
    try expect(info_array.Array.child.* == .Int);
}

test "Runtime TypeInfo.Struct" {
    const FooStruct = struct {
        int: i32,

        pub fn bar() void {}
    };

    var info_struct = TypeInfo.init(FooStruct);
    try expect(info_struct == .Struct);
    try expect(info_struct.Struct.layout == .Auto);
    try expectEqual(@as(usize, 1), info_struct.Struct.fields.len);
    try expectEqualStrings("int", info_struct.Struct.fields[0].name);
    try expect(info_struct.Struct.fields[0].field_type.* == .Int);
}

test "Runtime TypeInfo.ComptimeFloat" {
    var info_comptime_float = TypeInfo.init(comptime_float);
    try expect(info_comptime_float == .ComptimeFloat);
}

test "Runtime TypeInfo.ComptimeInt" {
    var info_comptime_int = TypeInfo.init(comptime_int);
    try expect(info_comptime_int == .ComptimeInt);
}

// // TODO .Undefined
// // TODO .Null

test "Runtime TypeInfo.Optional" {
    var info_optional = TypeInfo.init(?i32);
    try expect(info_optional == .Optional);
    try expect(info_optional.Optional.child.* == .Int);
}

// // TODO .ErrorUnion
// // TODO .ErrorSet

test "Runtime TypeInfo.Enum" {
    const FooEnum = enum { Foo, Bar };

    var info_enum = TypeInfo.init(FooEnum);
    try expect(info_enum == .Enum);
}

test "Runtime TypeInfo.Union" {
    const FooUnion = union { Foo: void, Bar: i32 };

    var info_union = TypeInfo.init(FooUnion);
    try expect(info_union == .Union);
}

test "Runtime TypeInfo.Fn" {
    // .Fn
    var info_fn = TypeInfo.init(fn () void);
    try expect(info_fn == .Fn);
}

test "Runtime TypeInfo.Struct declarations" {
    // .Fn
    var info_fn = TypeInfo.init(struct {
        const WackType = packed struct { mr_field: *LameType, ola: u8 };

        const LameType = struct {
            blah: **WackType,
        };

        pub fn thing(one: usize, two: *LameType, three: [*]u16) bool {
            return one == 1;
        }
    });
    try expect(info_fn == .Struct);
}

// TODO .BoundFn
// TODO .Opaque
// TODO .Frame
// TODO .AnyFrame
// TODO .Vector
// TODO .EnumLiteral
