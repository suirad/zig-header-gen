const std = @import("std");
const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;
const FnMeta = std.builtin.Type.Fn;
const FnDecl = std.builtin.Type.Declaration.Data.FnDecl;
const StructMeta = std.builtin.Type.Struct;
const EnumMeta = std.builtin.Type.Enum;
const UnionMeta = std.builtin.Type.Union;
const Dir = std.fs.Dir;
const DepsGraph = @import("../deps_graph.zig").DepsGraph;
const rt = @import("../runtime.zig");

const SymbolDeclaration = union(enum) {
    Struct: rt.TypeInfo.Struct,
    Union: rt.TypeInfo.Union,
    Enum: rt.TypeInfo.Enum,
    Fn: rt.TypeInfo.Fn,

    pub fn deinit(self: SymbolDeclaration, allocator: Allocator) void {
        switch (self) {
            .Struct => |s| s.deinit(allocator),
            .Union => |u| u.deinit(allocator),
            .Enum => |e| e.deinit(allocator),
            .Fn => |f| f.deinit(allocator),
        }
    }
};

fn isSymbolDependency(comptime symbol_type: type) bool {
    const info = @typeInfo(symbol_type);

    return switch (info) {
        .Struct, .Union, .Enum => true,
        .Pointer => |p| isSymbolDependency(p.child),
        .Array => |a| isSymbolDependency(a.child),
        else => false,
    };
}

fn getTypeName(comptime T: type) []const u8 {
    const type_info = @typeInfo(T);

    return switch (type_info) {
        .Pointer => |p| getTypeName(p.child),
        .Array => |p| getTypeName(p.child),
        else => @typeName(T),
    };
}

pub const SymbolPhase = enum {
    Signature, Body, Full
};

pub fn Ordered_Generator(comptime Generator: type) type {
    return struct {
        inner_gen: Generator,
        allocator: Allocator,
        symbols: DepsGraph(SymbolDeclaration),
        emitted_phase: StringHashMap(SymbolPhase),

        const Self = @This();

        pub fn init(comptime src_file: []const u8, dst_dir: *Dir) Self {
            var allocator = std.heap.page_allocator;

            return Self{
                .inner_gen = Generator.init(src_file, dst_dir),
                .allocator = allocator,
                .symbols = DepsGraph(SymbolDeclaration).init(allocator),
                .emitted_phase = StringHashMap(SymbolPhase).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.flush();

            self.symbols.deinit();
            
            self.inner_gen.deinit();

            self.emitted_phase.deinit();
        }

        fn getNextPhaseFor(self: *Self, symbol_name: []const u8, partial: bool) !?SymbolPhase {
            var result = try self.emitted_phase.getOrPut(symbol_name);

            if (!result.found_existing) {
                result.entry.value = if (partial) .Signature else .Full;

                return result.entry.value;
            } else if (result.entry.value == .Signature) {
                if (partial) {
                    return null;
                } else {
                    result.entry.value = .Full;

                    return .Body;
                }
            }

            return null;
        }

        fn flush(self: *Self) void {
            while (self.symbols.readEmitted()) |emitted| {
                const partial = if (emitted.symbol.payload == .Fn) false else emitted.partial;
                _ = partial;

                var phase = self.getNextPhaseFor(emitted.symbol.name, emitted.partial) catch unreachable orelse continue;

                switch (emitted.symbol.payload) {
                    .Struct => |meta| self.inner_gen.gen_struct(emitted.symbol.name, meta, phase),
                    .Union => |meta| self.inner_gen.gen_union(emitted.symbol.name, meta, phase),
                    .Enum => |meta| self.inner_gen.gen_enum(emitted.symbol.name, meta, phase),
                    .Fn => |meta| self.inner_gen.gen_func(emitted.symbol.name, meta),
                }
            }
        }

        pub fn gen_func(self: *Self, comptime name: []const u8, comptime meta: FnMeta) void {
            const decl: SymbolDeclaration = SymbolDeclaration{
                .Fn = rt.TypeInfo.Fn.init(meta),
            };

            self.symbols.beginSymbol(name, decl) catch |err| @panic(@errorName(err));
            inline for (meta.args) |f| {
                if (f.arg_type != null and comptime isSymbolDependency(f.arg_type.?)) {
                    self.symbols.addDependency(getTypeName(f.arg_type.?)) catch |err| @panic(@errorName(err));
                }
            }
            if (meta.return_type) |t| {
                if (comptime isSymbolDependency(t)) {
                    self.symbols.addDependency(getTypeName(t)) catch |err| @panic(@errorName(err));
                }
            }
            self.symbols.endSymbol() catch |err| @panic(@errorName(err));

            self.flush();
        }

        pub fn gen_struct(self: *Self, comptime name: []const u8, comptime meta: StructMeta) void {
            const decl: SymbolDeclaration = SymbolDeclaration{
                .Struct = rt.TypeInfo.Struct.init(meta, name),
            };

            self.symbols.beginSymbol(name, decl) catch |err| @panic(@errorName(err));
            inline for (meta.fields) |f| {
                if (comptime isSymbolDependency(f.field_type)) {
                    self.symbols.addDependency(getTypeName(f.field_type)) catch |err| @panic(@errorName(err));
                }
            }
            self.symbols.endSymbol() catch |err| @panic(@errorName(err));

            self.flush();
        }

        pub fn gen_enum(self: *Self, comptime name: []const u8, comptime meta: EnumMeta) void {
            const decl: SymbolDeclaration = SymbolDeclaration{
                .Enum = rt.TypeInfo.Enum.init(meta, name),
            };

            self.symbols.beginSymbol(name, decl) catch |err| @panic(@errorName(err));
            // Enums have no type dependencies I think, yay
            self.symbols.endSymbol() catch |err| @panic(@errorName(err));

            self.flush();
        }

        pub fn gen_union(self: *Self, comptime name: []const u8, comptime meta: UnionMeta) void {
            const decl: SymbolDeclaration = SymbolDeclaration{
                .Union = rt.TypeInfo.Union.init(meta, name),
            };

            self.symbols.beginSymbol(name, decl) catch |err| @panic(@errorName(err));
            inline for (meta.fields) |f| {
                if (comptime isSymbolDependency(f.field_type)) {
                    self.symbols.addDependency(getTypeName(f.field_type)) catch |err| @panic(@errorName(err));
                }
            }
            self.symbols.endSymbol() catch |err| @panic(@errorName(err));

            self.flush();
        }
    };
}
