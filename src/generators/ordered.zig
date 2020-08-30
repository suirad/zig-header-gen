const std = @import("std");
const Allocator = std.mem.Allocator;
const FnMeta = std.builtin.TypeInfo.Fn;
const FnDecl = std.builtin.TypeInfo.Declaration.Data.FnDecl;
const StructMeta = std.builtin.TypeInfo.Struct;
const EnumMeta = std.builtin.TypeInfo.Enum;
const UnionMeta = std.builtin.TypeInfo.Union;
const Dir = std.fs.Dir;
const DepsGraph = @import("../deps_graph.zig").DepsGraph;
const rt = @import("../runtime.zig");

const SymbolDeclaration = union(enum) {
    Struct: rt.TypeInfo.Struct,
    Union: rt.TypeInfo.Union,
    Enum: rt.TypeInfo.Enum,

    pub fn deinit(self: SymbolDeclaration, allocator: *Allocator) void {
        switch (self) {
            .Struct => |s| s.deinit(allocator),
            .Union => |u| u.deinit(allocator),
            .Enum => |e| e.deinit(allocator),
        }
    }
};

fn isSymbolDependency(comptime symbol_type: type) bool {
    const info = @typeInfo(symbol_type);

    return switch (info) {
        .Struct, .Union, .Enum => true,
        else => false,
    };
}

fn getTypeName (comptime T: type) []const u8 {
    comptime const type_info = @typeInfo(T);
                    
    return switch (type_info) {
        .Pointer => |p| getTypeName(p.child),
        .Array => |p| getTypeName(p.child),
        else => @typeName(T),
    };
}

pub fn Ordered_Generator(comptime Generator: type) type {
    return struct {
        inner_gen: Generator,
        allocator: *Allocator,
        symbols: DepsGraph(SymbolDeclaration),

        const Self = @This();

        pub fn init(comptime src_file: []const u8, dst_dir: *Dir) Self {
            var allocator = std.heap.page_allocator;

            return Self{
                .inner_gen = Generator.init(src_file, dst_dir),
                .allocator = allocator,
                .symbols = DepsGraph(SymbolDeclaration).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.flush();

            self.inner_gen.deinit();

            // TODO Enable deinit hashmap. Right now it segfaults, reason unknown
            // var iter = self.symbols.symbols.iterator();
            // while (iter.next()) |kv| {
            //     kv.value.payload.deinit(self.allocator);
            // }

            // self.symbols.deinit();
        }

        fn flush(self: *Self) void {
            while (self.symbols.readEmitted()) |emitted| {
                // Handle emitted.partial
                switch (emitted.symbol.payload) {
                    .Struct => |meta| self.inner_gen.gen_struct(emitted.symbol.name, meta),
                    .Union => |meta| self.inner_gen.gen_union(emitted.symbol.name, meta),
                    .Enum => |meta| self.inner_gen.gen_enum(emitted.symbol.name, meta),
                }
            }
        }

        pub fn gen_func(self: *Self, comptime name: []const u8, comptime meta: FnMeta) void {
            self.flush();

            // var rt_func = rt.TypeInfo.copy(rt.TypeInfo.Declaration.Data.FnDecl, self.allocator, func);
            // defer rt_func.deinit(self.allocator);

            var rt_meta = rt.TypeInfo.copy2(rt.TypeInfo.Fn, self.allocator, meta, 2);
            defer rt_meta.deinit(self.allocator);

            self.inner_gen.gen_func(name, rt_meta);
        }

        pub fn gen_struct(self: *Self, comptime name: []const u8, comptime meta: StructMeta) void {
            var decl: SymbolDeclaration = SymbolDeclaration{ .Struct = rt.TypeInfo.copy(rt.TypeInfo.Struct, self.allocator, meta) };

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
            var decl: SymbolDeclaration = SymbolDeclaration{ .Enum = rt.TypeInfo.copy(rt.TypeInfo.Enum, self.allocator, meta) };

            self.symbols.beginSymbol(name, decl) catch |err| @panic(@errorName(err));
            // Enums have no type dependencies I think, yay
            self.symbols.endSymbol() catch |err| @panic(@errorName(err));

            self.flush();
        }

        pub fn gen_union(self: *Self, comptime name: []const u8, comptime meta: UnionMeta) void {
            var decl: SymbolDeclaration = SymbolDeclaration{ .Union = rt.TypeInfo.copy(rt.TypeInfo.Union, self.allocator, meta) };

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
