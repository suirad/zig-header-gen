const rt = @import("../runtime.zig");
const header_gen = @import("header_gen");

export fn thing(one: usize, two: *LameType, three: [*]u16) bool {
    return one == 1;
}

export fn break_point(v: [*]u8) callconv(.Naked) void {
    @breakpoint();
}

const LameType = extern struct {
    blah: WackType,
};

const WackType = packed struct {
    mr_field: u8,
};

const WhatsAUnion = extern union {
    a: *LameType,
    b: u64,
};

const ThisWillBeVoid = struct {
    a: u64,
};

const LookMaAnEnum = extern enum {
    one = 1,
    three = 3,
    four,
    five = 5,
};

pub fn main () void {
    const gen = header_gen.HeaderGen(@This(), "lib").init();

    gen.exec(header_gen.C_Generator);
    gen.exec(header_gen.Ordered_Generator(header_gen.Python_Generator));
}