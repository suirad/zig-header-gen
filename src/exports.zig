export fn thing(one: usize, two: *LameType, three: [*]u16) bool {
    return one == 1;
}

export fn break_point(v: [*]ThisWillBeVoid) callconv(.Naked) void {
    @breakpoint();
}

const LameType = extern struct {
    blah: u64,
};

const WackType = packed struct {
    mr_field: u8,
};

const WhatsAUnion = extern union {
    a: LameType,
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
