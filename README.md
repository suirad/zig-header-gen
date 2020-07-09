# HeaderGen

`HeaderGen` automatically generates headers/bindings for other languages given
 Zig code with exported functions/types

Here are the following supported language binding outputs:

- [x] C Bindings
- [ ] Python Bindings
- [ ] Rust Bindings
- [ ] Go Bindings
- [ ] Nim Bindings

Here are the following supported Zig language features:

- [x] Extern Functions
- [x] Extern Structs
- [x] Extern Unions
- [x] Extern Enums

## Getting started

Given the following Zig code as the file `src/fancylib.zig` and using the C generator:

```zig
const std = @import("std");

export fn print_msg(msg_ptr: ?[*]const u8, len: usize) bool {
    if (msg_ptr) |raw_msg| {
        const msg = raw_msg[0 .. len - 1];
        std.debug.warn("Msg is: {}", .{msg});
        return true;
    }
    return false;
}

const Internal = struct {
    a: u64,
};

const External = extern struct {
    b: u64,
    c: [100]u8,
};

export fn use_internal_and_external(i: ?*Internal, e: ?*External) void {
    // do things...
}
```

You will receive the following C header in `headers/fancylib.h`:

```c
#ifndef _fancylib_H

#define _fancylib_H
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct External {
   uint64_t b;
   uint8_t c[100];
} External_t;

bool print_msg(uint8_t* arg0, size_t arg1);

void use_internal_and_external(void* arg0, External_t* arg1);


#endif
```

## Usage Notes

- Copy from this repo `header_gen.zig` and the folder `generators`;
 drop them next to your `build.zig`
- See the `build.zig` in this repo for an example of integration

