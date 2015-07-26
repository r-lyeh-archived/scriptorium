What is it?
=============

`c4x86.c` is a primitive x86 Just-In-Time compiler for awesome c4 virtual machine. It took about 86 lines of C code.

It is known to work on Linux (and OS X?).

How JIT works
=============

JIT compilation is based on the fact that mapping c4 opcodes into x86 instructions is quite straightforward:

| c4 opcode    | x86 instructions                           | comments
|--------------|--------------------------------------------|-----------------------
| `IMM` *val*  |`movl $val, %eax`                           |
| `PSH`        |`push %eax`                                 |
| `LEV`        |`movl %ebp, %esp; pop %esp; ret`            |
| `ADJ` *val*  |`subl $(4 * val), %esp)`                    |
| `LI`         |`movl (%eax), %eax`                         |
| `LC`         |`movzbl (%eax), %eax`                       |
| `SI`         |`pop %ecx; movl %eax, (%ecx)`               | `%ecx` is used as a temporary register
| `SC`         |`pop %ecx; movb %al, (%ecx)`                |
| `OR`         |`pop %ecx; orl %ecx, %eax`                  |
| `XOR`        |`pop %ecx; xorl %ecx, %eax`                 |
| `AND`        |`pop %ecx; andl %ecx, %eax`                 |
| `NE`         |see `Comparisons`                           | using `setne %al` opcode
| `EQ`         |see `Comparisons`                           | using `sete %al` opcode
| `GE`         |see `Comparisons`                           | using `setge %al` opcode
| `LE`         |see `Comparisons`                           | using `setle %al` opcode
| `GT`         |see `Comparisons`                           | using `setg %al` opcode
| `LT`         |see `Comparisons`                           | using `setl %al` opcode
| `SHL`        |`pop %ecx; xchg %eax, %ecx; shl %cl, %eax`  | `xchg` adjusts the operands order
| `SHR`        |`pop %ecx; xchg %eax, %ecx; shr %cl, %eax`  |
| `ADD`        |`pop %ecx; addl %ecx, %eax`                 |
| `SUB`        |`pop %ecx; xchg %eax, %ecx; subl %ecx, %eax`|
| `MUL`        |`pop %ecx; imul %ecx, %eax`                 |
| `DIV`        |`pop %ecx; xchg %eax, %ecx; idiv %ecx, %eax`|
| `MOD`        |`pop %ecx; xchg %ecx, %eax; xor %edx, %edx; idiv %ecx; xchg %edx, %eax` | `%edx` holds remainder after `idiv`
| `JMP`        |`jmp <off32>`                               |
| `JSR`        |`call <off32>`                              |
| `BZ`         |`jz <off32>`                                |
| `BNZ`        |`jnz <off32>`                               |
| `OPEN`; `ADJ <n>`  | see `Native calls` section           | 
| `READ` ; `ADJ <n>` | see `Native calls` section           |
| `CLOS` ; `ADJ <n>` | see `Native calls` section           |
| `PRTF` ; `ADJ <n>` | see `Native calls` section           |
| `MALC` ; `ADJ <n>` | see `Native calls` section           |
| `MSET` ; `ADJ <n>` | see `Native calls` section           |
| `MCMP` ; `ADJ <n>` | see `Native calls` section           |
| `EXIT` ; `ADJ <n>` | see `Native calls` section           |

Some executable and writable memory is allocated with `mmap()`, its address in `jitmem` pointer.

First pass of the JIT compiler translates c4 opcodes into instructions directly, leaving stubs for relative offsets in `JSR`, `JMP` (4 byte offset), `BZ`, `BNZ` (1 byte offset) to be filled during the second pass.

Comparisons
===========

Comparison uses `set<cc>` x86 operations after `cmp %ecx, %eax` where `%ecx` is popped from the stack with ensuing sign-extension of `%al` to `%eax`.

So, the full comparison code for, e.g. `EQ` is:

    pop %ecx
    cmp %ecx, %eax
    sete %al            # set %al to 0/1 depending on equality
    cbw                 # %al to %ax sign extension
    cwde                # %ax to %eax sign extension


Filling up relative offsets
===========================

For addresses of compiled "labels" to be known, the first pass stores addresses of compiled x86 code for each c4 opcode in the opcode cell (of `text[]` array) itself:

    before:    | <opcode> | 0x00    | 0x00   | 0x00    |
    after:     | <opcode> | <least 3 bytes of x86 ptr> |

The most significant byte is restored from `jitmem` value (it is assumed that the native code does not take more than 24 megabytes, so the most significant byte is the same for all pointers).

`JMP`/`JSR`/`BNZ`/`BZ` arguments are not modified.

The second pass reads c4 pointers of `JMP`/`JSR`/`BNZ`/`BZ` and extracts native codes addresses. Then relative offsets are calculated to fill offset gaps in native code.

Native calls
============
Native calls are tricky for x86: 

1. order of arguments is reversed; 
2. OS X ABI requires stack to be aligned at 16 bytes before calls.

Fortunately, arguments count is known for each call, it can be retrieved from `ADJ` right after c4 opcode of the call.

Both these complications require a quite hacky solution: the arguments evaluation code is left as is, but some additional stack memory is allocated before native calls (aligned to 16 bytes), arguments are copied there in reverse order and "old" stack pointer (without arguments) is saved in `%esi` register. On return from the native routine `%esp` is restored from `%esi`.

        movl $(4 * n), %ecx       # store 4 * #args in %ecx
        mov %esp, %esi            # %esi temporarily holds the native call stack pointer (to become %esp later)
        sub %ecx, %esi            # %esp - %esi must be large enough to contain all arguments
        andl $0xfffffff0, %esi    # the future %esp must be aligned at 16 bytes
        shr $2, %ecx              # let %ecx be just #args now

    1:                            # this is a loop copying %ecx arguments:
        pop %edx                  # movl (%esp), %edx; addl $4, %esp; - %esp grows until all arguments are below
        mov %edx, -4(%esi,%ecx,4) # %edx value is now stored at %esi+4*%ecx; -4 to compensate %ecx off-by-one
        loop 1b                   # dec %ecx; while %ecx is not 0, return to 1:

        xchg %esi, %esp           # now %esi contains the 'old' stack pointer, %esp is adjusted properly
        call printf               # %esi must be preserved according to cdecl calling convention

        xchg %esi, %esp           # ADJust: restore the stack state before the call

This is a lengthy and costly solution, but it keeps code size small.


Issues
======

1. this is x86 only; requires Unix-like calls; not self-hosted;
2. uses registers `%eax`, `%ecx`, `%ebp`, `%esp` only with quite redundant memory loads/stores; no register allocation;
3. it is limited to `open`/`read`/`close`/`printf`/`malloc`/`memset`/`memcmp`/`exit` calls.


(c) Dmytro Sirenko, 2014
