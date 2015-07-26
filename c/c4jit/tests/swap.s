argc1:
    pop %ecx
    mov %esp, %esi
    andl $0xfffffff0, %esi
    mov %ecx, (%esi)
    xchg %esi, %esp

    call 0xdeadbeef
    xchg %esi, %esp

argc2:
    pop %ecx
    pop %edx
    movl %esp, %esi
    andl $0xfffffff0, %esi
    mov %edx, (%esi)
    mov %ecx, 4(%esi)
    xchg %esi, %esp

    call 0xdeadbeef

    xchg %esi, %esp

argN:   # n = 4
    mov $0x10, %ecx
    mov %esp, %esi
    subl %ecx, %esi
    shr $2, %ecx
    andl $0xfffffff0, %esi
    
1:
    pop %edx
    mov %edx, -4(%esi, %ecx, 4) # -4 to compensate %ecx off-by-one
    loop 1b
    
    xchg %esi, %esp

    call 0xdeadbeef
    
    xchg %esi, %esp
