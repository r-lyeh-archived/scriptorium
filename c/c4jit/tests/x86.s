ENT: # 2
    push %ebp  
    mov %esp, %ebp
    subl $8, %esp

BZ: #0xdeadbeef
    test %eax, %eax 
    jz .exit
    # BNZ 0xdeadbeef
    test %eax, %eax 
    jnz .exit

    jnz 0x0
    jz .exit
    jz 0x0

    leal (-4 * 1)(%ebp), %eax    # LEA -1
    movl $42, %eax          # IMM 42
    jmp 0x210         # JMP 0xdeadbeef
    call *0xdeadbeef        # JSR 0xdeadbeef
    addl $(4 * 5), %esp     # ADJ 5
    movl (%eax), %eax       # LI
    movzb (%eax), %eax      # LC
    
SI:
    pop %ecx
    movl %eax, (%ecx)

SC:
    pop %ecx
    movb %al, (%ecx)

    push %eax

signextend:
    cbw
    cwde

GT1:
    mov %eax, %edx
    #push %eax pop %edx
    pop %ecx
    xor %eax, %eax
    cmp %edx, %ecx
    setg %al

GT2:
    pop  %ecx
    cmp %eax, %ecx
    setg %al
    cbw
    cwde

EQ1:
    pop %ecx
    cmp %eax, %ecx
    sete %al
    cbw
    cwde

cmps:
    setl %al
    setge %al
    setle %al
    setne %al

MOD:
    pop %ecx
    xchg %ecx, %eax
    xor %edx, %edx
    idiv %ecx
    xchg %edx, %eax

.exit:
    mov %ebp, %esp
    pop %ebp
    ret

    pop %ecx
    orl %ecx, %eax
    xchg %ecx, %eax
    xorl %ecx, %eax
    andl %ecx, %eax
    test %ecx, %eax     # EQ
    shll %cl, %eax
    shrl %cl, %eax
    add %ecx, %eax
    subl %ecx, %eax
    imul %ecx, %eax
    idiv %ecx, %eax
    
    
