%start
    mov sp $0xf000
    mov fp sp
%startprologue
    sub fp fp $0x6
    add sp sp $0x2
    mov sp fp
%startouter
    add r2 fp $0x4
    stl fp r2 $0x6
    sth fp r2 $0x7
    mov r4 $0x0
    mul r4 r4 $0x2
    ldl r5 fp $0x6
    ldh r5 fp $0x7
    add r5 r5 r4
    mov r2 $0x2a
    stl r5 r2 $0x0
    sth r5 r2 $0x1
    ldl r2 fp $0x6
    ldh r2 fp $0x7
    mov r3 $0x1
    mul r3 r3 $0x2
    add r2 r2 r3
    ldl r4 r2 $0x0
    ldh r4 r2 $0x1
    mov r2 r4
    printi r2
    ldl r2 fp $0x6
    ldh r2 fp $0x7
    mov r3 $0x0
    mul r3 r3 $0x2
    add r2 r2 r3
    ldl r4 r2 $0x0
    ldh r4 r2 $0x1
    mov r2 r4
    printi r2
%startepilogue
    halt 