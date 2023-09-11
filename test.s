%start
    mov sp $0xf000
    mov fp sp
%startprologue
    sub fp fp $0x8
    add sp sp $0x2
    mov sp fp
%startouter
    mov r2 $0x2a
    stl fp r2 $0x2
    sth fp r2 $0x3
    ldl r2 fp $0x4
    ldh r2 fp $0x5
    mov r2 fp
    add r2 r2 $0x2
    stl fp r2 $0x4
    sth fp r2 $0x5
    ldl r3 fp $0x4
    ldh r3 fp $0x5
    stl fp r3 $0x6
    sth fp r3 $0x7
    ldl r3 fp $0x6
    ldh r3 fp $0x7
    printi r3
    ldl r3 fp $0x4
    ldh r3 fp $0x5
    stl fp r3 $0x6
    sth fp r3 $0x7
    ldl r4 fp $0x6
    ldh r4 fp $0x7
    ldl r3 r4 $0x0
    ldh r3 r4 $0x1
    mov r4 r3
    stl fp r4 $0x6
    sth fp r4 $0x7
    ldl r3 fp $0x6
    ldh r3 fp $0x7
    printi r3
%startepilogue
    halt 