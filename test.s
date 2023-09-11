%start
    mov sp $0xf000
    mov fp sp
%startprologue
    sub fp fp $0x6
    add sp sp $0x2
    mov sp fp
%startouter
    mov r2 $0x2a
    stl fp r2 $0x2
    sth fp r2 $0x3
    mov r2 $0x1000
    stl fp r2 $0x4
    sth fp r2 $0x5
    ldl r2 fp $0x4
    ldh r2 fp $0x5
    ldl r3 fp $0x2
    ldh r3 fp $0x3
    stl r2 r3 $0x0
    sth r2 r3 $0x1
    ldl r2 fp $0x4
    ldh r2 fp $0x5
    stl fp r2 $0x6
    sth fp r2 $0x7
    ldl r3 fp $0x6
    ldh r3 fp $0x7
    ldl r2 r3 $0x0
    ldh r2 r3 $0x1
    mov r3 r2
    stl fp r3 $0x6
    sth fp r3 $0x7
    ldl r2 fp $0x6
    ldh r2 fp $0x7
    printi r2
%startepilogue
    halt 