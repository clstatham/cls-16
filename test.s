%start
    mov sp $0xf000
    mov fp sp
    sub fp fp $6
    mov sp fp
%startouter
    mov r1 $0x1a4
    stl fp r1 $0x2
    sth fp r1 $0x3
    ldl r2 fp $0x2
    ldh r2 fp $0x3
    mov r1 $0x2
    stl fp r1 $0x4
    sth fp r1 $0x5
    ldl r1 fp $0x4
    ldh r1 fp $0x5
    mul r2 r2 r1
    mov r1 $0x4
    stl fp r1 $0x4
    sth fp r1 $0x5
    ldl r1 fp $0x4
    ldh r1 fp $0x5
    div r2 r2 r1
    printi r2
%startepilogue
    add fp fp $6
    mov sp fp
    halt 