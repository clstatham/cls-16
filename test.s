%start
    mov fp $0xf000
    mov sp fp
    sub fp fp $0x4
    mov sp fp
%startbody
    mov r5 $0x2a
    add r5 r5 $0x3
    mul r2 r5 $0x2
    stl fp r2 $0x4
    sth fp r2 $0x5
    ldl r2 fp $0x4
    ldh r2 fp $0x5
    printi r2
    ldl r2 fp $0x4
    ldh r2 fp $0x5
    div r2 r2 $0x2
    stl fp r2 $0x4
    sth fp r2 $0x5
    ldl r2 fp $0x4
    ldh r2 fp $0x5
    printi r2
    add fp fp $0x4
    mov sp fp
    halt 