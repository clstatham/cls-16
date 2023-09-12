%start
    mov fp $0xf000
    mov sp fp
    sub fp fp $0x1c
    mov sp fp
%startbody
    mov r5 $0x2
    mul r5 r5 $0x0
    add r5 r5 $0x1c
    add r5 fp r5
    mov r2 $0x4d2
    stl r5 r2 $0x0
    sth r5 r2 $0x1
    mov r5 $0x2
    mul r5 r5 $0x1
    add r5 r5 $0x1c
    add r5 fp r5
    mov r2 $0x10e1
    stl r5 r2 $0x0
    sth r5 r2 $0x1
    mov r5 $0x2
    mul r5 r5 $0x0
    add r5 r5 $0x1c
    add r5 fp r5
    ldl r2 r5 $0x0
    ldh r2 r5 $0x1
    printi r2
    mov r5 $0x2
    mul r5 r5 $0x1
    add r5 r5 $0x1c
    add r5 fp r5
    ldl r4 r5 $0x0
    ldh r4 r5 $0x1
    printi r4
    mov r5 $0x2
    mul r5 r5 $0x0
    add r5 r5 $0x1c
    add r5 fp r5
    ldl r6 r5 $0x0
    ldh r6 r5 $0x1
    printi r6
    add fp fp $0x1c
    mov sp fp
    halt 