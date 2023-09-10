%start
    mov sp $0xf000
    mov fp sp
    sub fp fp $6
    mov sp fp
%startouter
    mov r1 $0x0
    stl fp r1 $0x2
    sth fp r1 $0x3
%startinit1
%startcond1
    mov r1 $0x1
    stl fp r1 $0x4
    sth fp r1 $0x5
    ldl r1 fp $0x4
    ldh r1 fp $0x5
    sub r0 r1 r0
    jz %startend1
    jmp %startbody1
%startbody1
    ldl r2 fp $0x2
    ldh r2 fp $0x3
    printi r2
    jmp %startstep1
%startstep1
    ldl r1 fp $0x2
    ldh r1 fp $0x3
    stl fp r1 $0x2
    sth fp r1 $0x3
    mov r1 $0x1
    stl fp r1 $0x6
    sth fp r1 $0x7
    ldl r1 fp $0x6
    ldh r1 fp $0x7
    ldl r5 fp $0x2
    ldh r5 fp $0x3
    add r5 r5 r1
    stl fp r5 $0x2
    sth fp r5 $0x3
    jmp %startcond1
%startend1
%startepilogue
    add fp fp $6
    mov sp fp
    halt 