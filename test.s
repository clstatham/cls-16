%start
    mov sp $0xf000
    mov fp sp
    sub fp fp $14
    mov sp fp
%startouter
    mov r1 $0x0
    stl fp r1 $0x2
    sth fp r1 $0x3
%startinit1
    mov r3 $0x0
    stl fp r3 $0x2
    sth fp r3 $0x3
%startcond1
    ldl r2 fp $0x2
    ldh r2 fp $0x3
    mov r1 $0xa
    sub r3 r2 r1
    jc %starttrue3
    jmp %startfalse3
%starttrue3
    mov r3 $0x1
    jmp %startend3
%startfalse3
    mov r3 $0x0
%startend3
    stl fp r3 $0x4
    sth fp r3 $0x5
    ldl r1 fp $0x4
    ldh r1 fp $0x5
    sub r0 r1 r0
    jz %startend1
    jmp %startbody1
%startbody1
    ldl r2 fp $0x2
    ldh r2 fp $0x3
    mov r1 $0x3
    sub r3 r2 r1
    jc %starttrue7
    jmp %startfalse7
%starttrue7
    mov r3 $0x1
    jmp %startend7
%startfalse7
    mov r3 $0x0
%startend7
    stl fp r3 $0x6
    sth fp r3 $0x7
    ldl r1 fp $0x6
    ldh r1 fp $0x7
    sub r0 r1 r0
    jz %startelse10
    jmp %startif10
%startif10
    ldl r2 fp $0x2
    ldh r2 fp $0x3
    mov r1 $0xa
    ldl r3 fp $0x8
    ldh r3 fp $0x9
    add r3 r2 r1
    stl fp r3 $0x8
    sth fp r3 $0x9
    ldl r1 fp $0x8
    ldh r1 fp $0x9
    printi r1
    jmp %startend10
%startelse10
    ldl r4 fp $0x2
    ldh r4 fp $0x3
    mov r3 $0x8
    sub r5 r4 r3
    jc %starttrue12
    jmp %startfalse12
%starttrue12
    mov r5 $0x1
    jmp %startend12
%startfalse12
    mov r5 $0x0
%startend12
    mov r2 r5
    ldl r4 fp $0x2
    ldh r4 fp $0x3
    mov r3 $0x5
    sub r5 r4 r3
    jc %startfalse15
    jz %starttrue15
    jmp %starttrue15
%starttrue15
    mov r5 $0x1
    jmp %startend15
%startfalse15
    mov r5 $0x0
%startend15
    mov r1 r5
    ldl r3 fp $0x8
    ldh r3 fp $0x9
    and r3 r2 r1
    stl fp r3 $0x8
    sth fp r3 $0x9
    ldl r1 fp $0x8
    ldh r1 fp $0x9
    sub r0 r1 r0
    jz %startelse18
    jmp %startif18
%startif18
    ldl r2 fp $0x2
    ldh r2 fp $0x3
    mov r1 $0x14
    ldl r3 fp $0xa
    ldh r3 fp $0xb
    add r3 r2 r1
    stl fp r3 $0xa
    sth fp r3 $0xb
    ldl r1 fp $0xa
    ldh r1 fp $0xb
    printi r1
    jmp %startend18
%startelse18
    ldl r2 fp $0x2
    ldh r2 fp $0x3
    mov r1 $0x1e
    ldl r3 fp $0xa
    ldh r3 fp $0xb
    add r3 r2 r1
    stl fp r3 $0xa
    sth fp r3 $0xb
    ldl r1 fp $0xa
    ldh r1 fp $0xb
    printi r1
    jmp %startend18
%startend18
    jmp %startend10
%startend10
    jmp %startstep1
%startstep1
    ldl r4 fp $0x2
    ldh r4 fp $0x3
    mov r3 $0x1
    ldl r5 fp $0x2
    ldh r5 fp $0x3
    add r5 r4 r3
    stl fp r5 $0x2
    sth fp r5 $0x3
    jmp %startcond1
%startend1
%startepilogue
    add fp fp $14
    mov sp fp
    halt 