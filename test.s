%start
    mov sp $0xf000
    mov fp sp
%startprologue
    sub fp fp $0x4
    add sp sp $0x2
    mov sp fp
%startouter
    mov r2 $0x539
    stl fp r2 $0x2
    sth fp r2 $0x3
    ldl r4 fp $0x2
    ldh r4 fp $0x3
    sub sp sp $0x2
    stl sp r4 $0x0
    sth sp r4 $0x1
    mov r4 %startmyprintiretaddr1
    sub sp sp $0x2
    stl sp r4 $0x0
    sth sp r4 $0x1
    jmp %myprinti
%startmyprintiretaddr1
    add fp fp $0x4
    ldl r5 fp $0x2
    ldh r5 fp $0x3
    mov r5 r1
    stl fp r5 $0x2
    sth fp r5 $0x3
    ldl r2 fp $0x2
    ldh r2 fp $0x3
    stl fp r2 $0x4
    sth fp r2 $0x5
    ldl r2 fp $0x4
    ldh r2 fp $0x5
    printi r2
%startepilogue
    halt 
%myprinti
%myprintiprologue
    sub fp fp $0x4
    add sp sp $0x2
    ldl r6 sp $0x0
    ldh r6 sp $0x1
    add sp sp $0x2
    stl fp r6 $0x2
    sth fp r6 $0x3
    mov sp fp
%myprintiouter
    ldl r2 fp $0x2
    ldh r2 fp $0x3
    stl fp r2 $0x4
    sth fp r2 $0x5
    ldl r2 fp $0x4
    ldh r2 fp $0x5
    printi r2
    ldl r3 fp $0x2
    ldh r3 fp $0x3
    mov r2 $0x1
    add r1 r3 r2
%myprintiepilogue
    add fp fp $0x4
    ldl r2 sp $0x0
    ldh r2 sp $0x1
    add sp sp $0x2
    mov sp fp
    jmp r2