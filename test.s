%start
    mov r5 $0x2a
    stl fp r5 $0x4
    sth fp r5 $0x5
    ldl r5 fp $0x4
    ldh r5 fp $0x5
    printi r5
    halt 