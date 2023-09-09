%start
    ; setup initial stack frame
    mov     sp $0xF000
    mov     fp sp

    mov     r1 $1337
    mov     r2 r1
    printi  r2

    jmp     %done
%done
    halt
