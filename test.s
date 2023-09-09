%start
    ; setup initial stack frame
    mov     sp $0xF000
    mov     fp sp

%1
    printi $1
%2
    printi $2
%3
    printi $3
%4
    printi $4
%5
    printi $5

    jmp     %done
%done
    halt
