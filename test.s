%start
  sub fp fp $0x0
  mov sp fp
  mov fp $0xf000
  mov sp fp
%startbody
  mov r5 $0x0
  sub r0 r5 r0
  jz %startels0
  jmp %startthen0
  %startthen0
    %startthen0block0
      printi $0x2
    jmp %startend0
  %startels0
    %startels0block0
      printi $0x3
    jmp %startend0
%startend0
  add fp fp $0x0
  mov sp fp
%startreturn
  halt 