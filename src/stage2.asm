[bits 16]


mov     al, 'A'
mov     ah, 0x0E
mov     bl, 0x07
mov     bh, 0x00
int     0x10

mov     ah, 0
int     16h
hlt
      