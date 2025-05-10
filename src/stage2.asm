[org 0x500]
;             In 16 bit mode setup 
ENTRY:
    [bits 16]
    cli                                             ;disable the interrupts

    mov     [boot_drive], dl
    mov     ax, ds
    mov     ss, ax
    mov     sp, 0xFFF0

    call    ENABLE_A20                              ;enable A20 line

    lgdt    [gdt_desc]                              ;load gdt        

    mov     eax, cr0                                ;load control register flags
    or      eax, 0x1                                ;set PE bit
    mov     cr0, eax                                

    jmp     0x8:PROTECTED_MODE                      ;0x8 locates the 32bit code descriptor 
                                                    ;jmp to 32 bit protected mode with no paging

ENABLE_A20:
    [bits 16]
    call    A20WAIT
    mov     al,0xAD
    out     0x64,al

    call    A20WAIT
    mov     al,0xD0
    out     0x64,al

    call    A20WAIT2
    in      al,0x60
    push    eax

    call    A20WAIT
    mov     al,0xD1
    out     0x64,al

    call    A20WAIT
    pop     eax
    or      al,2
    out     0x60,al

    call    A20WAIT
    mov     al,0xAE
    out     0x64,al

    call    A20WAIT
    ret


A20WAIT:
    [bits 16]
    in      al, 0x64
    test    al, 2
    jnz     A20WAIT
    ret

A20WAIT2:
    [bits 16]
    in      al, 0x64
    test    al, 1
    jz      A20WAIT2
    ret



PROTECTED_MODE:
    [bits 32]
                                                    ; Set up segment registers for 32-bit protected mode
    mov     ax, 0x10                                ; Data segment selector
    mov     ds, ax                                  ; the data segment in the 0x10 entry/index of gdt
    mov     es, ax  
    mov     fs, ax
    mov     gs, ax
    mov     ss, ax

                                                    ;Display 'S' to display you're in 32 bit mode
    mov     eax, 0xB8000                            ;VGA text buffer
    mov     byte [eax], 'S'
    mov     byte [eax+1], 0x0F

    mov     eax, cr4                                ;load contents of cr4
    or      eax, 1 << 5                             ;Set PAE bit
    mov     cr4, eax                                    

                    
    mov     eax, pml4                               ;Set CR3 to PML4 physical address
    mov     cr3, eax
    
    mov     ecx, 0xC0000080                         ;EFER MSR
    rdmsr
    or      eax, 1 << 8                             ;Set LME (Long Mode Enable)
    wrmsr

    call    SETUP_PAGING

    
    mov     eax, cr0                                ;Enable paging bit
    or      eax, 1 << 31                            ;Set PG bit i.e. 31st bit 
    mov     cr0, eax

    jmp     0x18:LONG_MODE                          ;jmp to 64 bit long mode with paging

SETUP_PAGING:
    [bits 32]
                                                    ; Clear page tables (16KB total)
    mov     edi, pml4
    xor     eax, eax
    mov     ecx, 4096 / 4                           ; Clear 4KB per table
    rep     stosd
    mov     edi, pdpt
    mov     ecx, 4096 / 4
    rep     stosd
    mov     edi, pd
    mov     ecx, 4096 / 4
    rep     stosd
    mov     edi, pt
    mov     ecx, 4096 / 4
    rep     stosd

                                                    ; PML4: Point to PDPT
    mov     edi, pml4
    mov     eax, pdpt
    or      eax, 0x03                               ; Present, writable
    mov     [edi], eax

                                                    ; PDPT: Point to PD
    mov     edi, pdpt
    mov     eax, pd
    or      eax, 0x03                               ; Present, writable
    mov     [edi], eax

                                                    ; PD: Point to PT
    mov     edi, pd
    mov     eax, pt
    or      eax, 0x03                               ; Present, writable
    mov     [edi], eax

                                                    ; PT: Identity-map first 2MB (512 entries, 4KB each)
    mov     edi, pt
    mov     eax, 0x03                               ; Present, writable
    mov     ecx, 512                                ; 512 entries
PT_LOOP:
    mov [edi], eax
    add eax, 0x1000                                 ; Next 4KB page
    add edi, 8                                      ; Next page table entry
    loop PT_LOOP

    ret

LONG_MODE:
    [bits 64]
    mov     ax, 0x10                                ;Set up segment registers (only CS matters in 64-bit mode)
    mov     ds, ax                                  ;others can be the same data segment used in the 32 bit mode
    mov     es, ax                                  ;i.e. 0x10 selector
    mov     fs, ax
    mov     gs, ax
    mov     ss, ax

                                                    ;Set up stack
    mov     rsp, 0x90000                            ;Stack at 0x90000 just make sure its big enough 

    
                                                    ;Jump to kernel main
    jmp     KERNEL_MAIN                             ;could set it as external function

    



boot_drive          db  0


gdt_start:
;---------------------------------------------------------------------------------------------------------
;                   null descriptor
                    dq  0                           ;8 bytes                              

;---------------------------------------------------------------------------------------------------------                    
;                   Code Segment 32          
                    dw  0xFFFF                      ;limit  0-15bits
                    dw  0x0                         ;base   0-15
                    db  0x0                         ;base   16-23
                    db  10011010b                   ;[1(present) , 00(DPL), 1(s), 
                                                    ; 1(code), 0(readonly), 1(executable), 0(not accessed)]
                    db  11001111b                    ;[1(Granuality), 1(DB), 0(Long mode), 0(reserved),
                                                    ; 1111(F => limit(16-19)) ]
                    db  0x0                         ;base   24-31
;---------------------------------------------------------------------------------------------------------
;                   Data Segment 
                    dw  0xFFFF                      ;limit  0-15bits
                    dw  0x0                         ;base   0-15
                    db  0x0                         ;base   16-23
                    db  10010010b                   ;[1(present) , 00(DPL), 1(s), 
                                                    ; 1(code), 0(readonly), 1(executable), 0(not accessed)]
                    db  11001111b                   ;[1(Granuality), 1(DB), 1(Long mode), 0(reserved),
                                                    ; 1111(F => limit(16-19)) ]
                    db  0x0                         ;base   24-31                 
;---------------------------------------------------------------------------------------------------------
;                   Code Segment 64         
                    dw  0xFFFF                      ;limit  0-15bits
                    dw  0x0                         ;base   0-15
                    db  0x0                         ;base   16-23
                    db  10011010b                   ;[1(present) , 00(DPL), 1(s), 
                                                    ; 1(code), 0(readonly), 1(executable), 0(not accessed)]
                    db  10101111b                   ;[1(Granuality), 0(DB), 1(Long mode), 0(reserved),
                                                    ; 1111(F => limit(16-19)) ]
                    db  0x0                         ;base   24-31
;---------------------------------------------------------------------------------------------------------

gdt_end:
gdt_desc:
                    dw  gdt_end - gdt_start - 1     ;size
                    dd  gdt_start                   ;gdt base address (lower 32)
                    dd  0x0                         ;gdt base address (upper 32)






times 0xB00-($-$$)  db 0                            ;to align the paging


pml4: times 4096    db 0                            ; 0x1000–0x1FFF
pdpt: times 4096    db 0                            ; 0x2000–0x2FFF
pd:   times 4096    db 0                            ; 0x3000–0x3FFF
pt:   times 4096    db 0                            ; 0x4000–0x4FFF



KERNEL_MAIN:
    [bits 64]
                                                    ; Example kernel: Write "OK" to VGA buffer
    mov     rax, 0x2f4b2f4f                         ; "OK" in ASCII with white-on-green
    mov     [0xb8000], rax
    mov     rax, [0x1FFFF8]                         ; Should work (last mapped address)
    cli
    jmp     $