[bits 16]
ENTRY:


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

    jmp     0x8:PROTECTED_MODE

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
    jnz     A20WAIT2
    ret


[bits 32]
PROTECTED_MODE:
    ; Set up segment registers for 32-bit protected mode
    mov     ax, 0x10                                ; Data segment selector
    mov     ds, ax
    mov     es, ax
    mov     fs, ax
    mov     gs, ax
    mov     ss, ax

    mov     eax, cr4
    or      eax, 1 << 5                             ;Set PAE bit
    mov     cr4, eax

    mov     eax, pml4_table                         ;load pml4 table address
    mov     cr3, eax                                ;store it in cr3

    ; Enable long mode
    mov ecx, 0xC0000080     ; EFER MSR
    rdmsr
    or eax, 1 << 8          ; Set LME bit
    wrmsr

    ; Enable paging
    mov eax, cr0
    or eax, 1 << 31         ; Set PG bit
    or eax, 1 << 0          ; Ensure PE bit
    mov cr0, eax

    ; Far jump to 64-bit code (selector 0x8 = code segment)
    jmp 0x8:long_mode

[bits 64]
long_mode:
    ; Set up segment registers (only CS matters in 64-bit mode)
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax

    ; Set up stack
    mov rsp, 0x90000        ; Stack at 0x90000 (adjust as needed)

    ; Jump to kernel main
    mov rax, kernel_main
    jmp rax

    ; Fallback infinite loop
    cli
    hlt
    jmp $



boot_drive          db  0



;GDT  datastructure note: it is divided 
align 8
gdt_start:
                    dq  0                           ;null descriptor

;---------------------------------------------------------------------------------------------------------                    
;                   Code Segment          
                    dw  0xFFFF                      ;limit  0-15bits
                    dw  0x0                         ;base   0-15
                    db  0x0                         ;base   16-23
                    db  10011010b                   ;[1(present) , 00(DPL), 1(s), 
                                                    ; 1(code), 0(readonly), 1(executable), 0(not accessed)]
                    db  11101111b                    ;[1(Granuality), 1(DB), 1(Long mode), 0(reserved),
                                                    ; 1111(F => limit(16-19)) ]
                    db  0x0                         ;base   24-31
                    dd  0x0                         ;base   32-61
                    dd  0x0                         ;reserved  
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
                    dd  0x0                         ;base   32-61
                    dd  0x0                         ;reserved                       
;---------------------------------------------------------------------------------------------------------
gdt_end:
gdt_desc:
    dw  gdt_end - gdt_start - 1                     ;size
    dd  gdt_start                                   ;gdt base address (lower 32)
    dd  0x0                                         ;gdt base address (upper 32)


align 4096
pml4_table:
    dq 0x0000000000001003   ; Points to PDPT, Present, Writable
    times 511 dq 0
pdpt_table:
    dq 0x0000000000002003   ; Points to PD, Present, Writable
    times 511 dq 0
pd_table:
    dq 0x0000000000000083   ; 2MB huge page, Present, Writable
    times 511 dq 0

[bits 64]
kernel_main:
    ; Example kernel: Write "OK" to VGA buffer
    mov rax, 0x2f4b2f4f     ; "OK" in ASCII with white-on-green
    mov [0xb8000], rax
    cli
    hlt
    jmp $