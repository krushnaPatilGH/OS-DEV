[BITS 16]
[ORG 0x7C00]


;BIOS Paramater Block

JMP SHORT START 
NOP

bdb_oem                         db 'MSWIN4.1'                   ;NAME OF VERSION                                                => 8B
bdb_bytes_per_sector            dw 512                          ;bytes in 1 sector                                              => 2B
bdb_sectors_per_clusters        db 1                            ;sectors in 1 cluster                                           => 1B
bdb_reserved_sectors            dw 1                            ;reserved sectors, boot record is here                          => 2B
bdb_fat_count                   db 2                            ;FAT counts                                                     => 1B
bdb_root_dir_entries_count      dw 0E0H                         ;<<<<<<<unknown>>>>>>>>                                         => 2B
bdb_total_sectors               dw 2280                         ;total sectors on the disk, 2880*512 (bytes) = 1.44MB           => 2B
bdb_media_descriptor_type       db 0F0H                         ;F0 => number for floppy disk                                   => 1B
bdb_sectors_per_fat             dw 9                            ;sectors per FAT                                                => 2B
bdb_sectors_per_track           dw 18                           ;sectors in each track                                          => 2B
bdb_heads                       dw 2                            ;number of heads in the disk                                    => 2B
bdb_hidden_sectors              dd 0                            ;number of hidden sectors                                       => 4B
bdb_large_sector_count          dd 0                            ;if count cannot fit in the bdb_total_sectors                   => 4B        



; Extended Boot Record
ebr_drive_number                db 0                            ;change using BIOS interrupt 0x13h, 0x00 for floppy
                                db 0                            ;flags for windown NT reserved otherwise
ebr_signature                   db 28h                          ;must be 0x28 or 0x29
ebr_volume_id                   db 14h, 15h, 16h, 17h           ;id random but unique
ebr_volume_label                db 'OS To learn'                ;just a label not important
ebr_identifier                  db 'FAT12   '                   ;just for fat type



;boot code
START:

    ;initialize
    mov     ax, 0
    mov     ds, ax
    mov     es, ax
    mov     ss, ax
    mov     sp, 0x7C00
    push    es
    push    WORD .begin
    retf

.begin: 
    mov     [ebr_drive_number], dl                              ;after POST drive number is stored in dl by bios

    ; reading the drive params from the BIOS
    push    es
    mov     ah, 08h
    int     13h
    jc      FLOPPY_ERROR

    pop     es
    and     cl, 0x3f                                            ;max sector num is stored here
    xor     ch, ch                                              ;just set the cx to sector num per track
    mov     [bdb_sectors_per_track], cx                         ;store new values

    inc     dh
    mov     [bdb_heads], dh

    ;compute lba of root directory => reserved sectors + fat * sectors per fat
    mov     ax, [bdb_sectors_per_fat]                           ;calculate total sectors occupied by fat
    mov     bl, [bdb_fat_count]
    xor     bh, bh
    mul     bx                                                  

    add     ax, [bdb_reserved_sectors]                          ;store the lba of root dir in stack        
    push    ax

    ;size of root dir => dir count * 32 / bytes per sector 
    mov     ax, [bdb_root_dir_entries_count]                    
    shl     ax, 5                                               ;shl by 6 => *=32
    xor     dx, dx
    div     word [bdb_bytes_per_sector]
    test    dx, dx
    jz      ROOT_DIR_AFTER
    inc     ax

;read root dir and store it in the buffer
ROOT_DIR_AFTER:
    mov     cl, al                                              ;sectors to read
    pop     ax                                                  ;lba of root directory
    mov     dl, [ebr_drive_number]                              ;drive number
    mov     bx, buffer                                          ;es:bx memory
    call    DISK_READ                                           ;READ ROOT DIRECTORY AND STORE IT IN THE MEMORY

    xor     bx, bx                                              ;
    mov     di, buffer
SEARCH_STAGE2:                                                  ;find the entry in root dir with the stage2 bootloader
    mov     si, BOOTLOADER_STAGE2
    mov     cx, 11                                              ;compare the name 
    push    di
    repe    cmpsb
    pop     di
    je      FOUND_STAGE2                                        ;if name found jump

    add     di, 32                                              ;if not next root dir entry
    inc     bx
    cmp     bx, [bdb_root_dir_entries_count]                    ;if entries finished exit
    jl      SEARCH_STAGE2
    jmp     STAGE2_NOT_FOUND      

FOUND_STAGE2:
    mov     ax, [di + 26]
    mov     [STAGE2_CLUSTER], ax                                ;load first cluster address that is stored in the root dir
                                                                ;cuz buffer will be overwritten by the FAT
    ;load fat from disk into memory
    mov     ax, [bdb_reserved_sectors]                          ;read fat and store into the memory
    mov     bx, buffer
    mov     cl, [bdb_sectors_per_fat]
    mov     dl, [ebr_drive_number]
    call    DISK_READ

    ;read stage2 bootloader
    mov     bx, STAGE2_LOAD_SEGMENT                             ;memory address diffrent to buffer to load the memory            
    mov     es, bx                                              ;this is the one above the bootloader 29KB
    mov     bx, STAGE2_LOAD_OFFSET                              

LOAD_STAGE2_LOOP:
    mov     ax, [STAGE2_CLUSTER]                                ;load the address of the cluster
    add     ax, 31                                              ;offset to start the file
    mov     cl, 1                                               ;sectors to read
    mov     dl, [ebr_drive_number]                              ;drive no
    call    DISK_READ                                           ;read the stage2 and write in the memory

    add     bx, [bdb_bytes_per_sector]                          ;change the offset in the memory


    mov     ax, [STAGE2_CLUSTER]                                ;location of next cluster is => (curr cluster * 3) / 2
    mov     cx, 3
    mul     cx
    mov     cx, 2
    div     cx    

    mov     si, buffer                                          ;select the buffer that has root dir
    add     si, ax                                              ;add offset to reach the entry in the root dir
    mov     ax, [ds:si]                                         ;select the data stored at that root dir with offset

    or      dx, dx                                              ;check if there is a reminder of the previous div or not
    jz      EVEN

ODD:            
    shr     ax, 4                                               ;the entry is of 12 bits but register is 16 bits so shift
    jmp     NEXT_CLUSTER_AFTER                                  ;4 bits so that the entry becomes of 12 bits

EVEN:
    and     ax, 0xFFF                                           ;just keep the cluster number

NEXT_CLUSTER_AFTER:
    cmp     ax, 0xFF8                                           ;end of cluster chain
    jae     READ_FINISH

    mov     [STAGE2_CLUSTER], ax                                ;load the next cluster
    jmp     LOAD_STAGE2_LOOP

READ_FINISH:
    mov     dl, [ebr_drive_number]                              ;load drive number int dl
    mov     ax, STAGE2_LOAD_SEGMENT                             ;stage 2 segment
    
    mov     ds, ax
    mov     es, ax

    jmp     STAGE2_LOAD_SEGMENT:STAGE2_LOAD_OFFSET
    jmp     WAIT_FOR_KEY

    cli
    hlt     





;params:
;   ax      =>  lba
;   cl      =>  sectors to read
;   dl      =>  drive number
;   es:bx   =>  mem location
DISK_READ:
    push    cx
    call    LBA_TO_CHS
    
    pop     ax                                                  ;al=>count of sectors
    mov     ah, 02h                                         
    mov     di, 03h                                             ;retry 3 times

RETRY:
    pusha
    stc
    int     0x13
    
    jnc     DONE

    ;failed
    popa
    call    DISK_RESET
    
    dec     di
    test    di, di
    jnz     RETRY

FAIL:
    JMP     FLOPPY_ERROR
DONE:
    popa
    ret      
    

;parameter: ax=>lba
;out put:   cx[0-5]=>s, cx[6-15]=>c, dh=>h
LBA_TO_CHS:
    push    ax
    push    dx

    xor     dx, dx                                              ; ax=>lba   chs
    div     word [bdb_sectors_per_track] 

    inc     dx
    mov     cx, dx                                              ;cx[0:0......] cl = sectors

    xor     dx, dx
    div     word [bdb_heads]                                    ;ax=>cyl, dx=>hea

    mov     dh, dl                                              ;dh=>heads
    mov     ch, al                                              ;
    shl     ah, 6
    or      ch, ah

    pop     ax
    mov     dl, al
    pop     ax
    ret  



;params dl=>driveno
DISK_RESET:
    pusha
    mov     ah, 0
    stc 
    int     13h
    jc      FLOPPY_ERROR
    popa 
    ret

;printing
PRINTCHAR:
    mov     ah, 0x0E
    mov     bl, 0x07
    mov     bh, 0x00
    int     0x10
    ret
PRINTSTRING:
    mov     al, [si]
    inc     si
    or      AL, AL
    jz      DONE2
    call    PRINTCHAR
    jmp     PRINTSTRING
DONE2:
    ret


STAGE2_NOT_FOUND:
	mov     si, stage2_err
	call    PRINTSTRING
	jmp     WAIT_FOR_KEY
FLOPPY_ERROR:
    mov     si, error_floppy_msg
    call    PRINTSTRING
    jmp     WAIT_FOR_KEY

WAIT_FOR_KEY:
    mov     ah, 0
    int     16h                                                 ;wait for key press
    jmp     0FFFFh:0                                            ;to bios




;data 
stage2_err              db "stage 2 not found", 0
error_floppy_msg        db "error using floppy", 0
HELLO_STRING            db "hello how's everyone", 0
BOOTLOADER_STAGE2       db "STAGE2  BIN"

STAGE2_LOAD_SEGMENT		equ 0x0
STAGE2_LOAD_OFFSET      equ 0x500

STAGE2_CLUSTER          dw 0    

TIMES 510 - ($ - $$)    db 0
DW 0xAA55

buffer: