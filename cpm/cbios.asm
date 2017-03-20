
;-----------------------------------
;        CBIOS FOR CP/M 2.2
;
;       for the KIMP1 system
;
;  written for the zmac assembler
;
;      Copyleft 2017 Zalasus
;       all wrongs reversed
;-----------------------------------

    ; include definition file for KIMP1 computer
    include ../kimp1def.inc

    ; CP/M config (shared by bootloader and BDOS assembly files)
    include conf.inc


; interrupt locations and corresponding restart instructions
INT_FDC_VECTOR:   equ $0008
IVR_FDC_RESTART:  equ $CF
IVR_FDC_NOP:      equ $00

INT_RTC_VECTOR:   equ $0010
IVR_RTC_RESTART:  equ $D7


; write type constants
CPM_WRITETYPE_ALLOC:   equ 0 ;write to allocated
CPM_WRITETYPE_DIR:     equ 1 ;write to directory
CPM_WRITETYPE_UNALLOC: equ 2 ;write to unallocated




bias:    equ (CONF_CPM_MEM-20)*1024
ccp:     equ 3400H+bias ;base of ccp
bdos:    equ ccp+806h   ;base of bdos
bios:    equ ccp+1600h  ;base of bios
    
sectorsToLoad: equ (bios - ccp) / 512

    org bios

; CBIOS jump vector
    jp  boot        ;cold start
    jp  wboot       ;warm start
    jp  const       ;console status
    jp  conin       ;console character in
    jp  conout      ;console character out
    jp  listout     ;list character out (renamed as zmac uses list as keyword)
    jp  punch       ;punch character out
    jp  reader      ;reader character out
    jp  home        ;move head to home position
    jp  seldsk      ;select disk
    jp  settrk      ;set track number
    jp  setsec      ;set sector number
    jp  setdma      ;set dma address
    jp  read        ;read disk
    jp  write       ;write disk
    jp  listst      ;return list status
    jp  sectran     ;sector translate

    

; ------------------- FIXED DATA AREAS -----------------------

DAT_CPM_DPH_BASE:  ; base address for disk parameter header
    ; DPH for drive 0
    dw $0000           ; sector translation table address (no translation done)
    dw $0000           ; current track number
    dw $0001           ; current sector number
    dw $0000           ; current directory number
    dw DAT_CPM_DIRBUF  ; directory buffer address, shared by all drives
    dw DAT_CPM_DPB     ; DPB address
    dw DAT_CPM_CSV_0   ; check sum vector address
    dw DAT_CPM_ALV_0   ; allocation vector address

    ; DPH for drive 1
    dw $0000           ; sector translation table address (no translation done)
    dw $0000           ; current track number
    dw $0001           ; current sector number
    dw $0000           ; current directory number
    dw DAT_CPM_DIRBUF  ; directory buffer address, shared by all drives
    dw DAT_CPM_DPB     ; DPB address
    dw DAT_CPM_CSV_1   ; check sum vector address
    dw DAT_CPM_ALV_1   ; allocation vector address    


; Disk parameter block for allocation block size of 2kiB
;  Our disks have 9 sectors per track of 512 bytes each, and 160 tracks (80 on ech side)
DAT_CPM_DPB:
    dw 36    ; count of 128-byte sectors per track (9 * 512/128)
    db 4     ; block shift factor
    db $0f   ; block mask
    db 0     ; extent mask
    dw 354   ; storage capacity (index of last allocation block, 0-based)  (9*512*158)/2048-1
    dw 255   ; number of last directory entry
    db $f0   ; mask for reserved directory blocks (we need 4 blocks to store 256 entries of 32 bytes)
    db $00   ;     "
    dw 64    ; checksum vector size
    dw 2     ; number of reserved tracks at start of disk



str_welcome:
    db '63k CP/M 2.2 FOR THE KIMP1', $0D, $0A, '(C) 1979 DIGITAL RESEARCH'
    db $0D, $0A, $00

str_bootFail:
    db 'FAILED TO BOOT', $0D, $0A, $00

str_biosPanic:
    db 'BIOS PANIC', $0D, $0A, $00

;------------------------CP/M BIOS ROUTINES----------------------------
    
; cold boot. only called by bootloader. this does not have to load anything since the bootloader
;  will already have loaded the BIOS, BDOS and CCP once it transfers control to here.
;  All that is left to do is turning off the ROM, initializing interrupt vectors 
;  (as they were previously stored in ROM) and the rest of the zeropage and eventually giving 
;  control to CP/M.
boot:
    ld SP, ccp

    call lowLevelSetup  ; will redifine IVRs and turn off ROM mapping

    ; print logon string
    ld HL, str_welcome
    call printString

    jp gocpm
    
    

; this is called when the CCP needs to be reloaded. basically only called when CP/M has been
;  running on this machine before, as such we don't need to fiddle with ROM mapping etc.
wboot:
    ; since CP/M uses a different stack location, we should restore a safe location at the end
    ; of CCP in the TPA. CP/M changes it back upon jumping to CCP
    ld SP, ccp

    ; just in case a transient program messed with this: turn off ROM and restore
    ;  interrupt locations again
    call lowLevelSetup

    xor A
    ld (DAT_DISK_NUMBER), A
    ld (DAT_DISK_TRACK), A
    ld A, 2   ; sector 1 stores bootloader. ignore it
    ld (DAT_DISK_SECTOR), A

    call fdc_recalibrate
    jp c, bootFail

    ld C, 0   ; number of sectors loaded

_wboot_loop:
    ; calculate target address for read
    ld HL, ccp
    ld A, C
    add A   ; shift left sector count 1 bit
    add H   ; add to H <=> shift left 8 bits -> sector count times 512
    ld H, A
    ld (DAT_DISK_DATAPTR), HL

    ld D, CONF_DISK_RETRIES  ; retries before fail
_wboot_retry:
    call fdc_readData
    jp nc, _wboot_noError
    dec D
    jp nz, _wboot_retry
    jp bootFail
_wboot_noError:

    inc C
    ld A, C
    cp sectorsToLoad   ; CCP+BDOS is 5632 bytes long -> exactly 11 sectors of 512 bytes needed
    jp z, gocpm  ; all sectors loaded. give control back to ccp (after intializing zeropage)

    ; increment sector number
    ld A, (DAT_DISK_SECTOR)
    inc A
    ld (DAT_DISK_SECTOR), A
    cp 10     ; check if carry to next track is neccessary
    jp nz, _wboot_loop

    ; carry over to next track
    ld A, 1
    ld (DAT_DISK_SECTOR), A
    ld A, (DAT_DISK_TRACK)
    inc A
    ld (DAT_DISK_TRACK), A
    call fdc_seek
    jp c, bootFail  ; no use in retrying seek operation
    jp _wboot_loop

    

gocpm:
    ; initialize system vectors in zeropage
    ; address 0 contains a jump to wboot
    ld A, $C3  ; jp instruction
    ld ($0000), A
    ld HL, wboot
    ld ($0001), HL

    ; address 5 contains a jump to BDOS
    ld ($0005), A
    ld HL, bdos
    ld ($0006), HL

    ld BC, $0080  ; initalize default DMA address
    call setdma

    call printNewLine
    call printNewLine

    ld C, $00  ; ccp will select this drive (low nibble) and user number (high nibble)

    jp ccp  ; give control to CP/M
    


; Turns off ROM mapping and redefines interrupt locations and IVRs.
lowLevelSetup:
    ; Turn off ROM mapping.
    ;  After ROM is turned off interrupt vectors need to be redefined.
    ;  Don't interrupt during this period.
    di
    ld A, [1 << BIT_TCCR_ROM_GATE] | [1 << BIT_TCCR_C2_GATE]  ; don't turn off the UART timer
    out (IO_TCCR), A

    ; redefine interrupt vectors
    ;  note that these vectors are different than the ones used by the
    ;  monitor as CP/M reserves some interrupt vectors
    ld A, $C3    ; jp instruction

    ; fdc interrupt at rst 08h
    ld (INT_FDC_VECTOR), A
    ld HL, fdc_isr
    ld (INT_FDC_VECTOR+1), HL

    ; rtc interrupt at rst 10h
    ld (INT_RTC_VECTOR), A
    ld HL, rtc_isr
    ld (INT_RTC_VECTOR+1), HL

    ; redefine IVRs
    ld A, IVR_FDC_RESTART
    out (IO_IVR_FDC), A

    ld A, IVR_RTC_RESTART
    out (IO_IVR_RTC), A

    ei  ; now interrupts are safe again

    ret

    

; sets A to $FF if console has readable byte, $00 if not
const:
    in A, (IO_UART_COM) ; read in status byte of UART
    and [1 << BIT_UART_RXRDY]
    jp z, _const_notReady ; bit is zero -> not ready
    ld A, $ff
    ret
_const_notReady:
    xor A ; set A to 0
    ret


    
; prints character in C to console
conout:
    in A, (IO_UART_COM) ; read in status byte of UART
    and [1 << BIT_UART_TXRDY]
    jp z, conout
    
    ; UART is ready to send another byte now
    ld A, C
    out (IO_UART_DAT), A ; UART will start sending the byte now
    
    ret


    
; reads character from console into A
conin:
    in A, (IO_UART_COM) ; read in status byte of UART
    and [1 << BIT_UART_RXRDY]
    jp z, conin ; do this until UART has a valid byte
    
    in A, (IO_UART_DAT) ; read in data byte
    
    ret
    


; Dummy method for list/punch since we don't have such
;  devices
listout:
punch:
    ld A, C
    ret



; Returns status of listing device. Since there
;  is none, returns not ready always.
listst:
    xor A
    ret



; No, there is no paper strip reader either. Return
;  $1A (end-of-file) as suggested by Alteration Guide.
reader:
    ld A, $1A
    ret



; Move currently selected drive to track 0
home:
    ld A, (DAT_CPM_HOSTDIRTY)
    or A
    jp nz, _home_dirty
    ld (DAT_CPM_HOSTBUFF_ACTIVE), A
_home_dirty:
    xor A
    ld (DAT_CPM_BIOSTRACK), A
    ret



; Selects drive in C, return DPH address in HL
seldsk:
    ; check if disk exists
    ld A, C
    and $fe  ; only last bit may be set (only support 2 drives)
    jp nz, _seldsk_nonExistentDrive

    ; get DPH address
    ;  no shifting etc. needed. we only have two drives. condition is faster
    ld A, C
    ld (DAT_CPM_BIOSDISK), A
    or A
    jp nz, _seldsk_disk1
    
    ld HL, DAT_CPM_DPH_BASE
    ret

_seldsk_disk1:
    ld HL, DAT_CPM_DPH_BASE+16
    ret

_seldsk_nonExistentDrive:
    ld HL, $0000
    ret



; Selects track in BC
settrk:
    ld A, C
    ld (DAT_CPM_BIOSTRACK), A
    ret



; Selects sector in BC
setsec:
    ld A, C
    ld (DAT_CPM_BIOSSECTOR), A
    ret

    

; Selects DMA address in BC
setdma:
    ld H, B
    ld L, C
    ld (DAT_CPM_DMAPTR), HL
    ret



; Translates sector given in BC using translation table in DE.
;  Returns translated sector in HL
sectran:
    ld H, B   ; no translation done
    ld L, C
    ret

;----------------------------------------------------------------------
    
; the following is an implementation of the example blocking/deblocking
;  algorithm from the CP/M 2.2 Alteration Guide with a few changes and
;  more annotations. Track numbers have been reduced to 8 bits.

read:
    xor A
    ld (DAT_CPM_UNALLOC_COUNT), A
    inc A
    ld (DAT_CPM_BIOSREAD), A       ; this is a read operation, so set flag
    ld (DAT_CPM_PREREAD_FLAG), A   ; a read is always neccessary for a read
    ; treat as write to unallocated, which won't require an immediate
    ;  writeback. this way we can save a check in the rw routine.
    ld A, CPM_WRITETYPE_UNALLOC
    ld (DAT_CPM_WRITETYPE), A
    jp rwoper



write:
    xor A
    ld (DAT_CPM_BIOSREAD), A  ; not a read operation

    ; check write type
    ld A, C
    ld (DAT_CPM_WRITETYPE), A
    cp CPM_WRITETYPE_UNALLOC
    jp nz, _write_checkUnallocated

    ; write to first sector of unallocated block
    ld A, 16  ; 16 CP/M sectors per allocation block
    ld (DAT_CPM_UNALLOC_COUNT), A
    ld A, (DAT_CPM_BIOSDISK)
    ld (DAT_CPM_UNALLOC_DISK), A
    ld A, (DAT_CPM_BIOSTRACK)
    ld (DAT_CPM_UNALLOC_TRACK), A
    ld A, (DAT_CPM_BIOSSECTOR)
    ld (DAT_CPM_UNALLOC_SECTOR), A

_write_checkUnallocated:
    ld A, (DAT_CPM_UNALLOC_COUNT)
    or A
    jp z, _write_alloc

    ; unallocated sectors remain
    dec A
    ld (DAT_CPM_UNALLOC_COUNT), A
    ld A, (DAT_CPM_BIOSDISK)
    ld HL, DAT_CPM_UNALLOC_DISK
    cp (HL)
    jp nz, _write_alloc

    ; disks are the same
    ld A, (DAT_CPM_BIOSTRACK)
    ld HL, DAT_CPM_UNALLOC_TRACK
    cp (HL)
    jp nz, _write_alloc

    ; tracks are the same
    ld A, (DAT_CPM_BIOSSECTOR)
    ld HL, DAT_CPM_UNALLOC_SECTOR
    cp (HL)
    jp nz, _write_alloc

    inc (HL)
    ld A, (HL) ; are we at end of track?
    cp 72  ; CP/M sectors per track
    jp c, _write_noTrackCarry

    ; carry over to next track
    ld (HL), 0  ; unallocated sector = 0
    ld A, (DAT_CPM_UNALLOC_TRACK) ; increment track number
    inc A
    ld (DAT_CPM_UNALLOC_TRACK), A

_write_noTrackCarry:
    ; this sector is part of an unallocated block.
    ;  a pre-read is not necessary
    xor A
    ld (DAT_CPM_PREREAD_FLAG), A
    jp rwoper

_write_alloc:
    ; this sector belongs to an allocated block, so it's contents
    ;  matter. mark as allocated with pre-read necessary
    xor A
    ld (DAT_CPM_UNALLOC_COUNT), A
    inc A
    ld (DAT_CPM_PREREAD_FLAG), A  ; read flag = 1



rwoper:
    xor A  ; clear error code
    ld (DAT_DISK_ERRORCODE), A

    ; calculate physical sector number from CP/M sector number
    ld A, (DAT_CPM_BIOSSECTOR)
    rrca  ; shift right by 2 -> divide by 4 (512/128 = 4)
    rrca
    and $3f  ; the lower 2 bits were shifted in the upper 2. mask them out
    ld (DAT_CPM_HOSTBIOSSECTOR), A

    ; mark host buffer as active and fill it if it was not previously active
    ld HL, DAT_CPM_HOSTBUFF_ACTIVE
    ld A, (HL)
    ld (HL), 1
    or A
    jp z, _rwoper_fillhst

    ; host buffer was already active. does it buffer the
    ;  same location (disk, track, sector) as the CP/M-buffer?
    ld A, (DAT_CPM_BIOSDISK)
    ld HL, DAT_CPM_HOST_DISK
    cp (HL)
    jp nz, _rwoper_nomatch   ; disks did not match

    ld A, (DAT_CPM_BIOSTRACK)
    ld HL, DAT_CPM_HOST_TRACK
    cp (HL)
    jp nz, _rwoper_nomatch   ; track did not match

    ld A, (DAT_CPM_HOSTBIOSSECTOR)
    ld HL, DAT_CPM_HOST_SECTOR
    cp (HL)
    jp z, _rwoper_match

_rwoper_nomatch:
    ; the host buffer does not buffer the CP/M sector we want.
    ;  we need to read the one host sector that intersects the CP/M sector we want
    ;  to the buffer. did we write to the host buffer recently? if yes -> write back
    ;  changes first
    ld A, (DAT_CPM_HOSTDIRTY)
    or A
    call nz, writeHost
    
_rwoper_fillhst:
    ; the host buffer did not buffer the CP/M sector we want (either because
    ;  buffer was not yet active or because adresses did not match). update
    ;  address information and fill buffer with proper sector if needed.
    ;  the latter might be unnecesary since the actual contents of an unallocated
    ;  sector are irrelevant, so by skipping the pre-read we can save some overhead
    ld A, (DAT_CPM_BIOSDISK)
    ld (DAT_CPM_HOST_DISK), A
    ld A, (DAT_CPM_BIOSTRACK)
    ld (DAT_CPM_HOST_TRACK), A
    ld A, (DAT_CPM_HOSTBIOSSECTOR)
    ld (DAT_CPM_HOST_SECTOR), A
    ld A, (DAT_CPM_PREREAD_FLAG)      ; is a read marked as necessary? 
    or A
    call nz, readHost
    xor A
    ld (DAT_CPM_HOSTDIRTY), A  ; freshly read host buffer. not dirty

_rwoper_match:
    ; either the buffer was buffering the right sector or we filled it
    ;  with the right section. now we have to move the CP/M sector we want to
    ;  or from the CP/M buffer.
    
    ; we need the offset at which we find the CP/M sector in our host sector.
    ;  since there are 4 CP/M sectors per host sector, the offset is equal to
    ;  the lowest 2 bits of the CP/M sector address times 128 (shift left by 7)
    ld A, (DAT_CPM_BIOSSECTOR)
    and $03      ; mask out lower 2 bits
    ; shift A in HL. do this by using a bit of rotation trickery
    rrca  ; bit 0 is now in bit 7, where it belongs in L, and bit 1 is in bit 0 for H
    ld L, A
    ld H, A
    ; now bit 0 of H and bit 7 of L are the right value. since we were
    ;  rotating, the other bits have to be cleared
    res 0, L
    res 7, H
    ; shift left by 7 is complete. now add base address of host buffer
    ;  for absolute address
    ld DE, DAT_CPM_HOSTBUFFER
    add HL, DE
    ex DE, HL  ; DE now holds the target address in the host buffer
    
    ; perform a memcopy. DE is the source address, HL the target
    ld HL, (DAT_CPM_DMAPTR)
    ld C, 128
    ld A, (DAT_CPM_BIOSREAD)  ; is this a read operation? if yes, transfer is from host to CP/M
    or A
    jp nz, _rwoper_moveLoop

    ; write operation. swap source and destination of copy (from CP/M buffer to host buffer)
    ;  and mark host buffer as dirty
    ld A, 1
    ld (DAT_CPM_HOSTDIRTY), A
    ex DE, HL

_rwoper_moveLoop:
    ld A, (DE)
    inc DE
    ld (HL), A
    inc HL
    dec C
    jp nz, _rwoper_moveLoop

    ; buffers have been transferred. if the operation performed was
    ;  a write to a directory sector, the next operation will probably
    ;  be to a different location entirely, so we're better off syncing the
    ;  host buffer right away
    ld A, (DAT_CPM_WRITETYPE)
    cp CPM_WRITETYPE_DIR
    ld A, (DAT_DISK_ERRORCODE)
    ret nz    ; not write to directory. we're done

    ; write to directory. write back buffer immediately
    or A
    ret nz    ; error flag set? skip write back and report
    xor A
    ld (DAT_CPM_HOSTDIRTY), A   ; after write back, buffer won't be dirty anymore
    call writeHost
    ld A, (DAT_DISK_ERRORCODE)
    ret



readHost:
    ld DE, fdc_readData
    jp diskCommand



writeHost:
    ld DE, fdc_writeData
    jp diskCommand



diskCommand:
    ; did the disk address change from last command? if yes, recalibrate drive first
    ld A, (DAT_CPM_HOST_DISK)
    ld HL, DAT_DISK_NUMBER
    cp (HL)
    ld (HL), A
    jp z, _diskCommand_noDiskChange
    call fdc_recalibrate
    jp c, _diskCommand_error
_diskCommand_noDiskChange:

    ld A, (DAT_CPM_HOST_TRACK)
    ld (DAT_DISK_TRACK), A

    ld A, (DAT_CPM_HOST_SECTOR)
    inc A  ; CP/M seems to give 0-based sector addresses here
    ld (DAT_DISK_SECTOR), A

    call fdc_seek
    jp c, _diskCommand_error

    ld C, CONF_DISK_RETRIES

_diskCommand_retry:
    ld HL, _diskCommand_return
    push HL
    push DE
    ret   ; bogus return; serves as indirect jump to DE
_diskCommand_return:
    jp nc, _diskCommand_done
    dec C
    jp nz, _diskCommand_retry

_diskCommand_error:
    ld A, $01
    ld (DAT_DISK_ERRORCODE), A
    ret

_diskCommand_done:
    xor A
    ld (DAT_DISK_ERRORCODE), A
    ret



;----------------------------------------------------------------------



; Prints error message and hangs
bootFail:
    di
    ld HL, str_bootFail
    call printString
_bootFail_hang:
    jp _bootFail_hang



; Same as above, but for runtime errors
biosPanic:
    di
    ld HL, str_biosPanic
    call printString
_biosPanic_hang:
    jp _biosPanic_hang



; prints CRLF characters
printNewLine:
    ld C, $0D
    call conout
    ld C, $0A
    call conout
    ret
    


printString:
    ld A, (HL) ; fetch byte
    or A
    ret z ; if byte is zero, we are done
    
    ;byte is not zero, so print it out
    ld C, A
    call conout
    
    inc HL ; increment HL and loop
    jp printString
    


resetCarryReturn:
    scf
    ccf
    ret



setCarryReturn:
    scf
    ret





    include io_fdc.asm

    include io_rtc.asm


    
    
bios_end:



; ----------------- NON-FIXED DATA AREA -------------------

DAT_RTC_COUNTER: db 0

; RTC callback address
DAT_RTC_CALLBACK: dw $0000



; Flag for currently active drive motor
DAT_DISK_MOTOR_DRIVE: db $00

; as returned by the SENSEI command
DAT_DISK_INT_SR0:      db 0
DAT_DISK_INT_CYLINDER: db 0

DAT_DISK_NUMBER:     db $ff   ; impossible drive so the first thing we do is recalibrate
DAT_DISK_TRACK:      db 0
DAT_DISK_TRACK_PHYS: db 0
DAT_DISK_HEAD:       db 0
DAT_DISK_SECTOR:     db 1
DAT_DISK_DATAPTR:    dw DAT_CPM_HOSTBUFFER

DAT_DISK_RES_BUFFER: dc 7, $00

; the error code to be returned to CP/M
DAT_DISK_ERRORCODE:   db 0



; The disk addresses as selected by the BIOS calls
DAT_CPM_BIOSDISK:   db 0
DAT_CPM_BIOSTRACK:  db 0
DAT_CPM_BIOSSECTOR: db 0

; the host sector that contains the CP/M sector we want
;  (translated bios sector)
DAT_CPM_HOSTBIOSSECTOR: db 0

; The disk address currently referenced by the host buffer
DAT_CPM_HOST_DISK:   db 0
DAT_CPM_HOST_TRACK:  db 0
DAT_CPM_HOST_SECTOR: db 0

; flag that is set if host buffer was used before (contains meaningful data)
;  if it is not set upon r/w we know for sure we need to fill it before doing stuff
DAT_CPM_HOSTBUFF_ACTIVE: db 0

; flag that is set if host buffer has unsaved changes
DAT_CPM_HOSTDIRTY: db 0
    
DAT_CPM_UNALLOC_COUNT:   db 0
DAT_CPM_UNALLOC_DISK:    db 0
DAT_CPM_UNALLOC_TRACK:   db 0
DAT_CPM_UNALLOC_SECTOR:  db 0

DAT_CPM_PREREAD_FLAG: db 0
DAT_CPM_BIOSREAD:     db 0
DAT_CPM_WRITETYPE:    db 0
DAT_CPM_DMAPTR:       dw $0080


; Buffer areas for BDOS
DAT_CPM_CSV_0:  dc 64, $00
DAT_CPM_ALV_0:  dc 46, $00
DAT_CPM_CSV_1:  dc 64, $00
DAT_CPM_ALV_1:  dc 46, $00
DAT_CPM_DIRBUF: dc 128, $00

; Buffer for blocking/deblocking
DAT_CPM_HOSTBUFFER:   dc 512, $00


    end
    
    
    
    
    
    
    
    