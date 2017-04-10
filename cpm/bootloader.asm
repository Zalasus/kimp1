
;-----------------------------
;
;   Bootloader for CP/M 2.2
;
;        for KIMP1
;
;-----------------------------


    include ../kimp1def.inc

    include ../mon/minimon_routines.inc

    include conf.inc



bias:    equ (CONF_CPM_MEM-20)*1024
ccp:     equ 3400H+bias ;base of ccp
boot:    equ ccp+1600h  ;base of bios, also cold boot jump vector

sectorsToLoad:    equ ($ffff - ccp + 1)/512

    org $2200

;------------- MAIN BOOTLOADER CODE-----------

    ; the loading location for the bios might overlap the stack.
    ;  create a temporary one under the ccp
    ld SP, ccp

    ld HL, str_loadingCPM
    call MINIMON_PRINTSTRING

    ld A, 0
    call MINIMON_SELECTDISK

_bootloader_loop:
    ; calculate target address for read
    ld HL, ccp
    ld A, (DAT_SECTORS_LOADED_TOTAL)
    add A   ; shift left sector count 1 bit
    add H   ; add to H <=> shift left 8 bits -> sector count times 512
    ld H, A

    ld A, (DAT_CURRENT_SECTOR)
    call MINIMON_READDATA   ; this includes retries
    jp c, bootFail

    ; increment total count and check if all are loaded
    ld A, (DAT_SECTORS_LOADED_TOTAL)
    inc A
    ld (DAT_SECTORS_LOADED_TOTAL), A
    cp sectorsToLoad
    jp z, _bootloader_done  ; all sectors loaded

    ; increment sector number
    ld A, (DAT_CURRENT_SECTOR)
    inc A
    ld (DAT_CURRENT_SECTOR), A
    cp 10     ; check if carry to next track is neccessary
    jp nz, _bootloader_loop

    ; carry over to next track
    ld A, 1
    ld (DAT_CURRENT_SECTOR), A
    ld A, (DAT_CURRENT_TRACK)
    inc A
    ld (DAT_CURRENT_TRACK), A
    call MINIMON_SEEK
    jp c, bootFail  ; no use in retrying seek operation
    jp _bootloader_loop
    


_bootloader_done:
    call MINIMON_CLS
    jp boot ; give control to bios cold boot routine



bootFail:
    ld HL, str_bootloaderFail
    call MINIMON_PRINTSTRING
_bootFail_hang:
    jp _bootFail_hang



;----------------- STRINGS -------------------

str_loadingCPM:
    db 'LOADING CP/M 2.2 ...', $0A, $00

str_bootloaderFail:
    db 'BOOTLOADER FAIL', $0A, $00

;--------------- DATA AREAS ------------------

DAT_SECTORS_LOADED_TOTAL:
    db 0

DAT_CURRENT_SECTOR:
    db 2

DAT_CURRENT_TRACK:
    db 0

;---------------------------------------------

    ; pad out file for 512 bytes - 2 signature bytes
    dc 510 - ($ - $2200), $00

    dw $BEEF  ; boot signature

    end
    
    