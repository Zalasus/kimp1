

; Data definition utility for KIMP1
;
; writes C sectors from HL to disk B, starting
; at sector D and track E


    include ../mon/minimon_routines.inc


    org $2200

    ld (DAT_SOURCE_ADDRESS), HL
    ld A, C
    ld (DAT_SECTORS_TO_WRITE), A
    ld A, D
    ld (DAT_CURRENT_SECTOR), A
    ld A, E
    ld (DAT_CURRENT_TRACK), A

    ld A, B
    call MINIMON_SELECTDISK
    jp c, diskError

    ld A, (DAT_CURRENT_TRACK)
    call MINIMON_SEEK
    jp c, diskError

loop:
    ; calculate source address for write
    ld HL, (DAT_SOURCE_ADDRESS)
    ld A, (DAT_SECTORS_LOADED_TOTAL)
    add A   ; shift left sector count 1 bit
    add H   ; add to H <=> shift left 8 bits -> sector count times 512
    ld H, A
    ld A, (DAT_CURRENT_SECTOR)
    call MINIMON_WRITEDATA   ; this includes retries
    jp c, diskError

    ; increment total count and check if all are loaded
    ld A, (DAT_SECTORS_TO_WRITE)
    ld B, A
    ld A, (DAT_SECTORS_LOADED_TOTAL)
    inc A
    ld (DAT_SECTORS_LOADED_TOTAL), A
    cp B
    jp z, done  ; all sectors written

    ; increment sector number
    ld A, (DAT_CURRENT_SECTOR)
    inc A
    ld (DAT_CURRENT_SECTOR), A
    cp 19     ; check if carry to next track is neccessary
    jp nz, loop
    ld A, 1
    ld (DAT_CURRENT_SECTOR), A
    ld A, (DAT_CURRENT_TRACK)
    inc A
    ld (DAT_CURRENT_TRACK), A
    call MINIMON_SEEK
    jp c, diskError  ; no use in retrying seek operation
    jp loop
    


done:
    ld HL, str_ok
    call MINIMON_PRINTSTRING
    ret ; back to monitor



diskError:
    ld HL, str_diskError
    call MINIMON_PRINTSTRING
    ret ; back to monitor



;----------------- STRINGS -------------------

str_diskError:
    db 'DISK ERROR', $0A, $00

str_ok:
    db 'WRITE OK', $0A, $00

;--------------- DATA AREAS ------------------

DAT_SOURCE_ADDRESS:
    dw 0

DAT_SECTORS_TO_WRITE:
    db 0

DAT_SECTORS_LOADED_TOTAL:
    db 0

DAT_CURRENT_SECTOR:
    db 1

DAT_CURRENT_TRACK:
    db 0



