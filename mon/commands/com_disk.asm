
;------------------------------
;        Disk command
;------------------------------

; Tool with options to read and write sectors and to format
;  floppy disks. Needs extension board.

command_disk:
    ld A, (DAT_EXT_INITIALIZED)
    cp $ff
    jp nz, _command_disk_error_extNotInitialized

    call skipWhites

    ld A, (HL)
    or A
    jp z, monitor_syntaxError  ; if no arguments supplied

    inc HL
    dec C

    cp 'h'
    jp z, _command_disk_help

    cp 'i'
    jp z, _command_disk_info
   
    cp 'd'
    jp z, _command_disk_selDisk

    cp 't'
    jp z, _command_disk_selTrack

    cp 's'
    jp z, _command_disk_selSector

    cp 'r'
    jp z, _command_disk_read

    cp 'w'
    jp z, _command_disk_write

    cp 'f'
    jp z, _command_disk_format

    jp monitor_syntaxError



_command_disk_help:
    ld HL, str_disktoolHelp
    call printString
    jp monitorPrompt_loop



_command_disk_info:
    ld HL, str_disktoolInfo
    ld DE, DAT_DISK_NUMBER
    ld C, 5

_command_disk_info_loop:
    call printString
    inc HL ; skip null terminator
    ld A, '='
    call printChar
    ld A, (DE)
    call printHex
    inc DE
    ld A, TERM_SPACE
    call printChar

    dec C
    jp nz, _command_disk_info_loop

    call printNewLine

    ld HL, str_disktoolDiskchange
    call printString
    call fdc_getDiskChange
    call printHex

    call printNewLine

    jp monitorPrompt_loop



_command_disk_selDisk:
    call expression
    jp c, monitor_syntaxError
    ld A, E
    ld (DAT_DISK_NUMBER), A
    call fdc_specify
    jp c, _command_disk_error
    call fdc_recalibrate
    jp c, _command_disk_error
    jp monitorPrompt_loop



_command_disk_selTrack:
    call expression
    jp c, monitor_syntaxError
    ld A, E
    ld (DAT_DISK_TRACK), A
    call fdc_seek
    jp c, _command_disk_error
    jp monitorPrompt_loop



_command_disk_selSector:
    call expression
    jp c, monitor_syntaxError
    ld A, E
    ld (DAT_DISK_SECTOR), A
    jp monitorPrompt_loop



_command_disk_read:
    ld DE, fdc_readData
    ld (DAT_DISK_COMMAND), DE
    jp _command_disk_rw



_command_disk_write:
    ld DE, fdc_writeData
    ld (DAT_DISK_COMMAND), DE
    jp _command_disk_rw



_command_disk_rw:
    call expression
    jp c, monitor_syntaxError
    ld (DAT_DISK_DATAPTR), DE

    ; did user provide second argument? -> block transfer
    call skipWhites
    ld A, (HL)
    cp MON_ARGUMENT_SEPERATOR
    jp z, _command_disk_rw_block

    ; nope, only transfer single sector
    ld DE, $0001
    jp _command_disk_rw_loop

_command_disk_rw_block:
    inc HL
    dec C
    call expression   ; parse sector count, stored in DE for the rest of the loop
    jp c, monitor_syntaxError
    
_command_disk_rw_loop:
    ; is sector count 0?
    ld A, D
    or E
    jp z, monitorPrompt_loop  ; we are done

    ; perform read/write
    ld HL, (DAT_DISK_COMMAND)
    call fdc_commandWithRetry
    jp c, _command_disk_error

    ; decrement sector count and increment target address
    dec DE  ; one sector transferred. decrement count
    ld HL, (DAT_DISK_DATAPTR)
    ld A, L
    add low CONF_DISK_BYTES_PER_SECTOR
    ld L, A
    ld A, H
    adc high CONF_DISK_BYTES_PER_SECTOR
    ld H, A
    ld (DAT_DISK_DATAPTR), HL

    ; increment sector number
    ld A, (DAT_DISK_SECTOR)
    inc A
    ld (DAT_DISK_SECTOR), A
    cp FDC_PARAM_SC+1  ; check if carry to next track is neccessary
    jp nz, _command_disk_rw_loop
    
    ; carry to next track
    ld A, 1               
    ld (DAT_DISK_SECTOR), A
    ld A, (DAT_DISK_TRACK)
    inc A
    ld (DAT_DISK_TRACK), A
    call fdc_seek
    jp c, _command_disk_error
    jp _command_disk_rw_loop



_command_disk_format:
    ld A, $E5  ; default filler for creating CP/M filesystems
    ld (DAT_DISK_FILLER), A

    call skipWhites  ; did user provide filler parameter?
    ld A, (HL)
    or A
    jp z, _command_disk_format_cont  ; no. continue

    call expression
    jp c, monitor_syntaxError
    ld A, E
    ld (DAT_DISK_FILLER), A

_command_disk_format_cont:
    xor A
    ld (DAT_DISK_TRACK), A
_command_disk_format_loop:
    call fdc_seek
    jp c, _command_disk_error

    call fdc_format       ; no need for retry wrapper. formatting will not fail with recoverable errors
    jp c, _command_disk_error

    ; increment track and check if last track reached
    ld A, (DAT_DISK_TRACK)
    inc A
    ld (DAT_DISK_TRACK), A
    cp CONF_DISK_TRACK_COUNT
    jp nz, _command_disk_format_loop
    dec A  ; track count is not the actual track we're on (0 based)
    ld (DAT_DISK_TRACK), A
    jp monitorPrompt_loop



_command_disk_error_extNotInitialized:
    ld HL, str_noExtPresent
    call printString
    jp monitorPrompt_loop



_command_disk_error:
    cp $01
    jp z, _command_disk_fdcError
    cp $02
    jp z, _command_disk_wpError
    ld HL, str_diskError
    call printString
    jp monitorPrompt_loop

_command_disk_fdcError:
    ld HL, str_fdcError
    call printString
    jp monitorPrompt_loop

_command_disk_wpError:
    ld HL, str_fdcWriteProtectedError
    call printString
    jp monitorPrompt_loop



