
;---------------------------------
;       Hex input command
;---------------------------------

; Reads Intel HEX from the terminal until end record is found.
;  No arguments.

command_input_hex:
    ld HL, str_readingHex
    call printString

_hex_record_start:
    ld D, $00  ; initialize D for checksum calculation
    ; wait until start code is read
_hex_waitForStart:
    call readChar
    cp ':'
    jp nz, _hex_waitForStart
    
    ; byte count
    call readHex
    jp c, _hex_error
    ld C, A        ; store byte count in C
    add D  ; add to checksum
    ld D, A

    ; address. store it in HL
    call readHex
    jp c, _hex_error
    ld H, A
    add D  ; add to checksum
    ld D, A
    call readHex
    jp c, _hex_error
    ld L, A
    add D  ; add to checksum
    ld D, A

    ; record type
    call readHex
    jp c, _hex_error
    ld B, A  ; buffer in B
    add D  ; add to checksum
    ld D, A
    ld A, B ; restore
    cp $00
    jp z, _hex_data
    cp $01
    jp z, _hex_eof
    jp _hex_error ; unrecognized record type


_hex_data:
    
    ld A, C ; check if byte count was zero
    or A
_hex_data_loop:
    jp z, _hex_data_done

    call readHex
    jp c, _hex_error
    ld (HL), A  ; store byte
    add D       ; add byte to D for checksum calculation
    ld D, A

    dec C
    inc HL

    jp _hex_data_loop


_hex_data_done:
    ; read checksum and add D to it
    call readHex
    jp c, _hex_error
    add D   ; if record/checksum is okay, this must yield zero
    jp nz, _hex_checksum_error  

    ; expect CR or LF line terminator
    call readChar
    cp TERM_LF
    jp z, _hex_record_start
    cp TERM_CR
    jp z, _hex_record_start ; got CR. there's likely a LF following, which will get caught when waiting for :

    jp _hex_error


_hex_eof:
    ; expect checksum $ff
    call readHex
    cp $ff
    jp nz, _hex_checksum_error
    
    ; got expected checksum. we're done reading hex file
    ld HL, str_hexOK
    call printString

    call monitor_waitForSpace ; just in case any stry records were appended

    jp monitorPrompt_loop


_hex_checksum_error:
    ld HL, str_hexChecksumError
    call printString

    ; wait for user to hit space and discard all other chars.
    ;  keeps hex garble out of command prompt
    call monitor_waitForSpace

    jp monitorPrompt_loop

_hex_error:
    ld HL, str_hexError
    call printString

    ; wait for user to hit space and discard all other chars.
    ;  keeps hex garble out of command prompt
    call monitor_waitForSpace

    jp monitorPrompt_loop


