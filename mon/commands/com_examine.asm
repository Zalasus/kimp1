

; Prints contents of memory location given by parameter
;  (may be an address range)
command_examine:
    call expression
    jp c, monitor_syntaxError
    call skipWhites
    push DE
    
    ld A, C
    or A  ; compare with zero
    jp z, _command_examine_print ; no more arguments -> start printing

    ld A, (HL) ; load remaining char
    cp MON_ARGUMENT_SEPERATOR
    jp nz, monitor_syntaxError ; remaining char is not , -> error
    
    inc HL ; move pointer to next byte
    dec C
    call skipWhites
    
    call expression
    jp c, monitor_syntaxError
    
_command_examine_print:
    inc DE ; since we want the upper address to be inclusive
    
    pop HL
    ; start address is now stored in HL, end address in DE
    
    ld C, CONF_COMM_EXAMINE_BYTES_PER_LINE + 1 ; counter for bytes on line (to insert LF after 16 bytes)

    ; print starting address token
    call printAddressToken

    ; if end address was ffff (is now 0) we need to skip the first loop check
    ld A, D
    or A
    jp nz, _command_examine_loop
    ld A, E
    or A
    jp z, _command_examine_cont1
    
_command_examine_loop:
    ; reached end address yet?
    ld A, H
    cp D
    jp nz, _command_examine_cont1
    ld A, L
    cp E
    jp z, _command_examine_end
_command_examine_cont1:
    dec C
    jp z, _command_examine_lf
_command_examine_cont2:

    ; check if user terminated printing
    call hasChar
    or A
    jp nz, _command_examine_end

    ld A, (HL)
    call printHex
    
    ld A, TERM_SPACE
    call printChar
    
    inc HL
    
    jp _command_examine_loop
    
_command_examine_lf:
    ; we have printed 16 chars. print a linefeed and a new address token
    call printNewLine
    call printAddressToken
    ld C, CONF_COMM_EXAMINE_BYTES_PER_LINE + 1
    jp _command_examine_loop
    
_command_examine_end:

    jp monitorPrompt_loop


