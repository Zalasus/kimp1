
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
    or E
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
    ; we have printed 16 chars. print ascii section, a linefeed and a new address token
    call _command_examine_printAscii
    call printNewLine
    call printAddressToken
    ld C, CONF_COMM_EXAMINE_BYTES_PER_LINE + 1
    jp _command_examine_loop
    
_command_examine_end:
    ; we are done. print ASCII section and go back to monitor
    dec C  ; due to the condition structure above, C is actually one to high when we end up here. fix that
    call _command_examine_printAscii
    jp monitorPrompt_loop



; Subroutine to print ASCII section of hexdump
_command_examine_printAscii:
    ; pad out hex section with 3*C spaces
    push BC  ; save BC. we need it to check how many bytes HL needs to go back

_command_examine_padLoop:
    ld A, C
    or A
    jp z, _command_examine_padLoopEnd
    
    ld A, ' '
    call printChar
    call printChar
    call printChar
    
    dec C

    jp _command_examine_padLoop

_command_examine_padLoopEnd:
    ld A, ' '      ; print two spaces to seperate hex and ascii
    call printChar
    call printChar

    pop BC
    ld A, CONF_COMM_EXAMINE_BYTES_PER_LINE
    sub C  
    ld C, A  ; C now contains the amount of bytes we printed in the hex section

    ; subtract C from HL. have to do it this way since we have no pairs to spare
    ld A, L
    sub C
    ld L, A
    ld A, H
    sbc 0
    ld H, A
    
    ; HL is now where it was at the start of the hex line
_command_examine_asciiLoop:
    ld A, C
    or A
    ret z  ; printed all ascii chars? we are finished
    
    ld A, (HL)
    call printPrintable
    
    inc HL
    dec C

    jp _command_examine_asciiLoop
    
    
    


