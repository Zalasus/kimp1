
;-------------------------------
;       Examine command
;-------------------------------

; Prints contents of memory location given by argument 1.
;  Optional argument 2 is amount of bytes to print. Default is 1 byte.

command_examine:
    call expression
    jp c, monitor_syntaxError
    call skipWhites
    push DE

    ld DE, 1  ; default is to print 1 byte

    ld A, (HL)
    or A
    jp z, _command_examine_print  ; no more arguments. start printing

    ; characters remaining. check for second argument
    cp MON_ARGUMENT_SEPERATOR
    jp nz, monitor_syntaxError ; remaining char is not , -> error

    inc HL
    call skipWhites
    
    call expression
    jp c, monitor_syntaxError
    
_command_examine_print:
    pop HL
    ; start address is now stored in HL, byte count in DE
    
    ld C, CONF_COMM_EXAMINE_BYTES_PER_LINE + 1 ; counter for bytes on line (to insert LF after 16 bytes)

    ; print starting address token
    call printAddressToken

_command_examine_loop:
    ; byte count hit zero?
    ld A, D
    or E
    jp z, _command_examine_end

    ; decrement count of bytes on line (do this here so we don't print address after last byte)
    dec C
    jp z, _command_examine_lf

    ; check if user terminated printing
    call hasChar
    or A
    jp nz, _command_examine_end

    ld A, (HL)
    call printHex
    
    ld A, TERM_SPACE
    call printChar
    
    inc HL
    dec DE
    
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
    
    
    


