

; Used to enter an arbitrary amount of bytes starting from a given 
;  address (first argument). Accepts only hex chars.
command_store:
    call expression
    jp c, monitor_syntaxError
    ex DE, HL
    
    ld C, 16 ; counter for bytes on line (to insert LF after 16 bytes)

    ; print starting address token
    call printAddressToken
    
_command_store_loop:
    call readChar

    cp TERM_CR ; pressed return?
    jp z, monitorPrompt_loop ; yes -> we are done

    cp MON_STRING
    jp z, _command_store_stringMode

    ld B, A
    call parseHex
    cp $ff
    jp z, _command_store_loop ; char was not valid. read again
    ; char was valid hex. echo it
    push AF
    ld A, B
    call printChar
    pop AF
    
    ; shift left by 4, store in D and proceed
    ld D, A
    sla D
    sla D
    sla D
    sla D
    
_command_store_lowerNibble_loop:
    ; read another nibble
    call readChar

    cp TERM_CR ; pressed return?
    jp z, monitorPrompt_loop ; yepp -> we are done

    ; Backspace? (only allow to go back to high nibble for now. going back whole bytes will be tricky)
    cp TERM_BS
    jp z, _command_store_bs
    cp TERM_DEL
    jp z, _command_store_bs

    ld B, A
    call parseHex
    cp $ff
    jp z, _command_store_lowerNibble_loop ; char was not valid. read again
    ; char was valid hex. echo it
    push AF
    ld A, B
    call printChar
    pop AF
    
    or D ; insert D into A
    ; A now contains the whole entered byte
    
    ld (HL), A ; store byte
    inc HL
    
    ld A, TERM_SPACE
    call printChar
    
    dec C
    jp nz, _command_store_loop  ; no LF needed
    
    ; we have printed 16 chars. print a linefeed and a new address token
    call printNewLine
    ld C, 16
    
    ; print address token
    call printAddressToken

    jp _command_store_loop


_command_store_bs:
    ld A, TERM_BS
    call printChar
    jp _command_store_loop



_command_store_stringMode:
    ; print initial string token to signalize we are in string mode
    ld A, TERM_BS
    call printChar
    ld A, MON_STRING
    call printChar
    
_command_store_stringLoop:
    call readChar
    
    cp TERM_CR
    jp z, monitorPrompt_loop

    cp MON_STRING
    jp z, _command_store_endString

    ; no control character. store char
    ld (HL), A
    inc HL

    ; now echo char if it is printable.
    ;  cheap check is to look if one of the bits 5,6 or 7 is set.
    ;  (just need to catch DEL char)
    cp TERM_DEL
    jp z, _command_store_charNotPrint
    ld B, A
    and $E0
    ld A, B
    jp nz, _command_store_charPrint
_command_store_charNotPrint:
    ; char is not printable. replace with space
    ld A, TERM_SPACE
_command_store_charPrint:
    call printChar
    ld A, TERM_SPACE
    call printChar
    call printChar

    dec C
    jp nz, _command_store_stringLoop  ; no LF needed
    
    ; we have printed 16 chars. print a linefeed and a new address token
    call printNewLine
    ld C, 16
    
    ; print address token
    call printAddressToken

    jp _command_store_stringLoop

_command_store_endString:
    ; print string end token and go back to loop
    ld A, TERM_BS
    call printChar
    ld A, MON_STRING
    call printChar
    jp _command_store_loop




