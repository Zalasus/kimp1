

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
    jp nz, _command_store_noLf
    
    ; we have printed 16 chars. print a linefeed and a new address token
    call printNewLine
    ld C, 16
    
    ; print address token
    call printAddressToken
    
_command_store_noLf:
    jp _command_store_loop