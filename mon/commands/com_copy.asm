


; Copies block of memory
command_copy:
    call expression ; parse source address
    jp c, monitor_syntaxError
    call skipWhites
    push DE
    
    ld A, (HL) ; load remaining char
    cp MON_ARGUMENT_SEPERATOR
    jp nz, monitor_syntaxError ; remaining char is not , -> error
    inc HL
    dec C
    call skipWhites
    
    call expression ; parse destination address
    jp c, monitor_syntaxError
    call skipWhites
    push DE
    
    ld A, (HL) ; load remaining char
    cp MON_ARGUMENT_SEPERATOR
    jp nz, monitor_syntaxError ; remaining char is not , -> error
    inc HL
    dec C
    call skipWhites
    
    call expression ; parse byte count
    jp c, monitor_syntaxError
    ld B, D ; store count in byte counter
    ld C, E
    pop DE
    pop HL
    
    ldir ; start copying

    jp monitorPrompt_loop


