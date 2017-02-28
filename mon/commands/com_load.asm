

; Loads the first record on tape into memory at given address
command_load:
    call expression ; destination address
    jp c, monitor_syntaxError

    ; first, we need to set up the PIT C0 to count the time between two zero
    ; crossings in the tape signal

    ld HL, str_pressPlayOnTape
    call printString
_command_load_waitForTape:
    in A,(IO_TCCR)
    and [1 << BIT_TCCR_TAPE_SENSE] ; mask out SENSE bit (active low!)
    jp nz,_command_load_waitForTape ; wait until user pushes play button
    
    ; user pushed play button. print loading message
    ld HL, str_loading
    call printString
    
    in A,(IO_TCCR) ; set MOTOR bit in TCR
    and IO_TCCR_WRITE_MASK ; mask out all the bits that are not readable
    or [1 << BIT_TCCR_TAPE_MOTOR]
    out (IO_TCCR), A
    
    ; motor is now running, now we can start to read bits from tape

_command_load_tapeLoop:
    ; first, read the current tape state
    in A,(IO_TCCR)
    and [1 << BIT_TCCR_TAPE_DATA_READ] ; mask out tape data bit...
    
    ; second, compare it with the previous state
    sub B
    jp p, _command_load_tapeLoop 
    
    ; now we store the current state
    ld C,A
    
    
    xor B

    
    ;......

    ; turn off motor
    in A, (IO_TCCR)
    and IO_TCCR_WRITE_MASK & ~[1 << BIT_TCCR_TAPE_MOTOR]
    out (IO_TCCR), A
    
    jp monitorPrompt_loop ; jump back to monitor loop


