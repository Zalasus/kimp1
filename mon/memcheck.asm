memCheck:
    ld HL, ROM_END
    ld B, $FF
    
memCheck_loop:
    ld (HL), B
    ld A, (HL)
    cp B
    jp NZ memCheck_error
    inc HL
    
    ; we are done when HL goes zero
    ld A, $00 
    cp H
    jp NZ, memCheck_loop
    cp L
    jp NZ, memCheck_loop
    
    ; high memory check successful. now we load the LOMEM checker into HIMEM
    ld HL, loMemCheck
    ld DE, ROM_END
    ld BC, loMemCheck_end - loMemCheck
    ldir
    ; lomem checker routine is stored in HIMEM now.
    ; jump to the loaded routine.
    jp phase_loMemCheck
    ; the routine jumps to the right handler after the check
    
memCheck_error:
    ; there was a fatal memory error. try to print error message and halt
    ld HL, str_memError
    call printString
    hlt
    jp memCheck
    

; this routine is loaded into HIMEM by the memory check routine in order to
; check LOMEM
loMemCheck: 
phase ROM_END ; thanks to phase, this routine now lies in RAM(HIMEM)
phase_loMemCheck:
    ld A, (1 << BIT_ROM_GATE) ; turn off the ROM mapping
    out (IO_TCCR),A ; LOMEM is now accessible

    ld HL, 0
    
phase_loMemCheck_loop:
    ld (HL), B
    ld A, (HL)
    cp B
    jp NZ phase_loMemCheck_error
    inc HL
    ; we are done when HL goes ROM_END
    ld A, high ROM_END
    cp H
    jp NZ, phase_loMemCheck_loop
    ld A, low ROM_END
    cp L
    jp NZ, phase_loMemCheck_loop
    
    ; no errors. turn ROM mapping back on. check ends here
    ld A, 0
    out (IO_TCCR), A ; ROM mapping is now back on
    jp memCheck_ok
    
phase_loMemCheck_error:
    ld A, 0
    out (IO_TCCR), A ; ROM mapping is now back on
    jp memCheck_error
    
dephase
loMemCheck:

memCheck_ok:
; when we end up here, memory is okay