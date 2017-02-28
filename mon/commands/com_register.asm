

; Prints the saved register buffer in human readable format or
;  modifies it.
;  NOTE: Quick and dirty routine. Surely to be compacted somehow.
command_register:
    call skipWhites
    ld A, C
    or A
    jp z, _command_register_print

    ; user supplied arguments -> modify stash
_command_register_modLoop:
    ld B, (HL) 
    inc HL
    dec C
    call skipWhites
    ld A, (HL)
    cp '='
    jp nz, monitor_syntaxError
    inc HL
    dec C
    ld A, B

    ; determine which register to modify
    ld IX, DAT_MON_REG_BUFFER+1
    cp 'A'
    jr z, _command_register_mod8

    inc IX
    cp 'C'
    jr z, _command_register_mod8

    inc IX
    cp 'B'
    jr z, _command_register_mod8

    inc IX
    cp 'E'
    jr z, _command_register_mod8

    inc IX
    cp 'D'
    jr z, _command_register_mod8

    inc IX
    cp 'L'
    jr z, _command_register_mod8

    inc IX
    cp 'H'
    jr z, _command_register_mod8

    ld IX, DAT_MON_REG_BUFFER+10
    cp 'X'
    jr z, _command_register_mod16

    ld IX, DAT_MON_REG_BUFFER+12
    cp 'Y'
    jr z, _command_register_mod16
    
    ld IX, DAT_MON_REG_BUFFER+14
    cp 'I'
    jr z, _command_register_mod8

    jp monitor_syntaxError

_command_register_mod8:
    call expression
    jp c, monitor_syntaxError
    ld (IX+0), E
    jp _command_register_modLoopCheck

_command_register_mod16:
    call expression
    jp c, monitor_syntaxError
    ld (IX+0), E
    ld (IX+1), D

_command_register_modLoopCheck:
    call skipWhites
    ld A, C
    or A
    jp nz, _command_register_modLoop
    ;jp monitorPrompt_loop
    ; commented so reg stash gets printed after mods are made

_command_register_print:
    ld IX, DAT_MON_REG_BUFFER
    
    ; F: NZ NC PO P
    ; A: 00 I: 00
    ; BC: 0000
    ; DE: 0000
    ; HL: 0000
    ; SP: 0000
    ; IX: 0000 IY: 0000

    ; NOTE: this bit assignment might not be portable
    ld E, 'F'
    call __command_register_rn8
    ld C, (IX + 0)

    ld A, C
    ld D, 'C'
    bit 0, A
    call __command_register_fs

    ld A, C
    ld D, 'N'
    bit 1, A
    call __command_register_fs

    ld A, C
    ld D, 'P'
    bit 2, A
    call __command_register_fs

    ld A, C
    ld D, 'H'
    bit 4, A
    call __command_register_fs

    ld A, C
    ld D, 'Z'
    bit 6, A
    call __command_register_fs

    ld A, C
    ld D, 'S'
    bit 7, A
    call __command_register_fs

    call printNewLine

    ld E, 'A'
    call __command_register_rn8
    ld A, (IX + 1)
    call printHex
    ld A, TERM_SPACE
    call printChar

    ld E, 'I'
    call __command_register_rn8
    ld A, (IX + 14)
    call printHex
    call printNewLine

    ld D, 'B'
    ld E, 'C'
    call __command_register_rn16
    ld A, (IX + 3)
    call printHex
    ld A, (IX + 2)
    call printHex
    call printNewLine

    ld D, 'D'
    ld E, 'E'
    call __command_register_rn16
    ld A, (IX + 5)
    call printHex
    ld A, (IX + 4)
    call printHex
    call printNewLine

    ld D, 'H'
    ld E, 'L'
    call __command_register_rn16
    ld A, (IX + 7)
    call printHex
    ld A, (IX + 6)
    call printHex
    call printNewLine

    ld D, 'S'
    ld E, 'P'
    call __command_register_rn16
    ld A, (IX + 9) ; LE!!
    call printHex
    ld A, (IX + 8)
    call printHex
    call printNewLine

    ld D, 'I'
    ld E, 'X'
    call __command_register_rn16
    ld A, (IX + 11)
    call printHex
    ld A, (IX + 10)
    call printHex
    ld A, TERM_SPACE
    call printChar
    
    ld D, 'I'
    ld E, 'Y'
    call __command_register_rn16
    ld A, (IX + 13)
    call printHex
    ld A, (IX + 12)
    call printHex
    call printNewLine

    jp monitorPrompt_loop
    
__command_register_rn16:
    ld A, D
    call printChar
__command_register_rn8:
    ld A, E
    call printChar
    ld A, ':'
    call printChar
    ld A, TERM_SPACE
    call printChar   
    ret

__command_register_fs:
    ; D = flag name, prefixed with N if reset
    jp nz, __command_register_fs_cont
    ld A, 'N'
    call printChar
__command_register_fs_cont:
    ld A, D
    call printChar
    ld A, TERM_SPACE
    call printChar
    ret



