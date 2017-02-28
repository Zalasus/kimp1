

;---------------------------------
;
;  Hex output command for Minimon
;
;---------------------------------


command_output_hex:
    call expression
    jp c, monitor_syntaxError
    call skipWhites
    push DE
    
    ld A, C
    or A  ; compare with zero
    jp z, _ohex_go ; no more arguments. just print this one byte

    ld A, (HL) ; load remaining char
    cp MON_ARGUMENT_SEPERATOR
    jp nz, monitor_syntaxError ; remaining char is not , -> error
    
    inc HL ; move pointer to next byte
    dec C
    call skipWhites
    
    call expression
    jp c, monitor_syntaxError

_ohex_go:
    inc DE  ; to make end address inclusive
    
    pop HL

    ; start address HL, end address DE, C used for checksum

    ; check if range contains bytes. if user entered the same address twice, skip data record
    ;  and go straight to eof
    ld A, D
    cp H
    jp nz, _ohex_data
    ld A, E
    cp L
    jp z, _ohex_eof

_ohex_data:
    ld C, $00  ; initialize C for checksum

    ld A,':'
    call printChar
    
    ; calculate and cap byte count at $10 and print it
    push DE   ; save end address on stack~ we'll need it later
    ex DE, HL
    call subtractHLDE
    ex DE, HL
    ld A, D
    or A
    jp nz, _ohex_count_cap  ; if high byte is not zero, count is definetely greater than 16
    ld A, E
    and $f0
    jp nz, _ohex_count_cap
    ; D is 0 and E is less than $10 -> print E
    ld A, E
    jp _ohex_count_end
_ohex_count_cap:
    ; count was greater or equal 16. cap DE and print $10
    ld DE, $0010
    ld A, $10
_ohex_count_end:
    call printHex
    ; HL is now the starting address, DE the capped byte count. End address still on stack
    ld A, E ; add count to checksum
    add C
    ld C, A

    ; address
    ld A, H
    call printHex
    ld A, L
    call printHex
    
    ; add address to checksum
    ld A, C
    add H
    add L
    ld C, A

    ; record type ( no need to add to checksum since it's 0)
    xor A
    call printHex

_ohex_data_loop:
    ; print bytes while E is still > 0
    ld A, E
    or A
    jp z, _ohex_data_end

    ld A, (HL)
    call printHex
    ld A, (HL) ; add to checksum
    add C
    ld C, A

    inc HL
    dec E

    jp _ohex_data_loop

_ohex_data_end:
    ;  negate and print checksum and line terminator
    xor A
    sub C
    call printHex
    call printNewLine

    call hasChar  ; allow user to abort printing
    or A
    jp nz, monitorPrompt_loop

    ; restore end address and check if anything left to print
    pop DE
    ld A, D
    cp H
    jp nz, _ohex_data
    ld A, E
    cp L
    jp nz, _ohex_data

    ; nothing left -> write eof record
_ohex_eof:
    ld HL, str_hexRecordEof
    call printString

    jp monitorPrompt_loop



