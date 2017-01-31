
;-------------------------- CONSOLE IO --------------------------------

; Prints the character stored in A. Trashes B.
printChar:
conout:
    ld B, A
_printChar_wait:
    in A, (IO_UART_COM) ; read in status byte of UART
    and [1 << BIT_UART_TXRDY] ; mask out all bits except the TXRDY bit
    jp z, _printChar_wait ; do this until UART is ready
    
    ; UART is ready to send another byte now
    ld A, B
    out (IO_UART_DAT), A ; UART will start sending the byte now
    
    ret
    
    
    
; Reads one character from the UART and stores it in A. The char is not echoed.
;  Blocking call. This method will wait until UART has received a byte
readChar:
conin:
    in A, (IO_UART_COM) ; read in status byte of UART
    and [1 << BIT_UART_RXRDY] ; mask out all bits except the RXRDY bit
    jp z, readChar ; do this until UART has a valid byte
    
    in A, (IO_UART_DAT) ; read in data byte
    
    ret
    


; Checks whether UART holds a character that is ready to be read.
;  Sets A to $ff if char is available, $00 if not.
hasChar:
const:
    in A, (IO_UART_COM) ; read in status byte of UART
    and [1 << BIT_UART_RXRDY] ; mask out all bits except the RXRDY bit
    jp z, _hasChar_no
    ld A, $ff
    ret
_hasChar_no:
    xor A
    ret
    


; Prints all characters from (HL) to the next zero byte.
;  Replaces all LFs ($0A) with CRLF ($0D, $0A)
printString:
    ld A, (HL) ; fetch byte
    or A  ; compare with zero
    ret Z ; if byte is zero, we are done
    
    cp TERM_LF ; if byte is LF, we need to go to next line
    jp z, _printString_lf
    
    ; byte is neither zero not LF, so print it out
    call printChar
    
    inc HL ; increment HL and loop
    jp printString
    
_printString_lf:
    call printNewLine ; use nl routine for portability
    inc HL
    jp printString
    
    
    
; Reads characters from the UART and stores them in the location pointed by HL.
;  Reading continues until 255 characters have been read or a LF character is
;  found. CR also terminates the input. The amount of bytes read is stored in the 
;  C register. HL points to the first read byte after the call. The last byte of 
;  the string (not included in C) is set to 0. During reading, control characters like 
;  backspace are processed accordingly so the input resembles somewhat of a 
;  command prompt.
readString:
    ld C, 1 ; count one char more than actually read to account for terminator when looking for overflow
    push HL ; save HL on stack
_readString_loop:
    call readChar
    
    cp TERM_BS
    jp z, _readString_backspace
    cp TERM_DEL
    jp z, _readString_backspace
    cp TERM_LF
    jp z, _readString_end ; LF means we are done
    cp TERM_CR
    jp z, _readString_end ; CR means the same
    
    call printChar ; echo the entered character
    
    ld (HL), A
    
    inc HL
    inc C
    jp z, _readString_end ; return if C has flown over (255 chars read)
    
    jp _readString_loop

_readString_backspace:
    ld A, C
    cp 1 ; important! we have one more char in c than actually read!
    jp z, _readString_loop ; nothing to delete. get on with it.
    
    ; TODO: Do this with string. takes less calls and loads
    ld A, TERM_BS
    call printChar ; echo the BS to move cursor back one char...
    ld A, TERM_RUBOUT
    call printChar ; overwrite the last entered char with rubout character...
    ld A, TERM_BS
    call printChar ; and place cursor over the rubout char again
    
    dec C ; buffer minus one
    dec HL
    
    jp _readString_loop ; back to loop
    
_readString_end:
    dec C ; remove the additional char
    ld (HL),0 ; insert null terminator
    pop HL ; move HL back to start of buffer
    ret

    
    
; Prints CRLF characters
printNewLine:
    ld A, $0A
    call printChar
    ld A, $0D
    call printChar
    ret

   

; Clears the screen
clearScreen:
    ld HL, str_cls
    call printString
    ret

