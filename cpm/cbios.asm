
;-----------------------------------
;        CBIOS FOR CP/M 2.2
;
;       for the KIMP1 system
;
;    (C) 2015 Knifto Industries
;        all wrongs reserved
;-----------------------------------

z80

    ; include definition file for KIMP1 computer
    include ../kimp1def.inc
    
    
    org bios

; CBIOS jump vector
    jp  boot        ;cold start
    jp  wboot       ;warm start
    jp  const       ;console status
    jp  conin       ;console character in
    jp  conout      ;console character out
    jp  list        ;list character out
    jp  punch       ;punch character out
    jp  reader      ;reader character out
    jp  home        ;move head to home position
    jp  seldsk      ;select disk
    jp  settrk      ;set track number
    jp  setsec      ;set sector number
    jp  setdma      ;set dma address
    jp  read        ;read disk
    jp  write       ;write disk
    jp  listst      ;return list status
    jp  sectran     ;sector translate

    
    
    
;------------------------CP/M BIOS ROUTINES----------------------------
    
boot:
    in A,(IO_TCCR)
    and IO_TCCR_WRITE_MASK
    or [1 << BIT_ROM_GATE]
    out (IO_TCCR),A ; disable rom mapping

    ld HL, str_welcome
    call printString
    jp gocpm
    
    
    
    
; sets A to $FF if console has readable byte, $00 if not
const:
    in A, (IO_UART_COM) ; read in status byte of UART
    and $02 ; mask out all bits except the RXRDY bit
    jp Z,_const_notReady ; bit is zero -> not ready
    ld A,$ff
    ret
_const_notReady:
    xor A ; set A to 0
    ret

    
; prints character in C to console
conout:
    in A, (IO_UART_COM) ; read in status byte of UART
    and $01 ; mask out all bits except the TXRDY bit
    jp Z,printChar ; do this until UART is ready
    
    ; UART is ready to send another byte now
    ld A,C
    out (IO_UART_DAT), A ; UART will start sending the byte now
    
    ret
    
; reads character from console into A
conin:
    in A, (IO_UART_COM) ; read in status byte of UART
    and $02 ; mask out all bits except the RXRDY bit
    jp Z,readChar ; do this until UART has a valid byte
    
    in A,(IO_UART_DAT) ; read in data byte
    
    ret
    
;----------------------------------------------------------------------
    
    
; prints CRLF characters
printNewLine:
    ld A, $0A
    call printChar
    ld A, $0D
    call printChar
    ret
    
printString:
    ld A, (HL) ; fetch byte
    cp 0
    ret Z ; if byte is zero, we are done
    
    ;byte is not zero, so print it out
    ld C,A
    call conout
    
    inc HL ; increment HL and loop
    jp printString
    
    
    
str_welcome:
    db 'CP/M 2.2 FOR THE KIMP1', $0D, $0A, '(C) 1979 DIGITAL RESEARCH'
    db $0D, $0A, $00
    
    
    
    
    
    
    
    
    
    
    