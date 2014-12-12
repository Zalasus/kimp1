
;-----------------------------------
;   CASSETTE TAPE MONITOR PROGRAM
;           VERSION 1.00
;
;       for the KIMP1 system
;
;    (C) 1979 Knifto Industries
;-----------------------------------

z80
name 'CTMPv10'


; include definition file for KIMP1 computer
include kimp1def.inc


UART_BAUDRATE equ 9600
UART_PRESCALE equ 1
UART_DIV_VAL equ (CPU_SPEED/UART_PRESCALE)/UART_BAUDRATE - 1

TERM_LF equ $0A

MON_PROMPT equ $3E ; the '>' char

MON_COM_RUN equ $72 ; the 'r' character
MON_COM_LOAD equ $6C ; the 'l' character
MON_COM_VERSION equ $76 ; the 'v' character

org $0000

ld HL, FFFFh ; init stackpointer to end of memory
ld SP,HL

jp monitorStart ; jump to monitor setup


str_welcome:
    db 'KIMP1 CASSETTE TAPE MONITOR PROGRAM 1.0', $0A, $0D, $00

str_pressPlayOnTape:
    db 'PRESS PLAY ON TAPE', $0A, $0D, $00
    
str_unknownCommand:
    db 'UNKNOWN COMMAND', $0A, $0D, $00
    
str_loading:
    db 'LOADING. PLEASE WAIT...', $0A, $0D, $00
    
; prints the character stored in A. destructive to contents of B
printChar:
    ld B,A
    in A, (IO_UART_COM) ; read in status byte of UART
    and $01 ; mask out all bits except the TXRDY bit
    jp Z,printChar ; do this until UART is ready
    
    ; UART is ready to send another byte now
    out (IO_UART_DAT), B ; UART will start sending the byte now
    
    ret
    
; prints all characters from HL to the next zero byte
printString:
    ld A, (HL) ; fetch byte
    tst A
    ret Z ; if byte is zero, we are done
    
    ;byte is not zero, so print it out
    call printChar
    
    inc HL ; increment HL and loop
    jp printString
    

; prints CRLF characters
printNewLine:
    ld A, $0A
    call printString
    ld A, $0D
    call printString
    
    ret
    
    
; reads one character from the UART and stores it in A
readChar:
    in A, (IO_UART_COM) ; read in status byte of UART
    and $02 ; mask out all bits except the RXRDY bit
    jp Z,readChar ; do this until UART has a valid byte
    
    in A,(IO_UART_DAT) ; read in data byte
    
if TERM_ENABLE_ECHO
    call printChar
endif
    
    ret
    

; Reads characters from the UART and stores them in HL. Reading continues until
; 256 characters have been read or a LF character is found. the amount of bytes
; read is stored in the C register. If the read was aborted after 256 chars, 
; the C register contains 0. HL points to the first read byte after the call.
readString:
    ld C, $00
    push HL ; save HL on stack
readString_loop:
    call readChar
    ld (HL), A
    
    inc HL
    
    inc C
    jp Z, readString_end ; return if C has flown over (256 chars read)
    
    cp TERM_LF
    jp NZ, readString_loop ; loop, if read character was not TERM_LF

readString_end:
    pop HL ; restore HL
    ret ; character was TERM_LF, so return
    

    
monitorStart:
    ; inititalize CCR and perform an IO-RESET before intitalizing peripherals
    ld A, (1 << BIT_IO_RESET); set only IO-RESET bit to one
    out (IO_CCR),A
    nop ; keep the IO-RESET line high for at least 6 clock pulses
    nop
    nop
    ld A, 0 ; clear IO-RESET bit
    out (IO_CCR),A
    ; CCR is now intitalized and IO-Devices are reset
    
    ; PIT 2 is connected to UART, so set it up for baud rate generation
    ld A, $84 ;10000100  set counter 2 in mode 2, binary counting
    out (IO_PIT_CTRL), A
    ; write divider value to counter
    ld A, high UART_DIV_VAL
    ld B, low UART_DIV_VAL
    out (IO_PIT_C2), A
    out (IO_PIT_C2), B
    ; registers for counter are set. now we can gate the counter
    in A,(IO_CCR)
    or (1 << BIT_C2_GATE)
    out (IO_CCR), A ; C2 is now counting
    
    ; write mode byte to UART (first command byte after reset)
    ld A, $4D; 01001110  8 data bits, 1 stop bit, no parity, 1 times prescaler
    out (IO_UART_COM),A 
    ; enable receiver and transmitter
    ld A, (1 << BIT_TXEN) | (1 << BIT_RXEN)
    out (IO_UART_COM), A
    
    ; reset TCC
    xor A,A ; set A to zero
    out (IO_TCC),A
    
    ; IO-Devices are now initialized
    
monitor_welcome:
    ld HL, str_welcome ; print welcome message
    call printString
    
monitorPromt_loop:
    ld A, MON_PROMPT ; print input promt
    call printChar
    
    ld HL, ROM_END ; this is where we want to store the read bytes
    call readString ; read user input
    
    ; process user input
    ld A,(HL) ; load fist byte entered
    inc HL ; move HL to next byte
    dec C
    
    ; determine entered command
    cp MON_COM_RUN
    jp Z, command_run
    
    cp MON_COM_LOAD
    jp Z, command_load
    
    cp MON_COM_BOOT
    jp Z, command_boot
    
    cp MON_COM_VERSION
    jp Z, monitor_welcome ; this command just prints out the welcome msg again
    
    ; no command character recognized. print error message
    ld HL, str_unknownCommand
    call printString
    
    jp monitorPromt_loop
    
; parses a single hex character at (HL), stores result in A. 
; A = $FF means error
parseHex:
    ld A, (HL)
    cp $30
    jp S, parseHex_noDigit ; char is < '0'
    cp $3A
    jp NS, parseHex_noDigit ; char is > '9' 
    ; we now know the char is a digit
    sub $30 ; subtract the value of '0'
    ; hex value is now stored in A
    ret
    
parseHex_noDigit:
    cp $41 ; the ASCII-char 'A'
    jp S, parseHex_error ; char is < 'A'
    cp $47 ; the ASCII-char 'G'
    jp NS, parseHex_error ; char is > 'F'
    ; we now know the char is a hex letter
    sub $50 ; subtract the value of 'A' and add the 15 offset (as A means 15)
    ;hex value is now stored in A
    ret
    
parseHex_error:
    ld A, $FF
    ret
    
    
; parses four hex chars pointed by HL and stores result in DE. sets A to 0 if
; parsing succeeds. sets A to $FF if not or C is smaller than 1 upon calling.
parseHexWord:
    ld A, C
    cp 1
    jp S, parseHexWord_error ; not enough bytes for parsing a word
    
parseHexWord_loop:
    call parseHex
    cp $FF
    ret Z ; not a valid hex char -> return
    
    ; we have parsed a valid hex char
    ; TODO: insert the parsed char into the DE register and shift stuff
    
    inc HL
    dec C
    ret Z ; no bytes remaining -> return
    
    jp parseHexWord_loop
    
parseHexWord_error:
    ld A, $FF
    ret
    
;--------------------Monitor command definition area----------------------
; NOTE: These are not CALL-ed! 
; In the end of each command, simply jump back to monitorPromt_loop


; monitor command to jump to given location
command_run:
    ; store the hexadecimal ASCII-coded number at (HL) in the DE register pair
    call parseHexWord 
    
    ld HL, DE
    jp (HL) ; we are leaving the monitor here. no need to jump back to loop
    
; monitor command that loads the first record on tape into memory
command_load:
    ; first, we need to set up the PIT C0 to count the time between two zero
    ; crossings in the tape signal
    ld A, $
    

    ld HL, str_pressPlayOnTape
    call printString
command_load_waitForTape:
    in A,(IO_TCR)
    and $01
    jp NZ,command_load_waitForTape ; wait until user pushes play button
    
    ; user pushed play button. print loading message
    ld HL, str_loading
    call printString
    
    ld A, $80 ; set MOTOR bit in TCR
    out A,(IO_TCR)
    
    ; motor is now running, now we can start to read bits from the tape


    ;......
    
    jp monitorPromt_loop ; jump back to monitor loop

; monitor command that loads the first record on tape to the first available 
; memory location and gives control to the loaded program. requires no args.
command_boot:



org ROM_END
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    