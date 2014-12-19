
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

MON_PROMPT equ $3E ; '>'

MON_COM_RUN equ $72 ; 'r'
MON_COM_LOAD equ $6C ; 'l'
MON_COM_VERSION equ $76 ; 'v'
MON_COM_EXAMINE equ $65 ; 'e'

MON_RANGE equ $2D ; '-'

org $0000

ld HL, FFFFh ; init stackpointer to end of memory
ld SP,HL

; perform memory check    
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

jp monitorStart ; jump to monitor setup


str_welcome:
    db 'KIMP1 CASSETTE TAPE MONITOR PROGRAM 1.0', $0A, $0D, $00

str_pressPlayOnTape:
    db 'PRESS PLAY ON TAPE', $0A, $0D, $00
    
str_unknownCommand:
    db 'UNKNOWN COMMAND', $0A, $0D, $00
    
str_loading:
    db 'LOADING. PLEASE WAIT...', $0A, $0D, $00
    
str_memError:
    db 'MEMORY ERROR', $0A, $0D, $00
    
str_syntaxError:
    db 'SYNTAX ERROR', $0A, $0D, $00
    
; prints the character stored in A
printChar:
    push B
    ld B,A
    in A, (IO_UART_COM) ; read in status byte of UART
    and $01 ; mask out all bits except the TXRDY bit
    jp Z,printChar ; do this until UART is ready
    
    ; UART is ready to send another byte now
    out (IO_UART_DAT), B ; UART will start sending the byte now
    
    pop B
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

; clears the screen (requires VT100 terminal)
clearScreen:

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
_readString_loop:
    call readChar
    ld (HL), A
    
    inc HL
    
    inc C
    jp Z, _readString_end ; return if C has flown over (256 chars read)
    
    cp TERM_LF
    jp NZ, _readString_loop ; loop, if read character was not TERM_LF

_readString_end:
    pop HL ; restore HL
    ret ; character was TERM_LF, so return


    
   
monitorStart:
    ; inititalize TCCR and perform an IO-RESET before intitalizing peripherals
    ld A, (1 << BIT_IO_RESET); set only IO-RESET bit to one
    out (IO_TCCR),A
    nop ; keep the IO-RESET line high for at least 6 clock pulses
    nop
    nop
    ld A, 0 ; clear IO-RESET bit
    out (IO_TCCR),A
    ; TCCR is now intitalized and IO-Devices are reset
    
    ; PIT 2 is connected to UART, so set it up for baud rate generation
    ld A, $84 ;10000100  set counter 2 in mode 2, binary counting
    out (IO_PIT_CTRL), A
    ; write divider value to counter
    ld A, high UART_DIV_VAL
    ld B, low UART_DIV_VAL
    out (IO_PIT_C2), A
    out (IO_PIT_C2), B
    ; registers for counter are set. now we can gate the counter
    in A,(IO_TCCR)
    and IO_TCCR_WRITE_MASK ; mask out all the bits that are not readable
    or (1 << BIT_C2_GATE)
    out (IO_TCCR), A ; C2 is now counting
    
    ; write mode byte to UART (first command byte after reset)
    ld A, $4D; 01001110  8 data bits, 1 stop bit, no parity, 1 times prescaler
    out (IO_UART_COM),A 
    ; enable receiver and transmitter
    ld A, (1 << BIT_TXEN) | (1 << BIT_RXEN)
    out (IO_UART_COM), A
    
    ; IO-Devices are now initialized
    
  
    call clearScreen
  
monitor_welcome:
    ld HL, str_welcome ; print welcome message
    call printString
    
monitorPrompt_loop:
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
    
    cp MON_COM_EXAMINE
    jp Z, command_examine
    
    cp MON_COM_VERSION
    jp Z, monitor_welcome ; this command just prints out the welcome msg again
    
    ; no command character recognized. print error message
    ld HL, str_unknownCommand
    call printString
    
    jp monitorPrompt_loop
    
    
; parses a single hex character at (HL), stores result in A. 
; A = $FF means error
parseHex:
    ld A, (HL)
    cp $30
    jp S, _parseHex_noDigit ; char is < '0'
    cp $3A
    jp NS, _parseHex_noDigit ; char is > '9' 
    ; we now know the char is a digit
    sub $30 ; subtract the value of '0'
    ; hex value is now stored in A
    ret
    
_parseHex_noDigit:
    cp $41 ; the ASCII-char 'A'
    jp S, _parseHex_error ; char is < 'A'
    cp $47 ; the ASCII-char 'G'
    jp NS, _parseHex_error ; char is > 'F'
    ; we now know the char is a hex letter
    sub $50 ; subtract the value of 'A' and add the 15 offset (as A means 15)
    ;hex value is now stored in A
    ret
    
_parseHex_error:
    ld A, $FF
    ret
    
    
; parses a hex word that is pointed by HL, the amount of bytes available at HL
; indicated by C. parsing is finished if a non-hex character is found, if C 
; goes zero during parsing or after 4 chars have been read. The parsed word is
; stored in the DE register pair. After the operation, HL points to the byte
; AFTER the last one parsed and C is decremented by the amount of bytes parsed.
parseHexWord:
    ld DE, 0
    ld A, C
    cp 0
    ret Z ; no bytes for parsing -> return
    
_parseHexWord_loop:
    call parseHex
    cp $FF
    ret Z ; not a valid hex char -> return
    
    ; we have parsed a valid hex char
    
    ; perform a shift-left-by-4 operation on DE register
    ex DE,HL
    add HL, HL
    add HL, HL
    add HL, HL
    add HL, HL
    ex DE,HL
    
    ; insert loaded char into DE
    ld B,A
    ld A,E
    add B
    ld E,A
    
    inc HL
    dec C
    ret Z ; no bytes remaining -> return
    
    jp _parseHexWord_loop
    

; DO NOT CALL!!! prints out a syntax error message
monitor_syntaxError:
    ld HL, str_syntaxError
    call printString
    
    jp monitorPrompt_loop
    
    
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
_command_load_waitForTape:
    in A,(IO_TCCR)
    and (1 << BIT_TAPE_SENSE) ; mask out SENSE bit
    jp NZ,_command_load_waitForTape ; wait until user pushes play button
    
    ; user pushed play button. print loading message
    ld HL, str_loading
    call printString
    
    in A,(IO_TCCR) ; set MOTOR bit in TCR
    and IO_TCCR_WRITE_MASK ; mask out all the bits that are not readable
    or (1 << BIT_TAPE_MOTOR)
    out A,(IO_TCCR)
    
    ; motor is now running, now we can start to read bits from the tape

_command_load_tapeLoop:
    ; first, read the current tape state
    in A,(IO_TCCR)
    and (1 << BIT_TAPE_DATA_READ) ; mask out tape data bit...
    
    ; second, compare it with the previous state
    sub B
    jp NS, _command_load_tapeLoop 
    ; TODO: this is unsafe. me might miss a transition. we
    ; need an interrupt based version of some sort
    
    ; now we store the current state
    ld C,A
    
    
    xor B 

    
    ;......
    
    jp monitorPrompt_loop ; jump back to monitor loop

; monitor command that loads the first record on tape to the first available 
; memory location and gives control to the loaded program. requires no args.
command_boot:
    jp monitorPrompt_loop

; prints contents of memory loaction given by parameter
; (may be an adress range)
command_examine:
    call parseHexWord
    push DE
    inc DE ; with no arguments, we only print one byte
    
    ld A,C
    cp 0
    jp Z, _command_examine_print ; no more arguments -> start printing

    ld A,(HL) ; load remaining char
    cp MON_RANGE
    jp NZ, monitor_syntaxError ; remaining char is not range indicator -> error
    
    inc HL ; move pointer to next byte
    dec C
    
    ld A,C ; check if at least one byte is remaining
    cp 0
    jp Z, monitor_syntaxError ; nothing after range indicator -> syntax error
    
    ; TODO: continue here
    
_command_examine_print:
    
    
    
    jp monitorPrompt_loop



org ROM_END
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    