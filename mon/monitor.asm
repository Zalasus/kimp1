
;-----------------------------------
;   CASSETTE TAPE MONITOR PROGRAM
;           VERSION 1.00
;
;       for the KIMP1 system
;
;    (C) 1979 Knifto Industries
;-----------------------------------

z80

    ; include definition file for KIMP1 computer
    include kimp1def.inc


UART_BAUDRATE equ 9600
UART_PRESCALE equ 1
UART_DIV_VAL equ (CPU_SPEED/UART_PRESCALE)/UART_BAUDRATE - 1

TERM_LF equ $0A ; linefeed
TERM_CR equ $0D ; carriage return
TERM_BS equ $08 ; backspace
TERM_DEL equ $7F ; delete
TERM_NULL equ $00 ; null
TERM_SPACE equ $20 ; space

MON_PROMPT equ '>'

MON_COM_RUN equ 'r'
MON_COM_LOAD equ 'l'
MON_COM_BOOT equ 'b'
MON_COM_VERSION equ 'v'
MON_COM_EXAMINE equ 'e'
MON_COM_STORE equ 's'
MON_COM_HELP equ 'h'
MON_RANGE equ '-'
MON_ADRESS_SEPARATOR equ ':'

    org $0000
    
main:

    ld HL, RAM_END ; init stackpointer to end of memory
    ld SP,HL

    jp monitorStart ; jump to monitor setup


str_welcome:
    db 'KIMP1 CASSETTE TAPE MONITOR PROGRAM 1.0', $0A, $0D, $00

str_logon:
    db 'LOGON: ', $00
    
str_answer:
    db 'ANSWER  >', $21, low(str_badLogon-23), high(str_badLogon-23)
    db $CD, low(printString), high(printString)
    db $C3, low(monitorPrompt_loop), high(monitorPrompt_loop)
    db $57,$68,$61,$74,$20,$77,$61,$73,$20,$74,$68,$65,$20,$71,$75,$65,$73,$74
    db $69,$6F,$6E,$3F,$00
    
str_badLogon:
    db 'INDENTIFICATION NOT RECOGNIZED BY SYSTEM', $0A, $0D, $00
    
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
    
str_logonName:
    db 'Joshua', $00
    
str_help:
    db 'X FOR ANY HEX CHARACTER', $0A, $0D
    db 'eXXXX[-XXXX] = EXAMINE ADDRESS XXXX to XXXX', $0A, $0D
    db 'sXXXX = STORE TO ADDRESS XXXX', $0A, $0D
    db 'rXXXX = EXECUTE PROGRAM AT XXXX', $0A, $0D
    db 'lXXXX = LOAD FROM TAPE TO ADDRESS XXXX', $0A, $0D
    db 'v = DISPLAY MONITOR VERSION', $0A, $0D
    db 'h = DISPLAY THIS MESSAGE', $0A, $0D
    db 'b = BOOTS FROM FLOPPY', $0A, $0D
    db 'I KNOW A HELP COMMAND FOR A MONITOR PROGRAM IS IDIOTIC.', $0A, $0D
    db 'HOWEVER, WE STILL HAD SOME SPACE IN THE ROM', $0A, $0D, $00
    
str_cls:
    db $1B, '[2J', $00
    
; prints the character stored in A. trashes B.
printChar:
    ld B,A
    in A, (IO_UART_COM) ; read in status byte of UART
    and $01 ; mask out all bits except the TXRDY bit
    jp Z,printChar ; do this until UART is ready
    
    ; UART is ready to send another byte now
    ld A,B
    out (IO_UART_DAT), A ; UART will start sending the byte now
    
    ret
    
; prints all characters from HL to the next zero byte
printString:
    ld A, (HL) ; fetch byte
    cp 0
    ret Z ; if byte is zero, we are done
    
    ;byte is not zero, so print it out
    call printChar
    
    inc HL ; increment HL and loop
    jp printString
    

; prints CRLF characters
printNewLine:
    ld A, $0A
    call printChar
    ld A, $0D
    call printChar
    ret

; clears the screen (requires VT100/ANSI)
clearScreen:
    ; replacement for debugger
    ld A, $03
    call printChar
    ;ld HL, str_cls
    ;call printString
    ret
    
    
; reads one character from the UART and stores it in A. the char is not echoed.
; program excecution halts until char was read.
readChar:
    in A, (IO_UART_COM) ; read in status byte of UART
    and $02 ; mask out all bits except the RXRDY bit
    jp Z,readChar ; do this until UART has a valid byte
    
    in A,(IO_UART_DAT) ; read in data byte
    
    ret
    

; Reads characters from the UART and stores them in the location pointed by HL.
; Reading continues until 255 characters have been read or a LF character is
; found. The amount of bytes read is stored in the C register. HL points to the
; first read byte after the call. The last byte of the string 
; (not included in C) is set to 0. During reading, control characters like 
; backspace are processed accordingly so the input resembles somewhat of a 
; command prompt.
readString:
    ld C, 1 ; count one char more than actually read, so 
    push HL ; save HL on stack
_readString_loop:
    call readChar
    
    cp TERM_BS
    jp Z, _readString_backspace
    cp TERM_DEL
    jp Z, _readString_backspace
    cp TERM_LF
    jp Z, _readString_end ; LF means we are done
    cp TERM_CR
    jp Z, _readString_end ; CR means the same
    
    call printChar ; echo the entered character
    
    ld (HL), A
    
    inc HL
    inc C
    jp Z, _readString_end ; return if C has flown over (255 chars read)
    
    jp _readString_loop

_readString_backspace:
    ld A, C
    cp 1 ; important! we have one more char in c than actually read!
    jp Z, _readString_loop ; nothing to delete. get on with it.
    
    ld A, TERM_BS
    call printChar ; echo the BS to move cursor back one char...
    ld A, TERM_NULL
    call printChar ; overwrite the last entered char with NULL...
    ld A, TERM_BS
    call printChar ; and place cursor over the null char again
    
    dec C ; buffer minus one
    dec HL
    
    jp _readString_loop ; back to loop
    
    
_readString_end:
    dec C ; remove the additional char
    ld (HL),0 ; insert null terminator
    pop HL ; move HL back to start of buffer
    ret

    
; parses a single hex character in A, stores result in A. Accepts both upper-
; and lowercase characters. If a non-hex character is found, A is set to $FF.
parseHex:
    cp $30
    jp M, _parseHex_noDigit ; char is < '0'
    cp $3A
    jp P, _parseHex_noDigit ; char is > '9' 
    ; we now know the char is a digit
    sub '0' ; subtract the value of '0'
    ; hex value is now stored in A
    ret
    
_parseHex_noDigit:
    cp 'A'
    jp M, _parseHex_noUC ; char is < 'A'
    cp 'G'
    jp P, _parseHex_noUC ; char is > 'F'
    ; we now know the char is an uppercase hex letter
    sub 'A' - $0a  ; subtract the value of 'A' and add $0a (as A means $0a)
    ;hex value is now stored in A
    ret
    
_parseHex_noUC:
    cp 'a'
    jp M, _parseHex_error ; char is < 'a'
    cp 'g'
    jp P, _parseHex_error; char is > 'f'
    ; we now know the char is an lowercase hex letter
    sub 'a' - $0a ; subtract the value of 'a' and add $0a (as a means $0a)
    ;hex value is now stored in A
    ret
    
_parseHex_error:
    ld A, $FF
    ret
    
    
; parses a hex word that is pointed by HL, the amount of bytes available at HL
; indicated by C. parsing is finished if a non-hex character is found or if C 
; goes zero during parsing. The parsed word is stored in the DE register pair.
; If more than 4 hex chars are found in the buffer, the first occuring chars
; are ignored and only the 4 last chars will be stored in the DE register.
; After the operation, HL points to the byte AFTER the last one parsed and C is 
; decremented by the amount of bytes parsed.
parseHexWord:
    ld DE, 0
    ld A, C
    cp 0
    ret Z ; no bytes for parsing -> return
    
_parseHexWord_loop:
    ld A, (HL)
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
    
    
; prints the byte stored in A in hexadecimal format
; taken from Alexis Kotlowys monitor because I'm lazy
printHex:
    push af
	and	$f0
	rrca
	rrca
	rrca
	rrca
	call _printHex_1
	pop	af
_printHex_1:
    and $0f
	cp $0a
	jp c, _printHex_2
	sub	$09
	or $40
	jp	_printHex_3
_printHex_2:	
    or	$30
_printHex_3:
	call printChar
	ret

; prints the word stored in HL, followed by a colon and space character
printAdressToken:
    ld A, H
    call printHex
    ld A, L
    call printHex
    ld A, MON_ADRESS_SEPARATOR ; print colon
    call printChar
    ld A, TERM_SPACE ; print space
    call printChar
    ret
   
   
; inititalizes the FDC to AT/EISA mode, setting data rate etc.
fdc_init:
    ; we assume the FDC is still in reset-mode
    ld A, [1 << BIT_SOFT_RESET] ; we don't want soft reset (active low)
    out (IO_FDC_OPER),A ; writing to operations reg initializes AT/EISA-Mode
    
    ret
    
; moves the drive specified by B to home sector
fdc_home:
    ld A, $08 ; recalibrate command
    out (IO_FDC_DATA), A
    ld A, B
    and $03
    out (IO_FDC_DATA), A
    
    ret
   
   
   
monitorStart:
    ; inititalize TCCR and perform an IO-RESET before intitalizing peripherals
    ld A, [1 << BIT_IO_RESET]; set only IO-RESET bit to one
    out (IO_TCCR),A
    nop ; keep the IO-RESET line high for at least 6 clock pulses
    nop
    nop
    ld A, 0 ; clear IO-RESET bit
    out (IO_TCCR),A
    ; TCCR is now intitalized and IO-Devices are reset
    
    ; PIT 2 is connected to UART, so set it up for baud rate generation
    ld A, $84 ; 10000100  set counter 2 in mode 2, binary counting
    out (IO_PIT_CTRL), A
    ; write divider value to counter
    ld A, high UART_DIV_VAL
    out (IO_PIT_C2), A
    ld A, low UART_DIV_VAL
    out (IO_PIT_C2), A
    ; registers for counter are set. now we can gate the counter
    in A,(IO_TCCR)
    and IO_TCCR_WRITE_MASK ; mask out all the bits that are not readable
    or [1 << BIT_C2_GATE]
    out (IO_TCCR), A ; C2 is now counting
    
    ; write mode byte to UART (first command byte after reset)
    ld A, $4D ; 01001110   8 data bits, 1 stop bit, no parity, 1 x prescaler
    out (IO_UART_COM),A 
    ; enable receiver and transmitter
    ld A, [1 << BIT_TXEN] | [1 << BIT_RXEN]
    out (IO_UART_COM), A
    
    ; IO-Devices are now initialized
    
  
    call clearScreen
  
monitor_welcome:
    ld HL, str_welcome ; print welcome message
    call printString
    
monitorPrompt_loop:
    call printNewLine
    ld A, MON_PROMPT ; print input promt
    call printChar
    
    ld HL, ROM_END ; this is where we want to store the read bytes
    call readString ; read user input
    
    ld A,C ; user entered nothing. prompt again
    cp 0
    jp Z, monitorPrompt_loop
    
    call printNewLine ; insert a new line after user entered a command
    
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
    
    cp MON_COM_STORE
    jp Z, command_store
    
    cp MON_COM_VERSION
    jp Z, monitor_welcome ; this command just prints out the welcome msg again
    
    cp MON_COM_HELP
    jp Z, command_help
    
    ; no command character recognized. print error message
    ld HL, str_unknownCommand
    call printString
    
    jp monitorPrompt_loop
    
    
    
monitor_syntaxError:
    ld HL, str_syntaxError
    call printString
    
    jp monitorPrompt_loop
    
    
;--------------------Monitor command definition area----------------------
; NOTE: These are not CALL-ed! 
; In the end of each command, simply jump back to monitorPromt_loop


command_help:
    ld HL, str_help
    call printString
    jp monitorPrompt_loop

; monitor command to jump to given location
command_run:
    ; store the hexadecimal ASCII-coded number at (HL) in the DE register pair
    call parseHexWord
    
    ex DE, HL
    jp (HL) ; we are leaving the monitor here. no need to jump back to loop

    
    
; monitor command that loads the first record on tape into memory
command_load:
    ; first, we need to set up the PIT C0 to count the time between two zero
    ; crossings in the tape signal

    ld HL, str_pressPlayOnTape
    call printString
_command_load_waitForTape:
    in A,(IO_TCCR)
    and [1 << BIT_TAPE_SENSE] ; mask out SENSE bit
    jp NZ,_command_load_waitForTape ; wait until user pushes play button
    
    ; user pushed play button. print loading message
    ld HL, str_loading
    call printString
    
    in A,(IO_TCCR) ; set MOTOR bit in TCR
    and IO_TCCR_WRITE_MASK ; mask out all the bits that are not readable
    or [1 << BIT_TAPE_MOTOR]
    out (IO_TCCR), A
    
    ; motor is now running, now we can start to read bits from the tape

_command_load_tapeLoop:
    ; first, read the current tape state
    in A,(IO_TCCR)
    and [1 << BIT_TAPE_DATA_READ] ; mask out tape data bit...
    
    ; second, compare it with the previous state
    sub B
    jp P, _command_load_tapeLoop 
    ; TODO: this is unsafe. me might miss a transition. we
    ; need an interrupt based version of some sort
    
    ; now we store the current state
    ld C,A
    
    
    xor B 

    
    ;......
    
    jp monitorPrompt_loop ; jump back to monitor loop

    
; loads the first sector from fdd 0 into memory and jumps to the loaded code
command_boot:

    

    jp monitorPrompt_loop

    
; prints contents of memory loaction given by parameter
; (may be an adress range)
command_examine:
    call parseHexWord
    push DE
    
    ld A,C
    cp 0
    jp Z, _command_examine_print ; no more arguments -> start printing

    ld A,(HL) ; load remaining char
    cp MON_RANGE
    jp NZ, monitor_syntaxError ; remaining char is not range indicator -> error
    
    inc HL ; move pointer to next byte
    dec C
    
    ; with no bytes bytes remaining in input buffer, this sets DE to 0, so
    ; the command eXXXX- would print the whole memory, starting from XXXX and
    ; rolling over at FFFF
    call parseHexWord
    
_command_examine_print:
    inc DE ; since we want the upper address to be inclusive
    
    pop HL
    ; start address is now stored in HL, end adress in DE
    
    ld C, 16 ; counter for bytes on line (to insert LF after 16 bytes)

    ; print starting adress token
    call printAdressToken
    
_command_examine_print_loop:

    ld A, (HL)
    call printHex
    
    ld A, TERM_SPACE
    call printChar
    
    inc HL
    
    dec C
    jp NZ, _command_examine_print_noLf
    
    ; we have printed 16 chars. print a linefeed and a new adress token
    call printNewLine
    ld C,16
    
    call printAdressToken
    
_command_examine_print_noLf:

    ; is HL == DE yet? if not, jump back to loop
    ld A, H
    cp D
    jp NZ, _command_examine_print_loop
    
    ld A, L
    cp E
    jp NZ, _command_examine_print_loop
    
    ; HL == DE. we are finished. back to monitor
    
    jp monitorPrompt_loop

    

; asks the user to enter an arbitrary amount of bytes starting from a given
; adress (first argument)
command_store:
    call parseHexWord
    ex DE, HL
    
    ld C, 16 ; counter for bytes on line (to insert LF after 16 bytes)

    ; print starting adress token
    call printAdressToken
    
_command_store_loop:
    call readChar
    call printChar ; echo
    call parseHex
    cp $ff
    jp Z, monitorPrompt_loop ; char was not valid. we are done
    
    ; char was half of a byte. shift left by 4, store in D and proceed
    mov D, A
    sla D
    sla D
    sla D
    sla D
    
    call readChar
    call printChar ; echo
    call parseHex
    cp $ff
    jp Z, monitorPrompt_loop ; char was not valid. we are done
    
    or D ; A now contains the whole entered byte
    
    ld (HL), A ; store byte
    inc HL
    
    ld A, TERM_SPACE
    call printChar
    
    dec C
    jp NZ, _command_store_noLf
    
    ; we have printed 16 chars. print a linefeed and a new adress token
    call printNewLine
    ld C,16
    
    ; print adress token
    call printAdressToken
    
_command_store_noLf:
    jp _command_store_loop

    

    org ROM_END
    
    end main

    
    
    
    
    
    
    
    
    
    
    
    
    
    