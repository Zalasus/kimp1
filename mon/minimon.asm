
;-----------------------------------
;             MINIMON
;           VERSION 0.1
;
;       for the KIMP1 system
;
;  written for the zmac assembler
;
;      Copyleft 2016 Zalasus
;       all wrongs reversed
;-----------------------------------

z80

    org $0000

    ; include definition file for KIMP1 computer
    include "../kimp1def.inc"

; general pre-assembly-configuration 
CONF_INCLUDE_HELP:     equ 1    ; set to zero to save a few bytes of ROM
CONF_RESET_ON_STARTUP: equ 0    ; will clear memory on startup if set to one

; UART configuration (data format is always 8 data, 1 stop, no parity)
CONF_UART_BAUDRATE: equ 9600
CONF_UART_PRESCALE: equ 1      ; possible values are 1, 16 and 64



; terminal characters
TERM_LF:     equ $0A ; linefeed
TERM_CR:     equ $0D ; carriage return
TERM_BS:     equ $08 ; backspace (used to move cursor left one char)
TERM_DEL:    equ $7F ; delete
TERM_NULL:   equ $00 ; null
TERM_SPACE:  equ $20 ; space
TERM_RUBOUT: equ $20 ; whatever character is not visible (space, del or whatever)

; monitor command characters
MON_COM_RUN:       equ 'r'
MON_COM_LOAD:      equ 'l'
MON_COM_BOOT:      equ 'b'
MON_COM_VERSION:   equ 'v'
MON_COM_EXAMINE:   equ 'e'
MON_COM_STORE:     equ 's'
MON_COM_HELP:      equ 'h'
MON_COM_COPY:      equ 'c'
MON_COM_PRINT:     equ 'p'
MON_COM_SOFTRESET: equ 'x'

; Other monitor characters
MON_PROMPT:             equ '>'
MON_DECIMAL:            equ '#'
MON_ANSWER:             equ '$'
MON_ARGUMENT_SEPERATOR: equ ','
MON_ADDRESS_SEPARATOR:  equ ':'


UART_DIV_VAL:  equ (CPU_SPEED/CONF_UART_PRESCALE)/CONF_UART_BAUDRATE - 1
if CONF_UART_PRESCALE == 1
    UART_MODE_INSTRUCTION: equ $4D  ; %01001101
endif
if CONF_UART_PRESCALE == 16
    UART_MODE_INSTRUCTION: equ $4E  ; %01001110
endif
if CONF_UART_PRESCALE == 64
    UART_MODE_INSTRUCTION: equ $4F  ; %01001111
endif


MON_INPUT_BUFFER:      equ ROM_END ; command line input buffer in HIMEM
MON_INPUT_BUFFER_SIZE: equ $100 ; 256 bytes
MON_EXPR_WORDSTOR:     equ MON_INPUT_BUFFER + MON_INPUT_BUFFER_SIZE ; single word storage for expression parser
MON_EXPR_ANSWER:       equ MON_EXPR_WORDSTOR + 2 ; memory for last parsed expression


;    org $0000
    
; monitor jump vector

    jp main
    jp monitorToRam
    jp printChar
    jp readChar
    jp printString
    jp readString
    jp clearScreen
    jp 0
    jp 0
    jp 0
    jp 0
    jp 0
    jp 0
    jp 0
    jp 0
    jp 0
    
main:

    ; The CPU recovers from the reset faster than the TCCR
    ;  This delay loop prevents any mishaps (like TCCR missing the first write)
    ;  After experimenting, 2^16 counts seems reasonable. 256 were too few.
    ld HL, 0
_setup_loop:
    inc HL
    ld A, H
    or L
    jp nz, _setup_loop
    

    ld HL, RAM_END ; init stackpointer to end of memory
    ld SP,HL

if CONF_RESET_ON_STARTUP == 0
    jp monitorStart ; skip soft reset and jump to monitor setup
endif


soft_reset:
    ld HL, RAM_END ; reset stackpointer
    ld SP, HL
    
    ; clear memory
    ld HL, ROM_END 
_soft_reset_loop:
    ld (HL), $00
    inc HL
    ld A,H
    cp RAM_END >> 8
    jp NZ,_soft_reset_loop
    ld A,L
    cp RAM_END & $00FF
    jp NZ,_soft_reset_loop
    
    
    jp monitorStart ; jump to monitor setup
    

    
;----------------------------STRING DATA-------------------------------

; NOTE: Use only LF ($0A) for linefeeds to save ROM. 
; The printing routine substitutes CRLF when needed.

str_welcome:
    db 'MINIMON 0.1 FOR KIMP1', $0A
    db ' WRITTEN BY ZALASUS', $0A, $00
    
str_pressPlayOnTape:
    db 'PRESS PLAY ON TAPE', $0A, $00
    
str_loading:
    db 'LOADING. PLEASE WAIT...', $0A, $00
    
str_unknownCommand:
    db 'UNKNOWN COMMAND', $0A, $00
    
str_syntaxError:
    db 'SYNTAX ERROR', $0A, $00
    
str_driveError:
    db 'DRIVE ERROR', $0A, $00

str_help:
if CONF_INCLUDE_HELP == 0
    db 'NO HELP AVAILABLE', $0A, $00
else
    db 'e S [,E]   Examine address S to E', $0A
    db 's X        Store to address X', $0A
    db 'r X        Execute program at X', $0A
    db 'c S, D, C  Copy C bytes from S to D', $0A
    db 'l X        Load from tape to address X', $0A
    db 'b          Boot from floppy', $0A
    db 'v          Display version string', $0A
    db 'h          Display this message', $0A
    db 'p X        Parses and prints X', $0A
    db 'x          Performs soft reset', $0A, $0A
    db 'Arguments in square brackets are optional', $0A
    db 'Math expressions in arguments are possible. Allowed: + - ( )', $0A
    db '$ is the last parsed number', $0A
    db 'Numbers are interpreted as hexadecimal.', $0A
    db 'Numbers prefixed with # are interpreted as decimal.', $0A, $00
endif
    
str_cls:
    db $1B, '[2J', $00 ; the VT100 way to clear screen
    

    
;---------------------------CONSOLE IO---------------------------------    

; prints the character stored in A. trashes B.
printChar:
    ld B,A
_printChar_wait:
    in A, (IO_UART_COM) ; read in status byte of UART
    and [1 << BIT_UART_TXRDY] ; mask out all bits except the TXRDY bit
    jp Z,_printChar_wait ; do this until UART is ready
    
    ; UART is ready to send another byte now
    ld A,B
    out (IO_UART_DAT), A ; UART will start sending the byte now
    
    ret
    
    
    
    
; reads one character from the UART and stores it in A. the char is not echoed.
; program execution halts until char was read.
readChar:
    in A, (IO_UART_COM) ; read in status byte of UART
    and [1 << BIT_UART_RXRDY] ; mask out all bits except the RXRDY bit
    jp Z,readChar ; do this until UART has a valid byte
    
    in A,(IO_UART_DAT) ; read in data byte
    
    ret
    
    
    
    
;------------------------------DISK IO---------------------------------

; initializes the FDC to AT/EISA mode, setting data rate etc.
fdc_init:
    ; we assume the FDC is still in reset-mode
    ld A, [1 << BIT_SOFT_RESET] ; we don't want soft reset (active low)
    out (IO_FDC_OPER),A ; writing to operations reg initializes AT/EISA-Mode
    
    ret

    
; moves the drive specified by B to home sector
fdc_home:
    ld A, $07 ; recalibrate command
    out (IO_FDC_DATA), A
    ld A, B
    and $03 ; we only need the two lower bits for the drive number
    out (IO_FDC_DATA), A
    
    ; the FDC is stepping the drive now. we need to wait until it is finished
    ;  stepping and has reached a home sector
    
    ; TODO: read status register 0 to A here
    
    
    ret    
    
;----------------------------------------------------------------------  
    
    
    
    
    
; prints all characters from HL to the next zero byte
printString:
    ld A, (HL) ; fetch byte
    or A  ; compare with zero
    ret Z ; if byte is zero, we are done
    
    cp TERM_LF ; if byte is LF, we need to go to next line
    jp Z, _printString_lf
    
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
;  found. The amount of bytes read is stored in the C register. HL points to the
;  first read byte after the call. The last byte of the string 
;  (not included in C) is set to 0. During reading, control characters like 
;  backspace are processed accordingly so the input resembles somewhat of a 
;  command prompt.
readString:
    ld C, 1 ; count one char more than actually read to account for terminator when looking for overflow
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

    
    
; prints CRLF characters
printNewLine:
    ld A, $0A
    call printChar
    ld A, $0D
    call printChar
    ret

    

    
; clears the screen
clearScreen:
    ld HL, str_cls
    call printString
    ret
    
    
    
; parses a single decimal character in A, stores result in A. If a non-digit 
;  character is found, A is set to $FF.
parseDecimal:
    cp $30
    jp M, _parse_error ; char is < '0'
    cp $3A
    jp P, _parse_error ; char is > '9' 
    ; we now know the char is a digit
    sub '0' ; subtract the value of '0'
    ; hex value is now stored in A
    ret
    
; parses a single hex character in A, stores result in A. Accepts both upper-
;  and lowercase characters. If a non-hex character is found, A is set to $FF.
parseHex:
    cp $30
    jp M, _parseHex_noDigit ; char is < '0'
    cp $3A
    jp P, _parseHex_noDigit ; char is > '9' 
    ; char is a digit
    sub '0' ; subtract the value of '0'. hex value is now stored in A
    ret
    
_parseHex_noDigit:
    cp 'a'
    jp M, _parseHex_noLC ; char is < 'a'
    cp 'g'
    jp P, _parseHex_noLC; char is > 'f'
    ; char is a lowercase hex letter
    sub 'a' - $0a ; subtract the value of 'a' and add $0a (as a means $0a)
    ret
    
_parseHex_noLC:
    cp 'A'
    jp M, _parse_error ; char is < 'A'
    cp 'G'
    jp P, _parse_error ; char is > 'F'
    ; char is an uppercase hex letter
    sub 'A' - $0a  ; subtract the value of 'A' and add $0a (as A means $0a)
    ret
    
_parse_error:
    ld A, $FF
    ret
    
    


    
    
; parses a hex word that is pointed by HL, the amount of bytes available at HL
;  indicated by C. parsing is finished if a non-hex character is found or if C 
;  goes zero during parsing. The parsed word is stored in the DE register pair.
;  If more than 4 hex chars are found in the buffer, the first occurring chars
;  are ignored and only the 4 last chars will be stored in the DE register.
;  After the operation, HL points to the byte AFTER the last one parsed and C is 
;  decremented by the amount of bytes parsed. If not a single byte could be
;  parsed, the carry bit is set and DE contains 0. If parsing succeeds, 
;  the carry bit is reset.
parseHexWord:
    ld DE, 0
    ld A, C
    or A  ; compare with zero
    jp Z, setCarryReturn ; no bytes for parsing -> error return
    
    ld A, (HL)
    call parseHex
    cp $FF
    jp Z, setCarryReturn ; first char not a valid hex char -> error return
    
_parseHexWord_loop:
    ld A, (HL)
    call parseHex
    cp $FF
    jp Z, resetCarryReturn ; not a valid hex char -> return
    
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
    add A,B
    ld E,A
    ld A,D ; add carry bit to D
    adc A,0
    ld D,A
    
    inc HL
    dec C
    jp Z, resetCarryReturn ; no bytes remaining -> return
    
    jp _parseHexWord_loop
    
    
; parses a decimal word that is pointed by HL, the amount of bytes available at
;  HL indicated by C. parsing is finished if a non-digit character is found or 
;  if C goes zero during parsing. The parsed word is stored in the DE register
;  pair. If more than digits are found in the buffer than fit in DE , the first 
;  occurring digits are ignored and only the 4 last ones will be stored in DE.
;  After the operation, HL points to the byte AFTER the last one parsed and C is 
;  decremented by the amount of bytes parsed. If not a single byte could be
;  parsed, the carry bit is set and DE contains 0. If parsing succeeds, 
;  the carry bit is reset.
parseDecWord:
    ld DE, 0
    ld A, C
    or A  ; compare with zero
    jp Z, setCarryReturn ; no bytes for parsing -> error return
    
    ld A, (HL)
    call parseDecimal
    cp $FF
    jp Z, setCarryReturn ; first digit not a valid digit -> error return
    
_parseDecWord_loop:
    ld A, (HL)
    call parseDecimal
    cp $FF
    jp Z, resetCarryReturn ; not a valid digit -> return
    
    ; we have parsed a valid digit
    
    ; multiply DE by 10
    ex DE,HL
    push DE
    ld D, H
    ld E, L
    add HL, HL ; shift left by 3 ( temp = value * 8)
    add HL, HL
    add HL, HL
    add HL, DE ; add two times ( temp = temp + 2* value)
    add HL, DE
    pop DE
    ex DE,HL
    
    ; add loaded digit to DE
    ld B,A
    ld A,E
    add B
    ld E,A
    ld A,D ; add carry bit to D
    adc A,0
    ld D,A
    
    inc HL
    dec C
    jp Z, resetCarryReturn ; no bytes remaining -> return
    
    jp _parseDecWord_loop
    
    
resetCarryReturn:
    scf
    ccf
    ret
    
setCarryReturn:
    scf
    ret
    
    
    
    
; prints the byte stored in A in hexadecimal format.
;  taken from Alexis Kotlowys monitor because I'm lazy. Man, this guy really hasn't many comments to spare.
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
    ld A, MON_ADDRESS_SEPARATOR ; print colon
    call printChar
    ld A, TERM_SPACE ; print space
    call printChar
    ret
    
   
   
; increments HL and decrements C while (HL) is a whitespace character
skipWhites:
    ld A,(HL)
    cp TERM_SPACE ; check if char is == SPACE
    ret NZ
    ;char was == SPACE -> skip char
    inc HL
    dec C
    jp skipWhites
   
   
;---------------------------------MAIN---------------------------------     
   
monitorStart:
    ; initialize TCCR and perform an IO-RESET before initializing peripherals
    ld A, [1 << BIT_IO_RESET]; set only IO-RESET bit to one
    out (IO_TCCR),A
    nop ; keep the IO-RESET line high for at least 8 clock pulses ( = 2 NOPs)
    nop
    xor A ; clear all TCCR bits, including IO-RESET bit
    out (IO_TCCR),A
    ; TCCR is now initialized and IO-Devices are reset
    
    ; PIT 2 is connected to UART, so set it up for baud rate generation
    ld A, $B6 ; %10110110  ; set counter 2 in mode 3, binary counting
    out (IO_PIT_CTRL), A
    ; write divider value to counter (first LSB, then MSB as set in command above)
    ld A, low UART_DIV_VAL
    out (IO_PIT_C2), A
    ld A, high UART_DIV_VAL
    out (IO_PIT_C2), A
    ; registers for counter are set. now we can gate the counter
    in A,(IO_TCCR)
    and IO_TCCR_WRITE_MASK ; mask out all the bits that are not readable
    or [1 << BIT_C2_GATE]
    out (IO_TCCR), A ; C2 is now counting
    
    ; write mode byte to UART (first command byte after reset)
    ld A, UART_MODE_INSTRUCTION
    out (IO_UART_COM), A 
    ; enable receiver and transmitter.
    ;  also, set /DTR of UART to 1. Indicator LED should turn off, giving visual feedback that CPU is alive
    ld A, [(1 << BIT_UART_TXEN) | (1 << BIT_UART_RXEN) | (1 << BIT_UART_DTR)]
    out (IO_UART_COM), A

    ; IO-Devices are now initialized
    
    call clearScreen
  
monitor_welcome:
    ld HL, str_welcome ; print welcome message
    call printString
    
monitorPrompt_loop:
    call printNewLine
    ld A, MON_PROMPT ; print input prompt
    call printChar
    
    ld HL, MON_INPUT_BUFFER ; this is where we want to store the read bytes
    call readString ; read user input
    
    ld A,C ; user entered nothing. prompt again
    or A  ; compare with zero
    jp Z, monitorPrompt_loop
    
    call printNewLine ; insert a new line after user entered a command
    
    ; process user input
    ld B,(HL) ; load fist byte entered
    inc HL ; move HL to next byte
    dec C
    
    call skipWhites
    
    ld A,B
    
    ; determine entered command
    cp MON_COM_BOOT
    jp Z, command_boot
    
    cp MON_COM_LOAD
    jp Z, command_load 
    
    cp MON_COM_RUN
    jp Z, command_run
    
    cp MON_COM_STORE
    jp Z, command_store
    
    cp MON_COM_EXAMINE
    jp Z, command_examine
    
    cp MON_COM_COPY
    jp Z, command_copy
    
    cp MON_COM_HELP
    jp Z, command_help
    
    cp MON_COM_PRINT
    jp Z, command_print

    cp MON_COM_SOFTRESET
    jp Z, soft_reset
    
    cp MON_COM_VERSION
    jp Z, monitor_welcome
    
    ; no command character recognized. print error message
    ld HL, str_unknownCommand
    call printString
    
    jp monitorPrompt_loop
    
    
    
monitor_syntaxError:
    ld HL, str_syntaxError
    call printString
    
    jp monitorPrompt_loop
    
    
; parses a hex/dec word. if the first char in the buffer is the base ten 
; indicator (#), the word is parsed as decimal. otherwise the hex parser is 
; used. if not a single character was read by the parser routines, this
; routine prints a syntax error.
parseNumber:
    ld A,(HL)
    cp MON_DECIMAL
    jp NZ, _parseNumber_hex
    
    ; we have a decimal indicator
    inc HL ; throw the indicator away
    dec C
    
    call parseDecWord
    jp C, monitor_syntaxError ; no digits were read -> syntax error
    
    ret
    
_parseNumber_hex:
    call parseHexWord
    jp C, monitor_syntaxError ; no digits were read -> syntax error
    
    ret
    
    
;--------------------Monitor command definition area----------------------
; NOTE: These are not CALL-ed! 
; In the end of each command, simply jump back to monitorPromt_loop


; prints help message
command_help:
    ld HL, str_help
    call printString
    jp monitorPrompt_loop



; monitor command to jump to given location
command_run:
    ; parse expression in input buffer and store result in DE
    call expression
    
    ex DE, HL
    jp (HL) ; we are leaving the monitor here. no need to jump back to loop

    
    
; monitor command that loads the first record on tape into memory
command_load:
    call expression ; destination address

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
    
    ; motor is now running, now we can start to read bits from tape

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

    ; turn off motor
    in A,(IO_TCCR)
    and IO_TCCR_WRITE_MASK & ~[1 << BIT_TAPE_MOTOR]
    out (IO_TCCR), A
    
    jp monitorPrompt_loop ; jump back to monitor loop

    
; loads the first sector from fdd 0 into memory and jumps to the loaded code
command_boot:

    jp monitorPrompt_loop

    
; prints contents of memory location given by parameter
;  (may be an address range)
command_examine:
    call expression
    call skipWhites
    push DE
    
    ld A,C
    or A  ; compare with zero
    jp Z, _command_examine_print ; no more arguments -> start printing

    ld A,(HL) ; load remaining char
    cp MON_ARGUMENT_SEPERATOR
    jp NZ, monitor_syntaxError ; remaining char is not , -> error
    
    inc HL ; move pointer to next byte
    dec C
    call skipWhites
    
    call expression
    
_command_examine_print:
    inc DE ; since we want the upper address to be inclusive
    
    pop HL
    ; start address is now stored in HL, end address in DE
    
    ld C, 16 ; counter for bytes on line (to insert LF after 16 bytes)

    ; print starting address token
    call printAdressToken
    
_command_examine_print_loop:

    ld A, (HL)
    call printHex
    
    ld A, TERM_SPACE
    call printChar
    
    inc HL
    
    dec C
    jp NZ, _command_examine_print_noLf
    
    ; we have printed 16 chars. print a linefeed and a new address token
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

    

; used to enter an arbitrary amount of bytes starting from a given 
;  address (first argument). accepts only hex chars.
; TODO: this routine is completely messed up, so it would be nice if someone
;  could clean up this piece of code
command_store:
    call expression
    ex DE, HL
    
    ld C, 16 ; counter for bytes on line (to insert LF after 16 bytes)

    ; print starting address token
    call printAdressToken
    
_command_store_loop:
    call readChar
    cp TERM_CR ; pressed return?
    jp Z, monitorPrompt_loop ; yes -> we are done
    ld B,A
    call parseHex
    cp $ff
    jp Z, _command_store_loop ; char was not valid. read again
    ; char was valid hex. echo it
    push AF
    ld A, B
    call printChar
    pop AF
    
    ; shift left by 4, store in D and proceed
    ld D, A
    sla D
    sla D
    sla D
    sla D
    
_command_store_lowerNibble_loop:
    ; read another nibble
    call readChar
    cp TERM_CR ; pressed return?
    jp Z, monitorPrompt_loop ; yepp -> we are done
    ld B,A
    call parseHex
    cp $ff
    jp Z, _command_store_lowerNibble_loop ; char was not valid. read again
    ; char was valid hex. echo it
    push AF
    ld A, B
    call printChar
    pop AF
    
    or D ; insert D into A
    ; A now contains the whole entered byte
    
    ld (HL), A ; store byte
    inc HL
    
    ld A, TERM_SPACE
    call printChar
    
    dec C
    jp NZ, _command_store_noLf
    
    ; we have printed 16 chars. print a linefeed and a new address token
    call printNewLine
    ld C,16
    
    ; print address token
    call printAdressToken
    
_command_store_noLf:
    jp _command_store_loop

    
command_copy:
    call expression ; parse source address
    call skipWhites
    push DE
    
    ld A,(HL) ; load remaining char
    cp MON_ARGUMENT_SEPERATOR
    jp NZ, monitor_syntaxError ; remaining char is not , -> error
    inc HL
    dec C
    call skipWhites
    
    call expression ; parse destination address
    call skipWhites
    push DE
    
    ld A,(HL) ; load remaining char
    cp MON_ARGUMENT_SEPERATOR
    jp NZ, monitor_syntaxError ; remaining char is not , -> error
    inc HL
    dec C
    call skipWhites
    
    call expression ; parse byte count
    ld B, D ; store count in byte counter
    ld C, E
    pop DE
    pop HL
    
    ldir ; start copying

    jp monitorPrompt_loop
    
    
    

command_print:
    call skipWhites

_command_print_loop:
    call expression
    ld A,D
    call printHex
    ld A,E
    call printHex
    call printNewLine
    
    call skipWhites
    ld A,C ; chars remaining?
    or A
    jp NZ,_command_print_loop
    
    jp monitorPrompt_loop




; HL = HL - DE
subtract:
    ld A,E ; calculate two's complement of DE
    cpl
    ld E,A
    ld A,D
    cpl
    ld D,A
    inc DE
    add HL,DE
    ret
    
    
    
    
    
expression:
    call term
    
_expression_loop:
    ld A,C ; chars remaining?
    or A
    jp Z, _expression_end
    ld A,(HL)
    cp '+'
    jp Z,_expression_add
    cp '-'
    jp NZ, _expression_end
    
    push DE
    inc HL
    dec C
    call term
    ld (MON_EXPR_WORDSTOR), HL ; save HL
    pop HL
    call subtract
    ex DE,HL
    ld HL,(MON_EXPR_WORDSTOR) ; restore HL
    jp _expression_loop
    
_expression_add:
    push DE
    inc HL
    dec C
    call term
    ld (MON_EXPR_WORDSTOR), HL
    pop HL
    add HL,DE
    ex DE,HL
    ld HL,(MON_EXPR_WORDSTOR)
    jp _expression_loop
    
_expression_end:
    ld (MON_EXPR_ANSWER),DE
    ret
    
    
term: ; we supported multiplication and division here once. what's the point?
factor:
    call skipWhites
    ld A,(HL)
    cp '('
    jp Z, _factor_expression
    cp MON_ANSWER
    jp Z, _factor_answer
    
    call parseNumber
    jp _factor_end
    
_factor_expression:
    inc HL
    dec C
    call expression
    ld A,(HL)
    cp ')'
    jp NZ,monitor_syntaxError
    inc HL
    dec C
    jp _factor_end
    
_factor_answer:
    inc HL
    dec C
    ld DE,(MON_EXPR_ANSWER)
    
_factor_end:
    call skipWhites
    ret

    
    
        
shovelknight_size:    equ shovelknight_rom_end - shovelknight_rom
shovelknight_ram:     equ MON_EXPR_ANSWER
shovelknight_ram_end: equ shovelknight_ram + shovelknight_size
        
monitorToRam:
    ; load shovelknight into memory
    ld HL, shovelknight_rom
    ld DE, shovelknight_ram
    ld BC, shovelknight_size
    ldir
    
    ; give control to shovelknight
    jp shovelknight_ram
    
    
shovelknight_rom:

    ld HL, $0000 ; copy monitor into HIMEM
    ld DE, shovelknight_ram_end
    ld BC, ROM_END
    ldir
    
    in A,(IO_TCCR) ; disable rom mapping
    and IO_TCCR_WRITE_MASK
    or [1 << BIT_ROM_GATE]
    out (IO_TCCR),A 
    
    ld HL, shovelknight_ram_end ; copy monitor back into LOMEM
    ld DE, $0000
    ld BC, ROM_END
    ldir
    
    jp monitorPrompt_loop ; shovelknight is done

shovelknight_rom_end:
monitor_end:
    
    ; pad out file for maximum rom size
    dc [8192 - monitor_end], $ff

    end main

    
    
    
    
    
    
    
    
    
    
    
    
    
     