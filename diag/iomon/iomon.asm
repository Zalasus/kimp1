

    org $0000


    include ../../kimp1def.inc



; terminal characters
TERM_LF:     equ $0A ; linefeed
TERM_CR:     equ $0D ; carriage return
TERM_BS:     equ $08 ; backspace (used to move cursor left one char)
TERM_DEL:    equ $7F ; delete
TERM_NULL:   equ $00 ; null
TERM_SPACE:  equ $20 ; space
TERM_RUBOUT: equ $20 ; whatever character is not visible (space, del or whatever)

CONF_UART_BAUDRATE: equ 9600
UART_DIV_VAL:  equ CPU_SPEED/CONF_UART_BAUDRATE - 1


    jp main

    
    org $0030   ; rst 30h  ($f7)
        jp fdc_isr



    org $0038   ; rst 38h  ($ff)
        jp rtc_isr



main:
    ld HL, 0
_setup_loop:
    inc HL
    ld A, H
    or L
    jp nz, _setup_loop

    ld HL, RAM_END ; init stackpointer to end of memory
    ld SP,HL

    ld HL, $0000  ; catch any stack underflows
    push HL

    ; init interrupt handlers
    im 0
    ld A, $f7
    out (IO_IVR_FDC), A
    ld A, $ff
    out (IO_IVR_RTC), A

    jp ioMon


;---------------CONIO-----------------

    include ../../mon/io_console.asm



;--------------STRINGS----------------

str_fdcInt:
    db '[FDC INT]', $0A, $00

str_rtcInt:
    db '[RTC INT]', $0A, $00

str_invalidCommand:
    db 'INVALID COMMAND', $0A, $00

str_cls:
    db $1B, '[2J', $00 ; the VT100 way to clear screen


;--------------ISRs-----------------

fdc_isr:
    ex AF, AF'
    exx

    ld HL, str_fdcInt
    call printString

    exx
    ex AF, AF'
    ret



rtc_isr:
    ex AF, AF'
    exx

    ld HL, str_rtcInt
    call printString

    exx
    ex AF, AF'
    ret


;-------------FORMATTERS------------

; Prints the byte stored in A in uppercase hexadecimal format.
;  Taken from Alexis Kotlowys monitor because I'm lazy. Man, this guy really hasn't many comments to spare.
printHex:
    push af
    and $f0
    rrca
    rrca
    rrca
    rrca
    call _printHex_1
    pop af
_printHex_1:
    and $0f
    cp $0a
    jp c, _printHex_2
    sub $09
    or $40
    jp _printHex_3
_printHex_2:
    or $30
_printHex_3:
    call printChar
    ret



parseHex:
    cp $30
    jp m, _parseHex_noDigit ; char is < '0'
    cp $3A
    jp p, _parseHex_noDigit ; char is > '9' 
    ; char is a digit
    sub '0' ; subtract the value of '0'. hex value is now stored in A
    ret
    
_parseHex_noDigit:
    cp 'a'
    jp m, _parseHex_noLC ; char is < 'a'
    cp 'g'
    jp p, _parseHex_noLC; char is > 'f'
    ; char is a lowercase hex letter
    sub 'a' - $0a ; subtract the value of 'a' and add $0a (as a means $0a)
    ret
    
_parseHex_noLC:
    cp 'A'
    jp m, _parse_error ; char is < 'A'
    cp 'G'
    jp p, _parse_error ; char is > 'F'
    ; char is an uppercase hex letter
    sub 'A' - $0a  ; subtract the value of 'A' and add $0a (as A means $0a)
    ret
    
_parse_error:
    ld A, $FF
    ret



parseByte:
    ld A, (HL)
    inc HL
    dec C
    call parseHex
    ld B, A

    ld A, C
    or A
    jp z, _parseByte_noByteLeft

    ld A, B
    rlca
    rlca
    rlca
    rlca
    ld B, A

    ld A, (HL)
    inc HL
    dec C
    call parseHex
_parseByte_noByteLeft:
    add B
    ret



; increments HL and decrements C while (HL) is a whitespace character
skipWhites:
    ld A,(HL)
    cp TERM_SPACE ; check if char is == SPACE
    ret nz
    ;char was == SPACE -> skip char
    inc HL
    dec C
    jp skipWhites


;-------------MAIN-------------------


ioMon:
    ; initialize TCCR and perform an IO-RESET before initializing peripherals
    ld A, [1 << BIT_TCCR_IO_RESET]; set only IO-RESET bit to one
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
    or [1 << BIT_TCCR_C2_GATE]
    out (IO_TCCR), A ; C2 is now counting
    
    ; write mode byte to UART (first command byte after reset)
    ld A, $4D  ; 1x mode, 8N1
    out (IO_UART_COM), A 
    ; enable receiver and transmitter.
    ;  also, set /DTR of UART to 1. Indicator LED should turn off, giving visual feedback that CPU is alive
    ld A, [(1 << BIT_UART_TXEN) | (1 << BIT_UART_RXEN) | (1 << BIT_UART_DTR)]
    out (IO_UART_COM), A



prompt:
    call printNewLine
    ld A, '>' ; print input prompt
    call printChar
    
    ld HL, DAT_INPUT_BUFFER ; this is where we want to store the read bytes
    call readString ; read user input
    
    ld A, C 
    or A
    jp z, prompt  ; user entered nothing. prompt again
    
    call printNewLine ; insert a new line after user entered a command

    call skipWhites ; skip whitespace at beginning of command

proc_loop: 
    ; process user input
    ld B, (HL) ; load fist byte entered
    inc HL ; move HL to next byte
    dec C
    
    call skipWhites
    
    ld A, B
    
    ; determine entered command
    cp 'i'
    jp z, command_in
    
    cp 'o'
    jp z, command_out
    
    cp 'p'
    jp z, command_port
    
    cp '_'
    jp z, command_ei

    cp '|'
    jp z, command_di

    ld HL, str_invalidCommand
    call printString
    jp prompt

proc_end:
    call skipWhites
    ld A, C 
    or A
    jp nz, proc_loop  ; still stuff left to process
    jp prompt



command_in:
    ld B, C  ; stash C
    ld A, (DAT_CURRENT_PORT)
    ld C, A
    in D, (C)
    ld C, B  ; restore C
    ld A, 'i'
    call printChar
    ld A, D
    call printHex
    call printNewLine
    jp proc_end

command_out:
    call parseByte
    ld D, A 
    ld B, C  ; stash C
    ld A, (DAT_CURRENT_PORT)
    ld C, A
    out (C), D
    ld C, B  ; restore C
    ld A, 'o'
    call printChar
    ld A, D
    call printHex
    call printNewLine
    jp proc_end

command_port:
    call parseByte
    ld (DAT_CURRENT_PORT), A
    ld A, 'p'
    call printChar
    ld A, (DAT_CURRENT_PORT)
    call printHex
    call printNewLine
    jp proc_end

command_ei:
    ei
    jp proc_end

command_di:
    di
    jp proc_end
    

    


mon_end:
    
    ; pad out file for maximum rom size
    dc [8192 - mon_end], $ff



; --------------- DATA AREAS --------------------

    org ROM_END

; Define data areas in RAM here

DAT_CURRENT_PORT:         ds 1
DAT_INPUT_BUFFER:         ds 256


    end main





