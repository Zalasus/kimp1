
;-----------------------------------
;             MINIMON
;           VERSION 0.3
;
;       for the KIMP1 system
;
;  written for the zmac assembler
;
;      Copyleft 2017 Zalasus
;       all wrongs reversed
;-----------------------------------


    org $0000

    include ../kimp1def.inc



;-------------- PRE-ASSEMBLY CONFIGURATION -------------------

; General configuration 
CONF_INCLUDE_HELP:     equ 1    ; set to zero to save a few bytes of ROM
CONF_RESET_ON_STARTUP: equ 0    ; will clear memory on startup if set to one
CONF_STARTUP_DELAY:    equ 1    ; will delay approx. 1 sec on startup

; UART configuration (data format is always 8 data, 1 stop, no parity)
;  Prescaler > 1 greatly reduces the number of bit errors due to phase shift of clock
CONF_UART_BAUDRATE: equ 9600
CONF_UART_PRESCALE: equ 16      ; possible values are 1, 16 and 64

; Command configuration
CONF_COMM_EXAMINE_BYTES_PER_LINE:   equ 16



; ---------------- DEFINITION AREA ----------------------

; terminal characters
TERM_LF:     equ $0A ; linefeed
TERM_CR:     equ $0D ; carriage return
TERM_BS:     equ $08 ; backspace (used to move cursor left one char)
TERM_DEL:    equ $7F ; delete
TERM_NULL:   equ $00 ; null
TERM_SPACE:  equ $20 ; space
TERM_RUBOUT: equ $20 ; whatever character is not visible (space, del or whatever)

; monitor command characters
MON_COM_RUN:        equ 'x'
MON_COM_RUN_JUMP:   equ '!'
MON_COM_REGISTER:   equ 'f'
MON_COM_LOAD:       equ 'l'
MON_COM_BOOT:       equ 'b'
MON_COM_VERSION:    equ 'v'
MON_COM_EXAMINE:    equ 'e'
MON_COM_STORE:      equ 's'
MON_COM_HELP:       equ 'h'
MON_COM_COPY:       equ 'c'
MON_COM_INPUT_HEX:  equ 'i'
MON_COM_OUTPUT_HEX: equ 'o'
MON_COM_PRINT:      equ 'p'
MON_COM_DISK:       equ 'd'
MON_COM_SOFTRESET:  equ 'r'

; Other monitor characters
MON_PROMPT:             equ '>'
MON_DECIMAL:            equ '#'
MON_ANSWER:             equ '$'
MON_ARGUMENT_SEPERATOR: equ ','
MON_ADDRESS_SEPARATOR:  equ ':'

MON_INPUT_BUFFER_SIZE:  equ $100

if CONF_UART_PRESCALE == 1
    UART_MODE_INSTRUCTION: equ $4D  ; %01001101
    UART_DIV_VAL:  equ CPU_SPEED/(CONF_UART_PRESCALE*CONF_UART_BAUDRATE) - 1 ; minus one correction seems to be necessary
endif
if CONF_UART_PRESCALE == 16
    UART_MODE_INSTRUCTION: equ $4E  ; %01001110
    UART_DIV_VAL:  equ CPU_SPEED/(CONF_UART_PRESCALE*CONF_UART_BAUDRATE)
endif
if CONF_UART_PRESCALE == 64
    UART_MODE_INSTRUCTION: equ $4F  ; %01001111
    UART_DIV_VAL:  equ CPU_SPEED/(CONF_UART_PRESCALE*CONF_UART_BAUDRATE)
endif



; ------------- MONITOR JUMP VECTOR -------------

    org $0000

    jp main
    jp monitorToRam
    jp printChar
    jp readChar
    jp hasChar
    jp printString
    jp readString
    jp clearScreen
    jp printHex
    jp readHex
    jp 0
    jp 0
    jp 0
    jp 0
    jp 0
    jp monitorPrompt_loop



; ---------- INTERRUPT SERVICE ROUTINE TABLE -------

; IM0 interrupt handlers here

IVR_FDC_DEF:    equ $f7
IVR_FDC_NOP:    equ $00
    org $0030   ; rst 30h  ($f7)
        jp fdc_isr



IVR_RTC:        equ $ff
    org $0038   ; rst 38h  ($ff)
        ; restart vector used by RTC IRQ
        ;  since the RTC-IVR is shared by both RTC and OPL, we need to
        ;  determine the exact cause for the interrupt here and jump to
        ;  the right handler. OPL has priority over RTC
        exx
        ex AF, AF'
        in A, (IO_EBCR)
        bit BIT_EBCR_IRQ_OPL, A
        jp z, opl_isr
        jp rtc_isr



; ---------------- INIT CODE ---------------------

main:

if CONF_STARTUP_DELAY == 1
    ; The CPU recovers from the reset faster than the TCCR
    ;  This delay loop prevents any mishaps (like TCCR missing the first write)
    ;  After experimenting, 2^16 counts seems reasonable. 256 were too few.
    ;  Maybe a smaller cap in the reset circuit would do. 10uF is bit overkill, really.
    ld HL, 0
_setup_loop:
    inc HL
    ld A, H
    or L
    jp nz, _setup_loop    
endif
    
    ld HL, $0000  
    ld SP, HL   ; init stackpointer to end of memory
    push HL   ; catch any stack underflows

    ; init interrupt handlers
    ;  we use IM0~ the 19 clock cycle penalty for IM2 is way too heavy for disk access
    ;  init both IVRs to known value, leave interrupts disabled for now
    di
    im 0
    xor A
    out (IO_IVR_FDC), A
    out (IO_IVR_RTC), A

if CONF_RESET_ON_STARTUP == 0
    jp monitorStart ; skip soft reset and jump to monitor setup
endif


soft_reset:
    ld HL, $0000  ; reset stackpointer
    ld SP, HL
    push HL       ; restore underflow catch
    
    ; clear memory
    ld HL, ROM_END 
_soft_reset_loop:
    ld (HL), $00
    inc HL
    ld A, H
    cp high RAM_END
    jp nz, _soft_reset_loop
    ld A, L
    cp low RAM_END
    jp nz, _soft_reset_loop
    
    
    jp monitorStart ; jump to monitor setup
    

    
;----------------------------STRING DATA-------------------------------

    include strings.asm
    

    
;-------------------------- CONSOLE IO --------------------------------

    include io_console.asm



;---------------------- EXTENSION BOARD ROUTINES ----------------------

; Checks if extension board is present. 
;  Sets carry bit if present, resets if not.
ext_test:
    ; TEST bit should always read inverted value as written if 
    ;  board is plugged in. Check both states twice to be sure
    call _ext_test_cl
    ret nc  ; first try failed. no need to do a second run

_ext_test_cl:
    ld A, $01
    out (IO_EBCR), A
    in A, (IO_EBCR)
    bit BIT_EBCR_TEST, A
    jp nz, resetCarryReturn

    xor A
    out (IO_EBCR), A
    in A, (IO_EBCR)
    bit BIT_EBCR_TEST, A
    jp z, resetCarryReturn

    jp setCarryReturn



;----------------------------- DISK IO --------------------------------

    include io_fdc.asm



;---------------------------- RTC ACCESS ------------------------------

    include io_rtc.asm



;-------------------------- SOUND CHIP ACCESS -------------------------

    include io_opl.asm



;---------------------- PARSER & FORMATTER METHODS --------------------
           
; Parses a single decimal character in A, stores result in A. If a non-digit 
;  character is found, A is set to $FF.
parseDecimal:
    cp $30
    jp m, _parse_error ; char is < '0'
    cp $3A
    jp p, _parse_error ; char is > '9' 
    ; we now know the char is a digit
    sub '0' ; subtract the value of '0'
    ; hex value is now stored in A
    ret
    
; Parses a single hex character in A, stores result in A. Accepts both upper-
;  and lowercase characters. If a non-hex character is found, A is set to $FF.
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
    


; Reads two hex characters from console and stores result in A. Trashes B
;  Sets carry bit if invalid characters are read.
readHex:
    call readChar
    call parseHex
    cp $ff
    jp z, setCarryReturn

    ; shift left by 4
    add A 
    add A
    add A
    add A
    ld B, A

    call readChar
    call parseHex
    cp $ff
    jp z, setCarryReturn
    
    or B
    
    jp resetCarryReturn


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
    
    
; Prints the word stored in HL, followed by a colon and space character
printAddressToken:
    ld A, H
    call printHex
    ld A, L
    call printHex
    ld A, MON_ADDRESS_SEPARATOR ; print colon
    call printChar
    ld A, TERM_SPACE ; print space
    call printChar
    ret
    
    
; Parses a hex word that is pointed by HL, the amount of bytes available at HL
;  indicated by C. Parsing is finished if a non-hex character is found or if C 
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
    jp z, setCarryReturn ; no bytes for parsing -> error return
    
    ld A, (HL)
    call parseHex
    cp $FF
    jp z, setCarryReturn ; first char not a valid hex char -> error return
    
_parseHexWord_loop:
    ld A, (HL)
    call parseHex
    cp $FF
    jp z, resetCarryReturn ; not a valid hex char -> return
    
    ; we have parsed a valid hex char
    
    ; perform a shift-left-by-4 operation on DE register
    ex DE, HL
    add HL, HL
    add HL, HL
    add HL, HL
    add HL, HL
    ex DE, HL
    
    ; insert loaded char into DE
    ld B, A
    ld A, E
    add A, B
    ld E, A
    ld A, D ; add carry bit to D
    adc A, 0
    ld D, A
    
    inc HL
    dec C
    jp z, resetCarryReturn ; no bytes remaining -> return
    
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
    jp z, setCarryReturn ; no bytes for parsing -> error return
    
    ld A, (HL)
    call parseDecimal
    cp $FF
    jp z, setCarryReturn ; first digit not a valid digit -> error return
    
_parseDecWord_loop:
    ld A, (HL)
    call parseDecimal
    cp $FF
    jp z, resetCarryReturn ; not a valid digit -> return
    
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
    jp z, resetCarryReturn ; no bytes remaining -> return
    
    jp _parseDecWord_loop
  


; Parses a hex/dec word. If the first char in the buffer is the base ten 
;  indicator (#), the word is parsed as decimal. Otherwise the hex parser is 
;  used. If not a single character was read by the parser routines, the carry bit is set.
parseNumber:
    ld A,(HL)
    cp MON_DECIMAL
    jp nz, _parseNumber_hex
    
    ; we have a decimal indicator
    inc HL ; throw the indicator away
    dec C
    
    call parseDecWord
    ret ; carry bit is still set in case of error
   
_parseNumber_hex:
    call parseHexWord
    ret  ; carry bit is still set in case of error
    


; Resets carry bit and returns.
resetCarryReturn:
    scf
    ccf
    ret
    
; Sets carry bit and returns.
setCarryReturn:
    scf
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
    
    
    
; Parses a math expression at (HL), the amount of bytes available beeing the value of C.
;  Parsing is terminated once a non-expression character is found or no bytes are available.
;  After parsing, DE contains the 16 bit result of the expression, HL points to the first
;  character after the expression and C is decremented by the amount of bytes parsed.
;  If syntax errors are found, the carry bit is set. If parsing terminates without errors,
;  carry bit is reset.
expression:
    call _expression_term
    ret c ; syntax error    

_expression_loop:
    ld A, C ; chars remaining?
    or A
    jp z, _expression_end
    ld A, (HL)
    cp '+'
    jp z, _expression_add
    cp '-'
    jp nz, _expression_end
    
    push DE
    inc HL
    dec C
    call _expression_term
    ret c  ; syntax error
    ld (DAT_EXPR_WORDSTOR), HL ; save HL
    pop HL
    call subtractHLDE
    ex DE, HL
    ld HL, (DAT_EXPR_WORDSTOR) ; restore HL
    jp _expression_loop
    
_expression_add:
    push DE
    inc HL
    dec C
    call _expression_term
    ret c ; syntax error
    ld (DAT_EXPR_WORDSTOR), HL
    pop HL
    add HL, DE
    ex DE, HL
    ld HL, (DAT_EXPR_WORDSTOR)
    jp _expression_loop
    
_expression_end:
    ld (DAT_EXPR_ANSWER), DE
    jp resetCarryReturn
    
    
_expression_term: 
    ; we supported multiplication and division here once. what's the point?
    ; just hand down to factor
_expression_factor:
    call skipWhites
    ld A, (HL)
    cp '('
    jp z, _expression_factor_subexpression
    cp MON_ANSWER
    jp z, _expression_factor_answer
    
    call parseNumber
    ret c ; syntax error
    jp _expression_factor_end
    
_expression_factor_subexpression:
    inc HL
    dec C
    call expression
    ret c ; syntax error
    ld A, (HL)
    cp ')'    ; expect closing parentheses
    jp nz, setCarryReturn ; syntax error
    inc HL
    dec C
    jp _expression_factor_end
    
_expression_factor_answer:
    inc HL
    dec C
    ld DE, (DAT_EXPR_ANSWER)
    
_expression_factor_end:
    call skipWhites
    jp resetCarryReturn


; HL = HL - DE. DE is not affected
subtractHLDE:
    push DE
    ld A, E ; calculate two's complement of DE
    cpl
    ld E, A
    ld A, D
    cpl
    ld D, A
    inc DE
    add HL, DE
    pop DE
    ret



;------------------------ MISC HELPER METHODS -------------------------------

; Stashes the register file. Layout of buffer: AF BC DE HL SP IX IY I  (16 bit regs LE)
stashRegisters:
    ld (DAT_MON_REG_BUFFER+2), BC
    ld (DAT_MON_REG_BUFFER+4), DE
    ld (DAT_MON_REG_BUFFER+6), HL
    ld (DAT_MON_REG_BUFFER+10), IX
    ld (DAT_MON_REG_BUFFER+12), IY

    push AF
    pop HL
    ld (DAT_MON_REG_BUFFER), HL

    ld HL, $0002 ; stack pointer is two bytes to low when calling this. fix that manually
    add HL, SP
    ld (DAT_MON_REG_BUFFER+8), HL

    ld A, I
    ld (DAT_MON_REG_BUFFER+14), A
    
    ret
    


; Loads the stashed registers back into the register file (except SP. We don't want to force stack corruption)
restoreRegisterStash:
    ld A, (DAT_MON_REG_BUFFER+14)
    ld I, A

    ld HL, (DAT_MON_REG_BUFFER)
    push HL
    pop AF

    ld BC, (DAT_MON_REG_BUFFER+2)
    ld DE, (DAT_MON_REG_BUFFER+4)
    ld HL, (DAT_MON_REG_BUFFER+6)
    ld IX, (DAT_MON_REG_BUFFER+10)
    ld IY, (DAT_MON_REG_BUFFER+12)

    ret
    


;------------------------- MAIN MONITOR LOOP --------------------------------     
   
monitorStart:
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
    ld A, UART_MODE_INSTRUCTION
    out (IO_UART_COM), A 
    ; enable receiver and transmitter.
    ;  also, set /DTR of UART to 1. Indicator LED should turn off, giving visual feedback that CPU is alive
    ld A, [(1 << BIT_UART_TXEN) | (1 << BIT_UART_RXEN) | (1 << BIT_UART_DTR)]
    out (IO_UART_COM), A


    ; IO-Devices are now initialized. Check if extension board is present and initialize
    ;  extension devices if neccessary
    xor A
    ld (DAT_EXT_INITIALIZED), A

    call ext_test
    jp nc, _monitor_init_noext
    call rtc_init
    call opl_init
    call fdc_init
    ei
    ld A, $ff
    ld (DAT_EXT_INITIALIZED), A
_monitor_init_noext:
    

    call clearScreen
  
monitor_welcome:
    ld HL, str_welcome ; print welcome message
    call printString

    ; check if extension board was initialized and print message
    call printNewLine
    ld A, (DAT_EXT_INITIALIZED)
    cp $ff
    jp nz, _monitor_welcomeExt_noext
    ld HL, str_extPresent
    jp _monitor_welcomeExt_end
_monitor_welcomeExt_noext:
    ld HL, str_noExtPresent
_monitor_welcomeExt_end:
    call printString
    
monitorPrompt_loop:
    call printNewLine
    ld A, MON_PROMPT ; print input prompt
    call printChar
    
    ld HL, DAT_INPUT_BUFFER ; this is where we want to store the read bytes
    call readString ; read user input
    
    ld A, C 
    or A
    jp z, monitorPrompt_loop ; user entered nothing. prompt again
    
    call printNewLine ; insert a new line after user entered a command
    
    call skipWhites ; skip whitespace at beginning of command

    ; process user input
    ld B, (HL) ; load fist byte entered
    inc HL ; move HL to next byte
    dec C
    
    call skipWhites
    
    ld A, B
    
    ; determine entered command
    cp MON_COM_BOOT
    jp z, command_boot
    
    cp MON_COM_LOAD
    jp z, command_load 
    
    cp MON_COM_RUN
    jp z, command_run
    
    cp MON_COM_REGISTER
    jp z, command_register
    
    cp MON_COM_STORE
    jp z, command_store
    
    cp MON_COM_EXAMINE
    jp z, command_examine
    
    cp MON_COM_COPY
    jp z, command_copy
    
    cp MON_COM_HELP
    jp z, command_help
    
    cp MON_COM_PRINT
    jp z, command_print

    cp MON_COM_INPUT_HEX
    jp z, command_input_hex

    cp MON_COM_OUTPUT_HEX
    jp z, command_output_hex

    cp MON_COM_DISK
    jp z, command_disk

    cp MON_COM_SOFTRESET
    jp z, soft_reset
    
    cp MON_COM_VERSION
    jp z, monitor_welcome
    
    ; no command character recognized. print error message
    ld HL, str_unknownCommand
    call printString
    
    jp monitorPrompt_loop
    
    
    
monitor_syntaxError:
    ld HL, str_syntaxError
    call printString
    
    jp monitorPrompt_loop
    
    

; subroutine that prints message asking user to hit space.
;  all characters are discarded. returns once user has hit space. 
monitor_waitForSpace:
    ld HL, str_hitSpace
    call printString
_monitor_waitForSpace_loop:
    call readChar
    cp TERM_SPACE
    jp nz, _monitor_waitForSpace_loop
    ret

    

;---------------------- MONITOR COMMANDS ---------------------

; NOTE: These are not CALL-ed! 
; In the end of each command, simply jump back to monitorPromt_loop
; The stack is always empty when the monitor jumps to these routines


; Prints help message
command_help:
    ld HL, str_help
    call printString
    jp monitorPrompt_loop



; Monitor command to jump to given location
command_run:
    ; check for ! token to see if user wants to jump instead of call
    ld A, (HL)
    cp MON_COM_RUN_JUMP
    jp z, _command_run_nocall

    ; there's no indirect call, so we put the return address on the stack ourself.
    ;  let called code return to cleanup routine (let's just hope the program doesn't ruin anything)
    ld DE, _command_run_cleanup
    push DE
    jp _command_run_noskip

_command_run_nocall:
    inc HL ; skip ! token for expression parser
    dec C
_command_run_noskip:
    call expression
    jp c, monitor_syntaxError

    ; push target address. we will simulate indirect jump by using a return
    ;  (can't use jp (HL) since we must restore register stash before jumping)
    push DE
    call restoreRegisterStash
    ret ; bogus return to argument address
    
_command_run_cleanup:
    ; we end up here when called routine returns
    ;  stash register file in buffer to be examined by Register command
    call stashRegisters

    jp monitorPrompt_loop



; Prints the saved register buffer in human readable format or
;  modifies it.
;  NOTE: Quick and dirty routine. Surely to be compacted somehow.
command_register:
    call skipWhites
    ld A, C
    or A
    jp z, _command_register_print

    ; user supplied arguments -> modify stash
    

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

    
    
; Loads the first record on tape into memory at given address
command_load:
    call expression ; destination address
    jp c, monitor_syntaxError

    ; first, we need to set up the PIT C0 to count the time between two zero
    ; crossings in the tape signal

    ld HL, str_pressPlayOnTape
    call printString
_command_load_waitForTape:
    in A,(IO_TCCR)
    and [1 << BIT_TCCR_TAPE_SENSE] ; mask out SENSE bit (active low!)
    jp nz,_command_load_waitForTape ; wait until user pushes play button
    
    ; user pushed play button. print loading message
    ld HL, str_loading
    call printString
    
    in A,(IO_TCCR) ; set MOTOR bit in TCR
    and IO_TCCR_WRITE_MASK ; mask out all the bits that are not readable
    or [1 << BIT_TCCR_TAPE_MOTOR]
    out (IO_TCCR), A
    
    ; motor is now running, now we can start to read bits from tape

_command_load_tapeLoop:
    ; first, read the current tape state
    in A,(IO_TCCR)
    and [1 << BIT_TCCR_TAPE_DATA_READ] ; mask out tape data bit...
    
    ; second, compare it with the previous state
    sub B
    jp p, _command_load_tapeLoop 
    
    ; now we store the current state
    ld C,A
    
    
    xor B

    
    ;......

    ; turn off motor
    in A, (IO_TCCR)
    and IO_TCCR_WRITE_MASK & ~[1 << BIT_TCCR_TAPE_MOTOR]
    out (IO_TCCR), A
    
    jp monitorPrompt_loop ; jump back to monitor loop

    

; Loads the first sector from drive A into memory and jumps to the loaded code
command_boot:
    ; first, check if extension board is plugged in
    ld A, (DAT_EXT_INITIALIZED)
    cp $ff
    jp z, _command_boot_cont

    ; board is not present (on startup). print error
    ld HL, str_noExtPresent
    call printString
    jp monitorPrompt_loop

_command_boot_cont:
    ld HL, str_notImplemented
    call printString
    jp monitorPrompt_loop

_command_boot_error:
    ld HL, str_fdcError
    call printString
    jp monitorPrompt_loop


    
; Prints contents of memory location given by parameter
;  (may be an address range)
command_examine:
    call expression
    jp c, monitor_syntaxError
    call skipWhites
    push DE
    
    ld A, C
    or A  ; compare with zero
    jp z, _command_examine_print ; no more arguments -> start printing

    ld A, (HL) ; load remaining char
    cp MON_ARGUMENT_SEPERATOR
    jp nz, monitor_syntaxError ; remaining char is not , -> error
    
    inc HL ; move pointer to next byte
    dec C
    call skipWhites
    
    call expression
    jp c, monitor_syntaxError
    
_command_examine_print:
    inc DE ; since we want the upper address to be inclusive
    
    pop HL
    ; start address is now stored in HL, end address in DE
    
    ld C, CONF_COMM_EXAMINE_BYTES_PER_LINE + 1 ; counter for bytes on line (to insert LF after 16 bytes)

    ; print starting address token
    call printAddressToken

    ; if end address was ffff (is now 0) we need to skip the first loop check
    ld A, D
    or A
    jp nz, _command_examine_loop
    ld A, E
    or A
    jp z, _command_examine_cont1
    
_command_examine_loop:
    ; reached end address yet?
    ld A, H
    cp D
    jp nz, _command_examine_cont1
    ld A, L
    cp E
    jp z, _command_examine_end
_command_examine_cont1:
    dec C
    jp z, _command_examine_lf
_command_examine_cont2:

    ; check if user terminated printing
    call hasChar
    or A
    jp nz, _command_examine_end

    ld A, (HL)
    call printHex
    
    ld A, TERM_SPACE
    call printChar
    
    inc HL
    
    jp _command_examine_loop
    
_command_examine_lf:
    ; we have printed 16 chars. print a linefeed and a new address token
    call printNewLine
    call printAddressToken
    ld C, CONF_COMM_EXAMINE_BYTES_PER_LINE + 1
    jp _command_examine_loop
    
_command_examine_end:

    jp monitorPrompt_loop

    

; Used to enter an arbitrary amount of bytes starting from a given 
;  address (first argument). Accepts only hex chars.
; TODO: this routine is completely messed up, so it would be nice if someone
;  could clean up this piece of code
command_store:
    call expression
    jp c, monitor_syntaxError
    ex DE, HL
    
    ld C, 16 ; counter for bytes on line (to insert LF after 16 bytes)

    ; print starting address token
    call printAddressToken
    
_command_store_loop:
    call readChar
    cp TERM_CR ; pressed return?
    jp z, monitorPrompt_loop ; yes -> we are done
    ld B, A
    call parseHex
    cp $ff
    jp z, _command_store_loop ; char was not valid. read again
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
    jp z, monitorPrompt_loop ; yepp -> we are done
    ld B, A
    call parseHex
    cp $ff
    jp z, _command_store_lowerNibble_loop ; char was not valid. read again
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
    jp nz, _command_store_noLf
    
    ; we have printed 16 chars. print a linefeed and a new address token
    call printNewLine
    ld C, 16
    
    ; print address token
    call printAddressToken
    
_command_store_noLf:
    jp _command_store_loop

    
    
; Copies block of memory
command_copy:
    call expression ; parse source address
    jp c, monitor_syntaxError
    call skipWhites
    push DE
    
    ld A, (HL) ; load remaining char
    cp MON_ARGUMENT_SEPERATOR
    jp nz, monitor_syntaxError ; remaining char is not , -> error
    inc HL
    dec C
    call skipWhites
    
    call expression ; parse destination address
    jp c, monitor_syntaxError
    call skipWhites
    push DE
    
    ld A, (HL) ; load remaining char
    cp MON_ARGUMENT_SEPERATOR
    jp nz, monitor_syntaxError ; remaining char is not , -> error
    inc HL
    dec C
    call skipWhites
    
    call expression ; parse byte count
    jp c, monitor_syntaxError
    ld B, D ; store count in byte counter
    ld C, E
    pop DE
    pop HL
    
    ldir ; start copying

    jp monitorPrompt_loop
    
    

command_input_hex:
    ld HL, str_readingHex
    call printString

_hex_record_start:
    ld D, $00  ; initialize D for checksum calculation
    ; wait until start code is read
_hex_waitForStart:
    call readChar
    cp ':'
    jp nz, _hex_waitForStart
    
    ; byte count
    call readHex
    jp c, _hex_error
    ld C, A        ; store byte count in C
    add D  ; add to checksum
    ld D, A

    ; address. store it in HL
    call readHex
    jp c, _hex_error
    ld H, A
    add D  ; add to checksum
    ld D, A
    call readHex
    jp c, _hex_error
    ld L, A
    add D  ; add to checksum
    ld D, A

    ; record type
    call readHex
    jp c, _hex_error
    ld B, A  ; buffer in B
    add D  ; add to checksum
    ld D, A
    ld A, B ; restore
    cp $00
    jp z, _hex_data
    cp $01
    jp z, _hex_eof
    jp _hex_error ; unrecognized record type


_hex_data:
    
    ld A, C ; check if byte count was zero
    or A
_hex_data_loop:
    jp z, _hex_data_done

    call readHex
    jp c, _hex_error
    ld (HL), A  ; store byte
    add D       ; add byte to D for checksum calculation
    ld D, A

    dec C
    inc HL

    jp _hex_data_loop


_hex_data_done:
    ; read checksum and add D to it
    call readHex
    jp c, _hex_error
    add D   ; if record/checksum is okay, this must yield zero
    jp nz, _hex_checksum_error  

    ; expect CR or LF line terminator
    call readChar
    cp TERM_LF
    jp z, _hex_record_start
    cp TERM_CR
    jp z, _hex_record_start ; got CR. there's likely a LF following, which will get caught when waiting for :

    jp _hex_error


_hex_eof:
    ; expect checksum $ff
    call readHex
    cp $ff
    jp nz, _hex_checksum_error
    
    ; got expected checksum. we're done reading hex file
    ld HL, str_hexOK
    call printString

    call monitor_waitForSpace ; just in case any stry records were appended

    jp monitorPrompt_loop


_hex_checksum_error:
    ld HL, str_hexChecksumError
    call printString

    ; wait for user to hit space and discard all other chars.
    ;  keeps hex garble out of command prompt
    call monitor_waitForSpace

    jp monitorPrompt_loop

_hex_error:
    ld HL, str_hexError
    call printString

    ; wait for user to hit space and discard all other chars.
    ;  keeps hex garble out of command prompt
    call monitor_waitForSpace

    jp monitorPrompt_loop



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

    

command_print:
    call skipWhites

_command_print_loop:
    call expression
    jp c, monitor_syntaxError

    ld A, D
    call printHex
    ld A, E
    call printHex
    call printNewLine
    
    call skipWhites
    ld A, C ; chars remaining?
    or A
    jp nz, _command_print_loop
    
    jp monitorPrompt_loop

    

command_disk:
    ld A, (DAT_EXT_INITIALIZED)
    cp $ff
    jp nz, _command_disk_error_extNotInitialized

    call skipWhites

    ld A, (HL)
    inc HL
    dec C

    cp 'h'
    jp z, _command_disk_help

    cp 'd'
    jp z, _command_disk_selDisk

    cp 't'
    jp z, _command_disk_selTrack

    cp 's'
    jp z, _command_disk_selSector

    cp 'r'
    jp z, _command_disk_recalibrate

    cp 'f'
    jp z, _command_disk_format

    jp monitor_syntaxError

_command_disk_help:
    ld HL, str_disktoolHelp
    call printString
    jp monitorPrompt_loop ; ignore all other characters after help

_command_disk_selDisk:
    call expression
    jp c, monitor_syntaxError
    ld A, E
    ld (DAT_DISK_NUMBER), A
    call fdc_specify
    jp c, _command_disk_error
    call fdc_recalibrate
    jp c, _command_disk_error
    jp _command_disk_end

_command_disk_selTrack:
    call expression
    jp c, monitor_syntaxError
    ld A, E
    ld (DAT_DISK_TRACK), A
    call fdc_seek
    jp c, _command_disk_error
    jp _command_disk_end

_command_disk_selSector:
    call expression
    jp c, monitor_syntaxError
    ld A, E
    ld (DAT_DISK_SECTOR), A
    jp _command_disk_end

_command_disk_recalibrate:
    call fdc_recalibrate
    jp c, _command_disk_error
    jp _command_disk_end

_command_disk_format:
    call skipWhites
    ld A, (HL)
    cp 'a'
    jp nz, _command_disk_format_notAll
    inc HL
    dec C
    ; TODO: add code to format all tracks here
_command_disk_format_notAll:
    call fdc_format
    jp c, _command_disk_error
    jp _command_disk_end

_command_disk_end:
    ; would have looped here, but HL and C might have changed and
    ;  i don't want to mess around with stashing them
    jp monitorPrompt_loop

_command_disk_error_extNotInitialized:
    ld HL, str_noExtPresent
    call printString
    jp monitorPrompt_loop

_command_disk_error:
    cp $01
    jp z, _command_disk_fdcError
    ld HL, str_diskError
    call printString
    jp monitorPrompt_loop
_command_disk_fdcError:
    ld HL, str_fdcError
    call printString
    jp monitorPrompt_loop



; --------------------- MISC ROUTINES ----------------------

; The last resort. If anything goes horribly wrong, this method can be used
;  to provide feedback via the LED connected to the UART. Disables interrupts.
panic:
    di
    ld B, 0

_panic_loop_entry:
    ld HL, 0
_panic_loop:
    inc HL
    ld A, H
    or L
    jp nz, _panic_loop 

    ld A, B
    xor [1 << BIT_UART_DTR]
    out (IO_UART_COM), A
    ld B, A
    
    jp _panic_loop_entry



; Copies Minimon ROM into RAM and disables ROM mapping. 
;  Control is given back to monitor prompt after copying has finished.
;  This routine disables interrupts prior to copying, since there is a short
;  period during copying in which there is no valid code in the interrupt vectors.
;  Interrupts are NOT re-enabled automatically.
monitorToRam:
    di ; make sure copying is never interrupted

    ; load shovelknight into memory
    ;  ugh... yes, i called it shovelknight cause it shovels around the monitor code. get off my back
    ld HL, shovelknight_rom
    ld DE, shovelknight_ram
    ld BC, shovelknight_size
    ldir
    
    ; give control to shovelknight
    jp shovelknight_ram

shovelknight_ram:     equ DAT_INPUT_BUFFER    ; where to store monitor ROM before disabling ROM mapping
shovelknight_size:    equ shovelknight_rom_end - shovelknight_rom                   
shovelknight_ram_end: equ shovelknight_ram + shovelknight_size    
    
shovelknight_rom:

    ld HL, $0000 ; copy monitor into HIMEM
    ld DE, shovelknight_ram_end
    ld BC, monitor_end
    ldir
    
    in A, (IO_TCCR) ; disable rom mapping
    and IO_TCCR_WRITE_MASK
    or [1 << BIT_TCCR_ROM_GATE]
    out (IO_TCCR),A 
    
    ld HL, shovelknight_ram_end ; copy monitor back into LOMEM
    ld DE, $0000
    ld BC, monitor_end
    ldir
    
    jp monitorPrompt_loop ; shovelknight is done

shovelknight_rom_end:

monitor_end:
    
    ; pad out file for maximum rom size
    dc [8192 - monitor_end], $ff



; --------------- DATA AREAS --------------------

    org ROM_END

; Define data areas in RAM here

DAT_EXPR_WORDSTOR:         ds 2 ; single word storage for expression parser
DAT_EXPR_ANSWER:           ds 2 ; memory (word) for last parsed expression
DAT_RTC_COUNTER:           ds 1
DAT_RTC_CALLBACK:          ds 2
DAT_EXT_INITIALIZED:       ds 1
DAT_DISK_MOTOR_DRIVE:      ds 1  ; bit 1 = motors were enabled, bit 0 = drive number
DAT_DISK_NUMBER:           ds 1  ; number of currently selected drive
DAT_DISK_TRACK:            ds 1
DAT_DISK_SECTOR:           ds 1
DAT_DISK_INT_SR0:          ds 1  ; used by FDC ISR to store result bytes of SENSEI command
DAT_DISK_INT_CYLINDER:     ds 1  ;   "
DAT_INPUT_BUFFER:          ds MON_INPUT_BUFFER_SIZE ; command line input buffer in HIMEM
DAT_MON_REG_BUFFER:        ds 16
DAT_DISK_DATABUFFER:       ds 128  ; this may grow downwards quite a bit. always keep last


    end main



    
    