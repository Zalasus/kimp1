
;-----------------------------------
;             MINIMON
;           VERSION 0.7
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

CONF_VERSION:          equ $07  ; version number

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

; Floppy disk configuration
CONF_DISK_MAX_RETRIES:        equ 8
CONF_DISK_USE_MFM:            equ 1    ; 0 for FM, 1 for MFM
CONF_DISK_BYTES_PER_SECTOR:   equ 512  ; 256, 512 or 128 (the latter only in FM mode)
CONF_DISK_TRACK_COUNT:        equ 160  ; 80 on each side. head address mapped into track

; Boot config
CONF_BOOT_LOCATION:           equ $2200 ; default location of the bootloader
CONF_BOOT_SIGNATURE:          equ $BEEF



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
MON_COM_EXEC:       equ 'x'
MON_COM_EXEC_JUMP:  equ '!'
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
MON_STRING:             equ $22

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
    jp selectDisk
    jp seek
    jp readData
    jp writeData
    jp extendedFunc
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
    
    ld SP, (RAM_END+1) % $10000   ; init stackpointer to end of memory
    ld HL, main
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
    ld A, (HL)
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
    ld A, (HL)
    or A
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
    ld A, (HL)
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
    ex DE, HL
    push DE
    ld D, H
    ld E, L
    add HL, HL ; shift left by 3 ( temp = value * 8)
    add HL, HL
    add HL, HL
    add HL, DE ; add two times ( temp = temp + 2* value)
    add HL, DE
    pop DE
    ex DE, HL
    
    ; add loaded digit to DE
    ld B, A
    ld A, E
    add B
    ld E, A
    ld A, D ; add carry bit to D
    adc A, 0
    ld D, A
    
    inc HL
    ld A, (HL)
    or A
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
    
    
   
; increments HL while (HL) is a whitespace character
skipWhites:
    ld A,(HL)
    cp TERM_SPACE ; check if char is == SPACE
    ret nz
    ;char was == SPACE -> skip char
    inc HL
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
    ld A, (HL) ; chars remaining that are valid expression syntax?
    or A
    jp z, _expression_end
    cp '+'
    jp z, _expression_add
    cp '-'
    jp nz, _expression_end
    
    push DE
    inc HL
    call _expression_term
    jp c, _expression_errorStackFix  ; syntax error
    ld (DAT_EXPR_WORDSTOR), HL ; save HL
    pop HL
    call subtractHLDE
    ex DE, HL
    ld HL, (DAT_EXPR_WORDSTOR) ; restore HL
    jp _expression_loop
    
_expression_add:
    push DE
    inc HL
    call _expression_term
    jp c, _expression_errorStackFix ; syntax error
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
    call _expression_factor
    ret c

_expression_term_loop:
    ld A, (HL) ; chars remaining that are valid term syntax?
    or A
    jp z, _expression_factor_end  ; skips whites, resets carry and returns
    cp '*'
    jp nz, _expression_factor_end
    ; No division yet supported. I don't see how that would be useful    

    push DE
    inc HL
    call _expression_factor
    jp c, _expression_errorStackFix  ; syntax error
    ld (DAT_EXPR_WORDSTOR), HL ; save HL
    pop HL
    call multHLDE
    ex DE, HL
    ld HL, (DAT_EXPR_WORDSTOR) ; restore HL
    jp _expression_term_loop
    



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
    call expression
    ret c ; syntax error
    ld A, (HL)
    cp ')'    ; expect closing parentheses
    jp nz, setCarryReturn ; syntax error
    inc HL
    jp _expression_factor_end
    
_expression_factor_answer:
    inc HL
    ld DE, (DAT_EXPR_ANSWER)
    
_expression_factor_end:
    call skipWhites
    jp resetCarryReturn

_expression_errorStackFix:
    inc SP  ; don't touch any registers. wont affect flags
    inc SP
    ret


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



; HL = HL * DE. DE is not affected. Result is truncated to 16 bit.
multHLDE:
    push BC
    ld C, L
    ld A, H
	ld B, 16
_multHLDE_loop:
	add HL, HL
	sla C
	rla
	jp nc, _multHLDE_noAdd
	add HL, DE
_multHLDE_noAdd:
	djnz _multHLDE_loop
    pop BC
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
    


; The following methods provide a disk interface for the monitor jump table

; Selects and recalibrates the disk given by A
selectDisk:
    ld (DAT_DISK_NUMBER), A
    call fdc_recalibrate
    ret



; Seeks the currently selected disk to the track given by A. All even track are mapped
;  to side 0 of the disk while all odd tracks reside on side 0. This way head addresses
;  can be ignored and double the amount of tracks assumed instead.
seek:
    ld (DAT_DISK_TRACK), A
    call fdc_seek
    ret



; Reads the sector given by A to the location given by HL.
readData:
    ld (DAT_DISK_SECTOR), A
    ld (DAT_DISK_DATAPTR), HL
    ld HL, fdc_readData
    call fdc_commandWithRetry
    ret



; Writes the data pointed to by HL to the sector given in A
writeData:
    ld (DAT_DISK_SECTOR), A
    ld (DAT_DISK_DATAPTR), HL
    ld HL, fdc_writeData
    call fdc_commandWithRetry
    ret



; Prints char in A if it is printable ASCII. Else, prints '.'
printPrintable:
    cp $20
    jp m, _printPrintable_noPrint ; char is < $20
    cp $7F
    jp p, _printPrintable_noPrint ; char is >= $7F
    jp printChar
_printPrintable_noPrint:
    ld A, '.'
    jp printChar



; Extended jump table call. Takes desired function in A.
;  Returns with carry set in case of error.
extendedFunc:
    cp $00 ; get version
    jp z, _extended_version

    cp $10 ; print time
    jp z, _extended_printTime

    ; invalid function. return with error
    jp setCarryReturn

_extended_version:
    ld A, CONF_VERSION
    jp resetCarryReturn

_extended_printTime:
    call rtc_printTime
    jp resetCarryReturn



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
    call printNewLine
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

    call skipWhites ; skip whitespace at beginning of input
    
    ld A, (HL) 
    or A
    jp z, monitorPrompt_loop  ; user entered nothing. prompt again
    
    call printNewLine ; insert a new line after user entered a command

    ; process user input
    ld B, (HL) ; load fist byte entered
    inc HL ; move HL to next byte
    
    call skipWhites
    
    ld A, B
    
    ; determine entered command
    cp MON_COM_BOOT
    jp z, command_boot
    
    cp MON_COM_LOAD
    jp z, command_load 
    
    cp MON_COM_EXEC
    jp z, command_execute
    
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
    ld A, (HL) ; chars remaining?
    or A
    jp nz, _command_print_loop
    
    jp monitorPrompt_loop



    include commands/com_execute.asm

    include commands/com_register.asm
   
    include commands/com_load.asm 

    include commands/com_boot.asm

    include commands/com_examine.asm    

    include commands/com_store.asm
    
    include commands/com_copy.asm
    
    include commands/com_hex_input.asm

    include commands/com_hex_output.asm

    include commands/com_disk.asm



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

shovelknight_ram:     equ DAT_SK_BUFFER    ; where to store monitor ROM before disabling ROM mapping
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

    ret ; shovelknight is done

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
; NOTE: the order of the following areas is important for the disk info command
DAT_DISK_NUMBER:           ds 1  ; number of currently selected drive
DAT_DISK_TRACK:            ds 1  ; logical track. these run from 0 to 159 and are mapped with even track on side 0
DAT_DISK_TRACK_PHYS:       ds 1  ; physical track used in conjunction with head number
DAT_DISK_HEAD:             ds 1  ; calculated by seek command.
DAT_DISK_SECTOR:           ds 1
; order sensitive area ends here
DAT_DISK_INT_SR0:          ds 1  ; used by FDC ISR to store result bytes of SENSEI command
DAT_DISK_INT_CYLINDER:     ds 1  ;   "
DAT_DISK_FILLER:           ds 1  ; used by format command
DAT_DISK_RES_BUFFER:       ds 8  ; used to store result bytes (max. 7, one slack)
DAT_DISK_DATAPTR:          ds 2
DAT_DISK_COMMAND:          ds 2
DAT_DISK_RETRIES:          ds 1
DAT_MON_REG_BUFFER:        ds 16
DAT_SK_BUFFER:
DAT_INPUT_BUFFER:          ds MON_INPUT_BUFFER_SIZE ; command line input buffer in HIMEM
DAT_DISK_DATABUFFER:       ds CONF_DISK_BYTES_PER_SECTOR  ; this may grow downwards quite a bit. always keep last


    end main



    
    