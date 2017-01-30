
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

    include "../kimp1def.inc"



;-------------- PRE-ASSEMBLY CONFIGURATION -------------------

; General configuration 
CONF_INCLUDE_HELP:     equ 1    ; set to zero to save a few bytes of ROM
CONF_RESET_ON_STARTUP: equ 0    ; will clear memory on startup if set to one
CONF_STARTUP_DELAY:    equ 1    ; will delay approx. 1 sec on startup

; UART configuration (data format is always 8 data, 1 stop, no parity)
CONF_UART_BAUDRATE: equ 9600
CONF_UART_PRESCALE: equ 1      ; possible values are 1, 16 and 64

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
MON_COM_SOFTRESET:  equ 'r'

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



; --------------- DATA AREAS --------------------

DAT_INPUT_BUFFER:          equ ROM_END ; command line input buffer in HIMEM
DAT_INPUT_BUFFER_SIZE:     equ $100 ; 256 bytes
DAT_MON_REG_BUFFER:        equ DAT_INPUT_BUFFER + DAT_INPUT_BUFFER_SIZE
DAT_EXPR_WORDSTOR:         equ DAT_MON_REG_BUFFER + 16 ; single word storage for expression parser
DAT_EXPR_ANSWER:           equ DAT_EXPR_WORDSTOR + 2 ; memory (word) for last parsed expression
DAT_RTC_COUNTER:           equ DAT_EXPR_ANSWER + 2
DAT_DISK_IRQFLAG:          equ DAT_RTC_COUNTER + 1
DAT_DISK_DATAPTR:          equ DAT_DISK_IRQFLAG + 1
DAT_DISK_DATACOUNT:        equ DAT_DISK_DATAPTR + 2
DAT_DISK_DATABUFFER:       equ DAT_DISK_DATACOUNT + 2



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

    org $0030   ; rst 30h  ($f7)
        ; unused restart vector. simply return
        ret



    org $0038   ; rst 38h  ($ff)
        ; restart vector used by RTC IRQ
        ;  since the RTC-IVR is shared by both RTC and OPL, we need to
        ;  determine the exact cause for the interrupt here and jump to
        ;  the right handler. RTC has priority over OPL
        in A, (IO_EBCR)
        bit BIT_EBCR_IRQ_RTC, A
        jp z, rtc_isr
        bit BIT_EBCR_IRQ_OPL, A
        jp z, opl_isr
        ret



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

    ld HL, RAM_END ; init stackpointer to end of memory
    ld SP,HL

    ld HL, $0000  ; catch any stack underflows
    push HL

    ; init interrupt handlers
    ;  we use IM0~ the 19 clock cycle penalty for IM2 is way too heavy for disk access
    im 0
    ; since the IVRs have no reset, we need to initialize them to some known
    ;  state, in this case a simple NOP
    xor A
    out (IO_IVR_FDC), A
    out (IO_IVR_RTC), A

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
    ld A, H
    cp high RAM_END
    jp nz, _soft_reset_loop
    ld A, L
    cp low RAM_END
    jp nz, _soft_reset_loop
    
    
    jp monitorStart ; jump to monitor setup
    

    
;----------------------------STRING DATA-------------------------------

; NOTE: Use only LF ($0A) for linefeeds for saving ROM / portability. 
; The printing routine substitutes CRLF when needed.

str_welcome:
    db 'MINIMON 0.3 FOR KIMP1', $0A
    db 'COPYLEFT 2017 ZALASUS', $0A
    db ' ALL WRONGS REVERSED', $0A, $00
    
str_pressPlayOnTape:
    db 'PRESS PLAY ON TAPE', $0A, $00
    
str_loading:
    db 'LOADING. PLEASE WAIT...', $0A, $00
    
str_unknownCommand:
    db 'UNKNOWN COMMAND', $0A, $00
    
str_syntaxError:
    db 'SYNTAX ERROR', $0A, $00
    
str_diskError:
    db 'DISK ERROR', $0A, $00

str_fdcError:
    db 'DISK CONTROLLER ERROR', $0A, $00

str_noExtPresent:
    db 'NO '
str_extPresent:
    db 'EXTENSION BOARD PRESENT', $0A, $00

str_readingHex:
    db 'READING HEX...', $0A, $00

str_hexOK:
    db 'HEX OK', $0A, $00

str_hexError:
    db 'HEX ERROR', $0A, $00

str_hexChecksumError:
    db 'HEX CHECKSUM ERROR', $0A, $00

str_hexRecordEof:
    db ':00000001FF', $0A, $00

str_notImplemented:
    db 'NOT IMPLEMENTED CAUSE ZAL IS A LAZY ASS', $0A, $00

str_help:
if CONF_INCLUDE_HELP == 0
    db 'NO HELP AVAILABLE', $0A, $00
else
    db 'First character of input denotes command' , $0A
    db 'Whitespace is always optional', $0A
    db 'Defined commands:', $0A
    db 'e S [,E]   Examine address S to E', $0A
    db 's X        Store to address X', $0A
    db 'x[!] X     Call to address X. Use x! to jump to X instead', $0A
    db 'f          Print flags and registers as returned by last call', $0A
    db 'c S, D, C  Copy C bytes from S to D', $0A
    db 'l X        Load from tape to address X', $0A
    db 'b          Boot from floppy', $0A
    db 'v          Show version', $0A
    db 'h          Show this message', $0A
    db 'p X        Parses and prints X', $0A
    db 'i          Starts reading of Intel HEX', $0A
    db 'o S [,E]   Dumps S to E as Intel HEX', $0A
    db 'r          Soft reset', $0A
    db 'Arguments in square brackets optional', $0A
    db 'Math expressions in arguments are possible. Allowed: + - ( )', $0A
    db '$ is the last parsed number', $0A
    db 'Numbers interpreted as hexadecimal', $0A
    db 'Prefix with # for decimal', $0A, $00
endif
    
str_cls:
    db $1B, '[2J', $00 ; the VT100 way to clear screen
    

    
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

; drive parameter table
fdc_dat_bps:
    db $03



; Initializes the FDC to AT mode, setting data rate etc.
;  This will initialize the FDC-IVR to a NOP.
fdc_init:
    ; initialize IVR to a NOP since all routines just wait for
    ;  IRQs using halt mode.
    xor A
    out (IO_IVR_FDC), A

    ld HL, $0000
    ld (DAT_DISK_DATAPTR), HL
    ld (DAT_DISK_DATACOUNT), HL

_fdc_reset:
    ; issue a soft reset to make sure FDC is in reset mode
    xor A
    out (IO_FDC_OPER), A

    ; once reset is finished, controller will interrupt
    ei
    hlt

    ; controller is now reset
    ;  write to OP once again, leaving /SRST high, to initialize AT mode
    ld A, [1 << BIT_FDC_SOFT_RESET]
    out (IO_FDC_OPER), A
    
    ; initialize data rate
    ; 125Kb/s @ FM for 16MHz default clock
    ld A, [1 << BIT_FDC_DATARATE1] 
    out (IO_FDC_CONT), A
    
    ret

    

; Recalibrates the drive specified by two LSbs of B. Drive is moved 
;  to track 0 and controller internal sector counters are reset.
;  If no errors occur, the carry bit is reset or set otherwise.
fdc_recalibrate:
    call fdc_preCommandCheck  ; make sure FDC accepts commands
    ld A, $07 ; recalibrate command
    out (IO_FDC_DATA), A

    call fdc_waitForRFM
    jp c, setCarryReturn  ; direction must still be input~ reclibrate requires 2 bytes
    ld A, B
    and $03 ; we only need the two lower bits for the drive number
    out (IO_FDC_DATA), A
    
    ; the FDC is stepping the drive now. we need to wait until stepping is finished
    ;  and head has has reached track 0.
    ei
    hlt

    ; stepping is finished.
    ; next, read SR0 to check if drive has successfully reached track 0
    call fdc_senseInterruptStatus
    ret c
    
    bit BIT_FDC_SEEK_END, B
    jp z, setCarryReturn
    bit BIT_FDC_EQUIPMENT_CHECK, B
    jp nz, setCarryReturn
    
    jp resetCarryReturn



; Sets drive parameters for the currently selected unit
;  Uses fixed parameters for step rate and load time that should provide
;  stable operation (Load = 80ms, Step = 10ms, Unload = 16ms). DMA mode is disabled.
fdc_specify:
    call fdc_preCommandCheck
    ld A, $03   ; specify command
    
    call fdc_waitForRFM
    jp c, setCarryReturn
    ld A, $51   ; unload time and step rate
    out (IO_FDC_DATA), A    

    call fdc_waitForRFM
    jp c, setCarryReturn
    ld A, [$28 << 1] | 1 ; load time and Non-DMA-Bit (set)
    out (IO_FDC_DATA), A

    ; no exec or result phase

    ret



; Selects and enables motor of drive specified by LSb of B and waits the appropriate delay for
;  spinning up to speed. Disables motor of other drive. If B > 1, both motors are deactivated.
;  There's no spindown time if both motors are deactivated.
fdc_driveSelect:
    in A, (IO_FDC_OPER)
    and ~[[1 << BIT_FDC_MOTOR_ON_ENABLE_1] | [1 << BIT_FDC_MOTOR_ON_ENABLE_2]] ; turn off both motor bits

    ld A, B
    and $fe
    jp nz, _fdc_driveSelect_disableBoth ; B is > 1. we're done
    
    bit 0, B
    jp nz, _fdc_driveSelect_drive2

    set BIT_FDC_MOTOR_ON_ENABLE_1, A
    res BIT_FDC_DRIVE_SELECT, A
    jp _fdc_driveSelect_spinup

_fdc_driveSelect_drive2:
    set BIT_FDC_MOTOR_ON_ENABLE_2, A
    set BIT_FDC_DRIVE_SELECT, A

_fdc_driveSelect_spinup:
    out (IO_FDC_OPER), A

    ; recommended spinup time is 500ms, which equals 32 RTC delay cycles
    ld A, 32
    call rtc_delay

    ret

_fdc_driveSelect_disableBoth:
    out (IO_FDC_OPER), A
    ret


    
; Loads status register 0 from controller. ST0 will be stored in B, the current cylinder
;  index of the drive will be stored in A.
;  Carry used as error bit.
fdc_senseInterruptStatus:
    call fdc_preCommandCheck
    ld A, $08
    out (IO_FDC_DATA), A
    ; command issued. this command has no execution phase and won't produce
    ;  an interrupt. data can be read back right away (hopefully)

    ; read ST0
    call fdc_waitForRFM
    jp nc, setCarryReturn   ; FDC should have two bytes to read now
    in A, (IO_FDC_DATA)
    ld B, A

    ; read current cylinder index
    call fdc_waitForRFM
    jp nc, setCarryReturn
    in A, (IO_FDC_DATA)

    ret
 


; Checks initial condition for issuing commands to the FDC. Waits for 
;  RQM signal and checks if DIO signal is set to "input".
;  If not, this routine will reset the controller and try again. This will bring the 
;  controller in the right state eventually or just hang forever.
;  TODO: Might check for busy bit also
fdc_preCommandCheck:
    call fdc_waitForRFM
    ret c  ; if carry set -> FDC awaits input. all ready

    call _fdc_reset
    jp fdc_preCommandCheck



; Waits until the FDC reports a Request For Master. After that, it checks the data direction
;  bit. If it is is set to "waiting for input" the carry flag is reset, if bit is set to "data available"
;  carry bit is set.
fdc_waitForRFM:
    in A, (IO_FDC_STAT)
    bit BIT_FDC_REQUEST_FOR_MASTER, A
    jp z, fdc_waitForRFM

    bit BIT_FDC_DATA_INPUT, A
    jp nz, resetCarryReturn ; DIO = 1 -> data register expects to be read
    jp setCarryReturn



; Prints FDC error message and returns
fdc_error:
    ld HL, str_fdcError
    call printString
    ret



; Prints Floppy Disk Error message and returns
fdc_diskError:
    ld HL, str_diskError
    call printString
    ret
    


; Experimental half polling/half IRQ routine for read data transfer. Uses interrupts
;  in mode 0 to wait for the FDC. Transfers data read from the FDC to the buffer pointed
;  by (HL), growing upwards. Leaves interrupts disabled when finished.
;  Trashes B,C,D. When finished, HL points to the location AFTER where the last byte was stored.
fdc_rtranfer:
    ; Since the FDC-IVR contains a NOP, we can use the HLT instruction in IM0 to wait for the IRQ
    ;  without the cycle penalty of calling an ISR. When HLT is lifted, the CPU will execute a NOP
    ;  for 6 cycles and then continue after the HLT. This is faster and more elegant than polling the MSR
    di
    ; TODO: maybe we should make sure no RTC interrupts can occur here
    
    ld D, [1 << BIT_FDC_EXEC_MODE] ; constant for bit masking. used instead of immediate value for speed
    ld C, IO_FDC_DATA  ; IO port constant for ini instruction (again used for speed)

_fdc_rtransfer_loop:
    ei
    hlt
    ; HALT was lifted~ check for interrupt cause
    in A, (IO_FDC_STAT)
    and D  ; [1 << BIT_FDC_EXEC_MODE], use D as constant for speed
    ret z  ; if not in exec mode anymore, we're done
    ini
    jp _fdc_rtransfer_loop



; Same as routine above, but writes data read from buffer at (HL) to FDC instead.
fdc_wtranfer:
    di
    ; TODO: maybe we should make sure no RTC interrupts can occur here
    
    ld D, [1 << BIT_FDC_EXEC_MODE] ; constant for bit masking. used instead of immediate value for speed
    ld C, IO_FDC_DATA  ; IO port constant for outi instruction (again used for speed)

_fdc_wtransfer_loop:
    ei
    hlt
    ; HALT was lifted~ check for interrupt cause
    in A, (IO_FDC_STAT)
    and D  ; [1 << BIT_FDC_EXEC_MODE], use D as constant for speed
    ret z  ; if not in exec mode anymore, we're done
    outi
    jp _fdc_wtransfer_loop



; Waits for approximately 15us
;  (recommended but not required before reading MSR in command phase~
;   we use polling instead. might remove routine if no other useful application is found)
fdc_delay:
    ld A, (CPU_SPEED/67000)/14  ; 1/15us = 67kHz, delay loop is 14 cycles long

_fdc_delay_loop:
    dec A
    jp nz, _fdc_delay_loop

    ret



;---------------------------- RTC ACCESS ------------------------------------    

; Initializes the RTC, including the IVR etc.
rtc_init:
    ld A, $01  ; disable RTC interrupt (mask bit = 1)
    out (IO_RTC_CE), A

    ld A, $ff  ; rst 38h  
    out (IO_IVR_RTC), A

    ret
    

; A test routine to print the current time to console. No LF is appended so CR can be used
rtc_printTime:
    in A, (IO_RTC_H10)
    and $03  ; mask out AM/PM bit
    add '0'
    call printChar
    
    in A, (IO_RTC_H1)
    and $0f ; 4-bit-device!!
    add '0'
    call printChar
    
    ld A, ':'
    call printChar
    
    in A, (IO_RTC_MI10)
    and $0f
    add '0'
    call printChar

    in A, (IO_RTC_MI1)
    and $0f
    add '0'
    call printChar    
    
    ld A, ':'
    call printChar
    
    in A, (IO_RTC_S10)
    and $0f
    add '0'
    call printChar

    in A, (IO_RTC_S1)
    and $0f
    add '0'
    call printChar

    ld A, TERM_SPACE
    call printChar

    ; if 24 Hour bit is not set, print AM/PM-Indicator. Print two rubouts otherwise
    in A, (IO_RTC_CF)
    bit BIT_RTC_24_12, A
    jp z, _rtc_printTime_ampm

    ld A, TERM_RUBOUT
    call printChar
    ld A, TERM_RUBOUT
    call printChar
    jp _rtc_printTime_done

_rtc_printTime_ampm:
    in A, (IO_RTC_H10)
    bit BIT_RTC_PM_AM, A
    jp z, _rtc_printTime_am
    ld A, 'P'
    jp _rtc_printTime_ampm_done
_rtc_printTime_am:
    ld A, 'A'
_rtc_printTime_ampm_done:
    call printChar
    ld A, 'M'
    call printChar

_rtc_printTime_done:
    ret



; Uses the RTC interrupt to delay a time interval given by A. The delay time
;  equals A*1/64 seconds +/- a few clock cycles. Will disable interrupts once finished.
rtc_delay:
    di

    ld (DAT_RTC_COUNTER), A

    ld A, $02  ; interrupt mode, 1/64 interval, mask bit = 0
    out (IO_RTC_CE), A

    ei

_rtc_delay_loop:
    hlt
    ld A, (DAT_RTC_COUNTER)
    or A
    jp nz, _rtc_delay_loop
    
    ld A, $01  ; mask bit = 1
    out (IO_RTC_CE), A

    di

    ret

    

; Interrupt handler for the timed interval interrupt by the RTC
rtc_isr:
    exx
    ex af,af'

    ; clear the irq flag
    in A, (IO_RTC_CD)
    and ~[1 << BIT_RTC_IRQ_FLAG]
    out (IO_RTC_CD), A
    
    ld A, (DAT_RTC_COUNTER)
    dec A
    ld (DAT_RTC_COUNTER), A

    exx
    ex af, af'
    ei
    ret



;-------------------------- SOUND CHIP ACCESS ------------------------------- 

; Initializes OPL sound chip. Does NOT initialize IVR since the IVR is primarily
;  used by the RTC. Use RTC initialization routine to initialize IVR.
opl_init:
    ret



; Interrupt service routine for the OPL timer interrupt
opl_isr:
    ; TODO: Clear OPL interrupt flag here
    ei
    ret



;---------------------- PARSER & FORMATTER METHODS --------------------------  
           
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
    
    ld HL, $0002 ; stack pointer is two bytes to low when calling this. fix that manually
    add HL, SP
    ld (DAT_MON_REG_BUFFER+8), HL

    push AF
    pop HL
    ld (DAT_MON_REG_BUFFER), HL

    ld A, I
    ld (DAT_MON_REG_BUFFER+14), A
    
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
    call ext_test
    jp nc, _monitor_init_noext
    call fdc_init
    call rtc_init
    call opl_init
_monitor_init_noext:
    
    call clearScreen
  
monitor_welcome:
    ld HL, str_welcome ; print welcome message
    call printString

    ; check if extension board is present again and print message
    call printNewLine
    call ext_test
    jp nc, _monitor_welcomeExt_noext
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
    jp Z, monitorPrompt_loop ; user entered nothing. prompt again
    
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

    ex DE, HL
    jp (HL)
    
_command_run_cleanup:
    ; we end up here when called routine returns
    ;  stash register file in buffer to be examined by Register command
    call stashRegisters

    jp monitorPrompt_loop



; Prints the saved register buffer in human readable format
;  NOTE: Quick and dirty routine. Surely to be compacted somehow.
command_register:
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
    call ext_test
    jp c, _command_boot_cont

    ; board is not present. print error
    ld HL, str_noExtPresent
    call printString
    jp monitorPrompt_loop

_command_boot_cont:
    ld HL, str_notImplemented
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
    jp nz, _hex_error
    
    ; got expected checksum. we're done reading hex file
    ld HL, str_hexOK
    call printString

    jp monitorPrompt_loop


_hex_checksum_error
    ld HL, str_hexChecksumError
    call printString

    jp monitorPrompt_loop

_hex_error:
    ld HL, str_hexError
    call printString

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

    end main



    
    