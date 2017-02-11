

    org $0000


    include ../kimp1def.inc




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

    include ../mon/io_console.asm



;--------------STRINGS----------------

str_fdcInt:
    db '[FDC INT]', $0A, $00

str_rtcInt:
    db '[RTC INT]', $0A, $00

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
    ei
    ret

rtc_isr:
    ex AF, AF'
    exx

    ld HL, str_rtcInt
    call printString

    exx
    ex AF, AF'
    ei
    ret



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
    ld A, UART_MODE_INSTRUCTION
    out (IO_UART_COM), A 
    ; enable receiver and transmitter.
    ;  also, set /DTR of UART to 1. Indicator LED should turn off, giving visual feedback that CPU is alive
    ld A, [(1 << BIT_UART_TXEN) | (1 << BIT_UART_RXEN) | (1 << BIT_UART_DTR)]
    out (IO_UART_COM), A
    






