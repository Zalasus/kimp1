
    org $0000

    include "../kimp1def.inc"


CONF_UART_BAUDRATE: equ 9600
CONF_UART_PRESCALE: equ 1      ; possible values are 1, 16 and 64

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


    ; first, a "tiny" startup delay so everything can settle
    ld HL, 0
    xor A
_setup_loop:
    inc HL
    cp H
    jp nz, _setup_loop
    cp L
    jp nz, _setup_loop

    jp main


turnoff:
    ld A, 0+(1 << BIT_UART_TXEN) | (1 << BIT_UART_DTR)
    out (IO_UART_COM), A
    ret

turnon:
    ld A, 0+(1 << BIT_UART_TXEN)
    out (IO_UART_COM), A
    ret


conout:
    ld B, A
_conout_wait:
    in A, (IO_UART_COM) ; read in status byte of UART
    and 0+(1 << BIT_UART_TXRDY) ; mask out all bits except the TXRDY bit
    jp Z,_conout_wait ; do this until UART is ready
    
    ; UART is ready to send another byte now
    ld A,B
    out (IO_UART_DAT), A ; UART will start sending the byte now

    ret



main:
    ; init stackpointer to end of memory
    ld HL, RAM_END 
    ld SP,HL

    ; PIT 2 is connected to UART, so set it up for baud rate generation
    ld A, $B6 ; %10110110  ; set counter 2 in mode 3, binary counting
    out (IO_PIT_CTRL), A
    ; write divider value to counter (first LSB, then MSB as set in command above)
    ld A, UART_DIV_VAL & $00FF
    out (IO_PIT_C2), A
    ld A, UART_DIV_VAL >> 8
    out (IO_PIT_C2), A
    ; registers for counter are set. now we can gate the counter
    ld A, 0+(1 << BIT_C2_GATE)
    out (IO_TCCR), A ; C2 is now counting
    
    ; write mode byte to UART (first command byte after reset)
    ld A, UART_MODE_INSTRUCTION
    out (IO_UART_COM), A
    ; enable receiver and transmitter.
    ;  also, set /DTR of UART to 1. Indicator LED should turn off, giving visual feedback that CPU is alive
    ld A, 0+(1 << BIT_UART_TXEN) | (1 << BIT_UART_DTR)
    out (IO_UART_COM), A


loop:
    in A, (IO_UART_COM)
    and 0+(1 << BIT_UART_DSR)
    jp Z, off

on: 
    ld A, '1'
    call conout
    call turnon
    jp loop

off:
    ld A, '0'
    call conout
    call turnoff
    jp loop
    


diag_end:
    ; pad out file for maximum rom size
    ds 0+(8192 - diag_end), $ff

    end