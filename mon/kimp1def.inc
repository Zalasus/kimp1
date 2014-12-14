
;-----------------------------------
;           Defition file
;       for the KIMP1 system
;
;    (C) 1979 Knifto Industries
;-----------------------------------


CPU_SPEED equ 2457600 ; 2.4576 MHz default clock

ROM_END equ $2000 ; adress of first byte NOT in ROM anymore

IO_UART_COM equ $00 ; UART Command/Mode register
IO_UART_DAT equ $01 ; UART Data Register
BIT_TXEN equ 0
BIT_RXEN equ 1

IO_PIT_C0   equ $08 ; PIT Timer Register 0
IO_PIT_C1   equ $09 ; PIT Timer Register 1
IO_PIT_C2   equ $0A ; PIT Timer Register 2
IO_PIT_CTRL equ $0B ; PIT Control Register

IO_TCCR equ $10 ; Tape and Computer Control Register
BIT_ROM_GATE equ 0
BIT_IO_RESET equ 1 ; write only
BIT_TAPE_SENSE equ 1 ; read only
BIT_C0_GATE equ 2
BIT_C1_GATE equ 3
BIT_C2_GATE equ 4
BIT_C1_INT_ENABLE equ 5
BIT_TAPE_MOTOR equ 6 ; write only
BIT_C0_OUT equ 6 ; read only
BIT_TAPE_DATA_WRITE equ 7 ; write only
BIT_TAPE_DATA_READ equ 7; read only
; 0 in this mask means the respective bit is not the same meaning when reading 
; from than when writing to TCCR
IO_TCCR_WRITE_MASK equ $3D