
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

IO_PIT_C0   equ $20 ; PIT Timer Register 0
IO_PIT_C1   equ $21 ; PIT Timer Register 1
IO_PIT_C2   equ $22 ; PIT Timer Register 2
IO_PIT_CTRL equ $23 ; PIT Control Register

IO_TCR equ $40 ; Tape Control Register
BIT_PLAY equ 0
BIT_DATA_READ equ 1
BIT_FALLING equ 2
BIT_DATA_WRITE equ 3
BIT_RECORD equ 4
BIT_DIRECTION equ 6
BIT_MOTOR equ 7

IO_CCR equ $60 ; Computer Control Register
BIT_ROM_GATE equ 0
BIT_IO_RESET equ 1
BIT_C0_GATE equ 2
BIT_C1_GATE equ 3
BIT_C2_GATE equ 4