
;-----------------------------------
;           Defition file
;       for the KIMP1 system
;
;      Copyleft 2016  Zalasus
;-----------------------------------


CPU_SPEED: equ 2457600 ; 2.4576 MHz default clock

ROM_END:   equ $2000 ; adress of first byte NOT in ROM anymore
RAM_END:   equ $FFFF ; last byte in RAM



;-------------82C51 USART-------------
IO_UART_DAT: equ $00 ; UART Data Register
IO_UART_COM: equ $01 ; UART Command/Mode register

; command register bits
BIT_UART_TXEN:        equ 0 
BIT_UART_DTR:         equ 1
BIT_UART_RXEN:        equ 2
BIT_UART_SEND_BREAK:  equ 3
BIT_UART_RESET_ERROR: equ 4
BIT_UART_RST:         equ 5 ; Request To Send, not Reset
BIT_UART_INT_RESET:   equ 6
BIT_UART_HUNT_MODE:   equ 7 ; unused in async mode

; status register bits
BIT_UART_TXRDY:       equ 0
BIT_UART_RXRDY:       equ 1
BIT_UART_TXEMPTY:     equ 2
BIT_UART_PARITY_ERR:  equ 3
BIT_UART_OVERRUN_ERR: equ 4
BIT_UART_FRAMING_ERR: equ 5
BIT_UART_SYNDET:      equ 6
BIT_UART_DSR:         equ 7


;-------------82C53 PIT-------------
IO_PIT_C0:   equ $10 ; PIT Timer Register 0
IO_PIT_C1:   equ $11 ; PIT Timer Register 1
IO_PIT_C2:   equ $12 ; PIT Timer Register 2
IO_PIT_CTRL: equ $13 ; PIT Control Register (write only)



;-------------Tape Counter Control Register-------------
IO_TCCR:             equ $20

BIT_ROM_GATE:        equ 0
BIT_IO_RESET:        equ 1 ; write only
BIT_TAPE_SENSE:      equ 1 ; read only
BIT_C0_GATE:         equ 2
BIT_C1_GATE:         equ 3
BIT_C2_GATE:         equ 4
BIT_C1_INT_ENABLE:   equ 5
BIT_TAPE_MOTOR:      equ 6 ; write only
BIT_C0_OUT:          equ 6 ; read only
BIT_TAPE_DATA_WRITE: equ 7 ; write only
BIT_TAPE_DATA_READ:  equ 7 ; read only
; 0 in this mask means the respective bit is not the same meaning when reading 
;  from than when writing to TCCR
IO_TCCR_WRITE_MASK:  equ $3D


;-------------WD37C65 FLOPPY CONTROLLER-------------
IO_FDC_STAT: equ $30
IO_FDC_DATA: equ $31
IO_FDC_OPER: equ $32
IO_FDC_CONT: equ $33

; operation register bits
BIT_DRIVE_SELECT:      equ 0
BIT_SOFT_RESET:        equ 2
BIT_DMA_ENABLE:        equ 3
BIT_MOTOR_ON_ENABLE_1: equ 4
BIT_MOTOR_ON_ENABLE_2: equ 5
BIT_MODE_SELECT:       equ 7

; status register 0 bits
BIT_FDD0_BUSY:          equ 0
BIT_FDD1_BUSY:          equ 1
BIT_FDD2_BUSY:          equ 2
BIT_FDD3_BUSY:          equ 3
BIT_FDC_BUSY:           equ 4
BIT_EXCEC_MODE:         equ 5
BIT_DATA_INPUT:         equ 6
BIT_REQUEST_FOR_MASTER: equ 7

; status register 1 bits
BIT_UNIT_SELECT_2:   equ 0
BIT_UNIT_SELECT_1:   equ 1
BIT_HEAD_SELECT:     equ 2
BIT_NOT_READY:       equ 3
BIT_EQUIPMENT_CHECK: equ 4
BIT_SEEK_END:        equ 5
BIT_INTERRUPT_CODE:  equ 7

; status register 2 bits
BIT_MISSING_ADRESS_MARK: equ 0
BIT_NOT_WRITEABLE:       equ 1
BIT_NO_DATA:             equ 2
BIT_OVERRUN:             equ 4
BIT_DATA_ERROR:          equ 5
BIT_END_OF_CYLINDER:     equ 7

; status register 3 bits
BIT_MISSING_ADRESS_MARK_IN_DATA_FIELD: equ 0
BIT_BAD_CYLINDER:                      equ 1
BIT_SCAN_NOT:                          equ 2
BIT_SCAN_EQUAL:                        equ 3
BIT_WRONG_CYLINDER:                    equ 4
;BIT_DATA_ERROR:                        equ 5
BIT_CONTROL_MARK:                      equ 6

; master status register
;BIT_UNIT_SELECT_2:          equ 0
;BIT_UNIT_SELECT_1:          equ 1
;BIT_HEAD_SELECT:            equ 2
BIT_WRITE_PROTECTED:        equ 3
BIT_TRACK_0:                equ 4
BIT_READY:                  equ 5
BIT_WRITE_PROTECTED_LEGACY: equ 6









