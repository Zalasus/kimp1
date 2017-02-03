
;-----------------------------------
;           Defition file
;       for the KIMP1 system
;
;      Copyleft 2016  Zalasus
;-----------------------------------


CPU_SPEED: equ 2457600 ; 2.4576 MHz default clock

ROM_END:   equ $2000 ; adress of first byte NOT in ROM anymore
RAM_END:   equ $FFFF ; last byte in RAM



;=============== CPU BOARD HARDWARE =================

;-------------82C51 USART-------------
IO_UART_DAT: equ $00 ; UART Data Register
IO_UART_COM: equ $01 ; UART Command/Mode register

; command register bits
BIT_UART_TXEN:        equ 0 
BIT_UART_DTR:         equ 1 ; 1=pin low, tied to status LED (1=led off)
BIT_UART_RXEN:        equ 2
BIT_UART_SEND_BREAK:  equ 3
BIT_UART_RESET_ERROR: equ 4
BIT_UART_RTS:         equ 5 ; 1=output low
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
BIT_UART_DSR:         equ 7 ;1=pin low, tied to button (1=button pressed)



;-------------82C53 PIT-------------
IO_PIT_C0:   equ $10 ; PIT Timer Register 0
IO_PIT_C1:   equ $11 ; PIT Timer Register 1
IO_PIT_C2:   equ $12 ; PIT Timer Register 2
IO_PIT_CTRL: equ $13 ; PIT Control Register (write only)



;-------------Tape Counter Control Register-------------
IO_TCCR:             equ $20

BIT_TCCR_ROM_GATE:        equ 0
BIT_TCCR_IO_RESET:        equ 1 ; write only
BIT_TCCR_TAPE_SENSE:      equ 1 ; read only
BIT_TCCR_C0_GATE:         equ 2
BIT_TCCR_C1_GATE:         equ 3
BIT_TCCR_C2_GATE:         equ 4
BIT_TCCR_C1_INT_ENABLE:   equ 5
BIT_TCCR_TAPE_MOTOR:      equ 6 ; write only
BIT_TCCR_C0_OUT:          equ 6 ; read only
BIT_TCCR_TAPE_DATA_WRITE: equ 7 ; write only
BIT_TCCR_TAPE_DATA_READ:  equ 7 ; read only
; 0 in this mask means the respective bit is not the same meaning when reading 
;  from than when writing to TCCR
IO_TCCR_WRITE_MASK:  equ $3D




;=============== EXTENSION BOARD HARDWARE =================

;-------------WD37C65 FLOPPY CONTROLLER-------------
IO_FDC_STAT: equ $30
IO_FDC_DATA: equ $31
IO_FDC_OPER: equ $34
IO_FDC_CONT: equ $38 ; write only

; Note: The register descriptions in the crap datasheet of the WD37C65 were
;  mostly incorrect. These bit assignments were obtained by comparing the WD37C65
;  datasheet with the (mostly) compatible NEC uPD765 and are assumed to be correct

; master status register bits
BIT_FDC_FDD0_BUSY:          equ 0
BIT_FDC_FDD1_BUSY:          equ 1
BIT_FDC_FDD2_BUSY:          equ 2
BIT_FDC_FDD3_BUSY:          equ 3
BIT_FDC_BUSY:               equ 4
BIT_FDC_EXEC_MODE:          equ 5
BIT_FDC_DATA_INPUT:         equ 6
BIT_FDC_REQUEST_FOR_MASTER: equ 7

; master status register 1
BIT_FDC_POWERDOWN:         equ 0

; operation register bits
BIT_FDC_DRIVE_SELECT:      equ 0
BIT_FDC_SOFT_RESET:        equ 2 ; active low
BIT_FDC_DMA_ENABLE:        equ 3
BIT_FDC_MOTOR_ENABLE_1:    equ 4
BIT_FDC_MOTOR_ENABLE_2:    equ 5
BIT_FDC_MODE_SELECT:       equ 7

; control register bits
BIT_FDC_DATARATE0:       equ 0
BIT_FDC_DATARATE1:       equ 1
BIT_FDC_DISABLE_PRECOMP: equ 2

; status register 0 bits
BIT_FDC_SR0_UNIT_SELECT_0: equ 0
BIT_FDC_SR0_UNIT_SELECT_1: equ 1
BIT_FDC_HEAD_SELECT:       equ 2
BIT_FDC_NOT_READY:         equ 3 ; always 0
BIT_FDC_EQUIPMENT_CHECK:   equ 4
BIT_FDC_SEEK_END:          equ 5
BIT_FDC_INTERRUPT_CODE0:   equ 6
BIT_FDC_INTERRUPT_CODE1:   equ 7

; status register 1 bits
BIT_FDC_MISSING_ADRESS_MARK: equ 0
BIT_FDC_NOT_WRITEABLE:       equ 1
BIT_FDC_NO_DATA:             equ 2
BIT_FDC_OVERRUN:             equ 4
BIT_FDC_DATA_ERROR:          equ 5
BIT_FDC_END_OF_CYLINDER:     equ 7

; status register 2 bits
BIT_FDC_MISSING_ADRESS_MARK_IN_DATA_FIELD: equ 0
BIT_FDC_BAD_CYLINDER:                      equ 1
BIT_FDC_SCAN_NOT:                          equ 2
BIT_FDC_SCAN_EQUAL:                        equ 3
BIT_FDC_WRONG_CYLINDER:                    equ 4
BIT_FDC_SR3_DATA_ERROR:                    equ 5
BIT_FDC_CONTROL_MARK:                      equ 6

; status register 3 bits
BIT_FDC_UNIT_SELECT_0:          equ 0
BIT_FDC_UNIT_SELECT_1:          equ 1
BIT_FDC_HEAD_SELECT:            equ 2
BIT_FDC_WRITE_PROTECTED:        equ 3 ; active low
BIT_FDC_TRACK_0:                equ 4
BIT_FDC_READY:                  equ 5 ; always 1
BIT_FDC_WRITE_PROTECTED_LEGACY: equ 6 ; active low



;---------------Control Registers--------------
IO_IVR_FDC:   equ $3C
IO_IVR_RTC:   equ $3D
IO_EBCR:      equ $3E

BIT_EBCR_TEST:            equ 0
BIT_EBCR_TC:              equ 2
BIT_EBCR_RTC_IRQ_INHIBIT: equ 4
BIT_EBCR_IRQ_FDC:         equ 5  ; active high
BIT_EBCR_IRQ_RTC:         equ 6  ; active low
BIT_EBCR_IRQ_OPL:         equ 7  ; active low



;--------------------MSM6242B RTC----------------------
; NOTE: This is a 4-bit device. When reading registers, the upper
;  nibble of the databus is undefined
IO_RTC_S1:       equ $40
IO_RTC_S10:      equ $41
IO_RTC_MI1:      equ $42
IO_RTC_MI10:     equ $43
IO_RTC_H1:       equ $44
IO_RTC_H10:      equ $45 ; NOTE: Bits 0-1: hour, Bit 2: PM/AM, Bit 3: 0
IO_RTC_D1:       equ $46
IO_RTC_D10:      equ $47
IO_RTC_MO1:      equ $48
IO_RTC_MO10:     equ $49
IO_RTC_Y1:       equ $4A
IO_RTC_Y10:      equ $4B
IO_RTC_W:        equ $4C
IO_RTC_CD:       equ $4D
IO_RTC_CE:       equ $4E
IO_RTC_CF:       equ $4F

; H10 bits
BIT_RTC_PM_AM:      equ 2 ; 0=AM,  1=PM

; CD bits
BIT_RTC_HOLD:       equ 0
BIT_RTC_BUSY:       equ 1 ; read-only
BIT_RTC_IRQ_FLAG:   equ 2 ; write 0 to reset, write 1 to leave unaffected
BIT_RTC_30_SEC_ADJ: equ 3

; CE bits
BIT_RTC_MASK:       equ 0
BIT_RTC_ITRPT_STND: equ 1 ; 0=7.8125ms pulse,  1=as long as irq is set
BIT_RTC_T0:         equ 2
BIT_RTC_T1:         equ 3

; CF bits
BIT_RTC_REST:       equ 0
BIT_RTC_STOP:       equ 1
BIT_RTC_24_12:      equ 2 ; 0=12h-mode,  1=24h-mode
BIT_RTC_TEST:       equ 3






