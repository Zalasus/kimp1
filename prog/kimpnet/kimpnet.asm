;---------------------------
;
;   ESP8266 modem driver
;        for KIMP1
;
;---------------------------


; This is an attempt to connect the KIMP1 to the internet
;  by means of an ESP8266 using it's default AT command 
;  firmware. Since the KIMP1 only has one UART, which is used
;  by the terminal, a second one has to be emulated in software
;  using one of the available I/O-Ports. This uses the cassette
;  port pins on the base board, so no hardware modification is
;  needed.


; Sends the byte stored in B via the emulated UART
uartSend:
    ; start bit is zero (high level)
    in A, (IO_TCCR)
    and IO_TCCR_WRITE_MASK
    or [1 << BIT_TCCR_TAPE_DATA_WRITE]
    out (IO_TCCR), A

    ; now we need to wait 256 cycles before sending the next bit

    
    