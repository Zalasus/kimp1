

;---------------------------- RTC ACCESS ------------------------------

; Initializes the RTC, including the IVR etc.
rtc_init:
    ld A, $01  ; disable RTC interrupt (mask bit = 1)
    out (IO_RTC_CE), A

    ld A, $ff  ; rst 38h  
    out (IO_IVR_RTC), A

    ; reset counter and callback
    ld HL, $0000
    ld (DAT_RTC_CALLBACK), HL
    xor A
    ld (DAT_RTC_COUNTER), A

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
;  equals A*1/64 seconds +/- a few clock cycles. Will enable interrupts.
;  Due to the nature of the RTC interrupt, the first of the counted intervals
;  can be anything from 0 to 1/64 seconds.
rtc_delay:
    ld (DAT_RTC_COUNTER), A

    ld A, $02  ; interrupt mode, 1/64 interval, mask bit = 0
    out (IO_RTC_CE), A
    
    ei
_rtc_delay_loop:
    ld A, (DAT_RTC_COUNTER)
    or A
    jp nz, _rtc_delay_loop
    
    ld A, $01  ; mask bit = 1
    out (IO_RTC_CE), A

    ret



; Sets up the RTC to wait for a time given by A as a number
;  of 1/64 second intervals, stores the address in DE, then returns.
;  Once the set time has passed, a call to the stored address is made
;  and the timer is deactivated. Will activate interrupts.
;  Due to the nature of the RTC interrupt, the first of the counted intervals
;  can be anything from 0 to 1/64 seconds.
;  If this is called when another timeout is already pending, the old one will be
;  ignored and the callback never made.
rtc_setTimeout:
    ld (DAT_RTC_COUNTER), A
    
    ld A, $02  ; interrupt mode, 1/64 interval, mask bit = 0
    out (IO_RTC_CE), A

    ld (DAT_RTC_CALLBACK), DE

    ld A, $02  ; interrupt mode, 1/64 interval, mask bit = 0
    out (IO_RTC_CE), A

    ei

    ret



; Disables the RTC interrupt and resets the interval counter
;  and callback address
rtc_deleteTimeout:
    ld A, $01  ; mask bit = 1
    out (IO_RTC_CE), A

    ; reset counter and callback
    ld HL, $0000
    ld (DAT_RTC_CALLBACK), HL
    xor A
    ld (DAT_RTC_COUNTER), A

    ret



; Disables RTC interrupts but will not delete a pending timeout.
;  Won't affect the CPU interrupt settings
rtc_disableInterrupt:
    ld A, $01  ; mask bit = 1
    out (IO_RTC_CE), A
    ret
    


; Enables 1/64 second interrupt
rtc_enableInterrupt:
    ld A, $02  ; interrupt mode, 1/64 interval, mask bit = 0
    out (IO_RTC_CE), A
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
    jp nz, _rtc_isr_end

    ; counter hit zero. if there's a callback, call it
    ld HL, (DAT_RTC_CALLBACK)
    ld A, H
    or L
    jp z, _rtc_isr_end  ; no callback
    
    ; make a call to callback
    ld DE, _rtc_isr_callback_end
    push DE
    jp (HL)
_rtc_isr_callback_end:
    call rtc_deleteTimeout    

_rtc_isr_end:   
    exx
    ex af, af'
    ei
    ret


    



