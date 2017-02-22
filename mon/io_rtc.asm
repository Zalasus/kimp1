

;---------------------------- RTC ACCESS ------------------------------

; Initializes the RTC, including the IVR etc.
rtc_init:
    ld A, $03  ; disable RTC interrupt (mask bit = 1, int mode)
    out (IO_RTC_CE), A
    xor A      ; make sure irq line is deactivated
    out (IO_RTC_CD), A 
    ld A, [1 << BIT_RTC_24_12]  ; disable TEST bit and select 24h mode
    out (IO_RTC_CF), A

    ld A, IVR_RTC
    out (IO_IVR_RTC), A

    ; reset counter and callback
    ld HL, $0000
    ld (DAT_RTC_CALLBACK), HL
    xor A
    ld (DAT_RTC_COUNTER), A

    ret
    

; A test routine to print the current time to console. No LF is appended so CR can be used
;  to refresh the printed line for a continous clock
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
rtc_delay:
    di

    ld (DAT_RTC_COUNTER), A
    xor A
    ld (DAT_RTC_CALLBACK), A
    ld (DAT_RTC_CALLBACK+1), A

    ; reset internal second counter
    ld A, [1 << BIT_RTC_24_12] | [1 << BIT_RTC_REST]
    out (IO_RTC_CF), A
    nop
    ld A, [1 << BIT_RTC_24_12]
    out (IO_RTC_CF), A

    ld A, $de
    out ($ff), A

    ld A, $02  ; interrupt mode, 1/64 interval, mask bit = 0
    out (IO_RTC_CE), A
    
    ei
_rtc_delay_loop:
    ld A, (DAT_RTC_COUNTER)
    or A
    jp nz, _rtc_delay_loop
    
    call rtc_disableInterrupt

    ret



; Sets up the RTC to wait for a time given by A as a number
;  of 1/64 second intervals, stores the address in HL, then returns.
;  Once the set time has passed, a call to the stored address is made
;  and the timer is deactivated. Will enable interrupts.
;  If this is called when another timeout is already pending, the old one will be
;  ignored and that callback never made.
rtc_setTimeout:
    di

    ld (DAT_RTC_COUNTER), A
    ld (DAT_RTC_CALLBACK), HL
    
    ; reset internal second counter
    ld A, [1 << BIT_RTC_24_12] | [1 << BIT_RTC_REST]
    out (IO_RTC_CF), A
    nop
    ld A, [1 << BIT_RTC_24_12]
    out (IO_RTC_CF), A

    ld A, $af
    out ($ff), A

    ld A, $02  ; interrupt mode, 1/64 interval, mask bit = 0
    out (IO_RTC_CE), A

    ei

    ret



; Disables the RTC interrupt and resets the interval counter
;  and callback address
rtc_deleteTimeout:
    call rtc_disableInterrupt

    ; reset counter and callback
    xor A
    ld (DAT_RTC_COUNTER), A
    ld (DAT_RTC_CALLBACK), A
    ld (DAT_RTC_CALLBACK+1), A

    ret



; Disables RTC interrupts but will not delete a pending timeout.
;  Won't affect the CPU interrupt settings
rtc_disableInterrupt:
    ld A, $03  ; mask bit = 1, int mode
    out (IO_RTC_CE), A
    xor A      ; make sure irq line is deactivated
    out (IO_RTC_CD), A 
    ret
    


; Enables 1/64 second interrupt. Does not affect the CPU interrupt
;  flags
rtc_enableInterrupt:
    ld A, $02  ; interrupt mode, 1/64 interval, mask bit = 0
    out (IO_RTC_CE), A
    ret



; Interrupt handler for the timed interval interrupt by the RTC
rtc_isr:
    ; NOTE: do not exchange here. since we needed to check a flag in restart vector,
    ;  registers will already have been exchanged once we get here
    ;exx
    ;ex af,af'

    ; clear the irq flag
    xor A
    out (IO_RTC_CD), A
    
    ld A, (DAT_RTC_COUNTER)
    or A
    jp z, _rtc_isr_counterHitZero
    dec A
    ld (DAT_RTC_COUNTER), A
    jp nz, _rtc_isr_end

_rtc_isr_counterHitZero:
    ; counter hit zero. if there's a callback, call it
    call rtc_disableInterrupt
    ld HL, (DAT_RTC_CALLBACK)
    ld A, H
    or L
    jp z, _rtc_isr_end
    
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




