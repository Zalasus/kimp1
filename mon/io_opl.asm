

;-------------------------- SOUND CHIP ACCESS -------------------------

; Initializes OPL sound chip. Does NOT initialize IVR since the IVR is primarily
;  used by the RTC. Use RTC initialization routine to initialize IVR.
opl_init:
    ret



; Interrupt service routine for the OPL timer interrupt
opl_isr:
    ; TODO: Clear OPL interrupt flag here
    ei
    ret