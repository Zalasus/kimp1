
z80

    include ../mon/minimon_routines.inc

    org $5000

    ld HL, str_helloWorld
    call MINIMON_PRINTSTRING

    jp MINIMON_J_BACK_TO_PROMPT

str_helloWorld:
    db 'Hello World!', $0A, $00

    end
    