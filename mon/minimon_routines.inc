
; addresses to the monitor jump vector. these are usually called,
;  however some routines should be jumped to. these are marked MINIMON_J_...

MINIMON_J_MAIN:           equ $0000
MINIMON_MONITOR_TO_RAM:   equ $0003
MINIMON_CONOUT:           equ $0006
MINIMON_CONIN:            equ $0009
MINIMON_CONST:            equ $000C
MINIMON_PRINTSTRING:      equ $000F
MINIMON_READSTRING:       equ $0012
MINIMON_CLS:              equ $0015
MINIMON_PRINTHEX:         equ $0018
MINIMON_READHEX:          equ $001B
MINIMON_SELECTDISK:       equ $001E
MINIMON_SEEK:             equ $0021
MINIMON_READDATA:         equ $0024
MINIMON_WRITEDATA:        equ $0027
MINIMON_EXTENDED:         equ $002A
MINIMON_J_BACK_TO_PROMPT: equ $002D


MINIMON_EXT_GETVERSION:   equ $00 ; A = version number
MINIMON_EXT_PRINTTIME:    equ $10 ; prints time from RTC to console