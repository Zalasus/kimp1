

    include ../kimp1def.inc

    org $100

    ; get ROM enabler out of the part of the TPA that overlaps
    ;  the rom ($0 - $1fff)
    ld DE, $2000
    ld HL, romEnabler
    ld BC, $10   ; should be enough
    ldir

    jp $2000

romEnabler:
    di
    ld A, [1 << BIT_TCCR_C2_GATE]
    out (IO_TCCR), A
    jp $0000