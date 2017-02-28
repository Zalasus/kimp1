

; Loads the first sector from drive A into memory and jumps to the loaded code
command_boot:
    ; first, check if extension board is plugged in
    ld A, (DAT_EXT_INITIALIZED)
    cp $ff
    jp z, _command_boot_cont

    ; board is not present (or was not on startup). print error
    ld HL, str_noExtPresent
    call printString
    jp monitorPrompt_loop

_command_boot_cont:
    ; seek drive A/0 to track 0
    xor A
    ld (DAT_DISK_NUMBER), A
    call fdc_recalibrate
    jp c, _command_disk_error  ; re-use disktool error routine

    ld HL, CONF_BOOT_LOCATION
    ld (DAT_DISK_DATAPTR), HL
    ld A, 1
    ld (DAT_DISK_SECTOR), A
    ld HL, fdc_readData
    call fdc_commandWithRetry
    jp c, _command_disk_error

    ; read without errors. check if code has boot signature
    ld HL, (CONF_BOOT_LOCATION + CONF_DISK_BYTES_PER_SECTOR - 2)
    ld A, H
    cp high CONF_BOOT_SIGNATURE
    jp nz, _command_boot_notBootable
    ld A, L
    cp low CONF_BOOT_SIGNATURE
    jp nz, _command_boot_notBootable

    ; disable motors before handing over control in case
    ;  the bootloader messes with the interrupts or RTC. we don't want the motors
    ;  to stay enabled forever until the next disk access
    call rtc_deleteTimeout
    call fdc_disableMotor

    ; give control to whatever we just loaded (hopefully a bootloader)
    jp CONF_BOOT_LOCATION

_command_boot_notBootable:
    ld HL, str_notBootable
    call printString
    jp monitorPrompt_loop


