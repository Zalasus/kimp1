

;=============================================
;
;          Floppy IO for Minimon
;
; for the WD37C65 floppy subsystem controller
;
;=============================================


; All following routines can trash A, B and HL. C and DE are preserved.

; fixed config for 512 bytes/sector MFM
FDC_PARAM_N:     equ 2
FDC_PARAM_GPL_RW:  equ $1B
FDC_PARAM_SC:      equ $09
FDC_PARAM_GPL_FMT: equ $54


; Issues a soft reset to the FDC, thereby stopping any running commands.
;  This will not affect the data rate setting, but will always initialize AT mode
fdc_reset:
    ; set active low /SRST bit in operations register to 0
    di
    xor A
    out (IO_FDC_OPER), A

    ; enable DMA and INT pins, select special mode and lift reset condition
    ld A, [1 << BIT_FDC_DMA_ENABLE] | [1 << BIT_FDC_SOFT_RESET]
    out (IO_FDC_OPER), A

    ei
    hlt

    ; this will cause an interrupt that will be caught by ISR and cleared with
    ;  a SENSEI command. once we're through here, controller is reset and
    ;  we are in AT mode

    jp fdc_specify



; Enables motor of selected drive and waits the spinup time.
;  If the right motor was already enabled, this method just returns.
;  This uses a status byte in the data area:
;     Bit 0 -> Drive Number, Bit 1 -> Motor On
;  Creates a timeout that turns the motor off again after 3 seconds.
fdc_enableMotor:
    ; just set drive select bit appropriately and enable both motors.
    ;  only the selected drive should spin up
    ld A, (DAT_DISK_NUMBER)
    ld B, A
    ld A, (DAT_DISK_MOTOR_DRIVE)
    bit 1, A
    jp z, _fdc_enableMotor_turnOn ; no motor was enabled yet. do it now
    xor B
    bit 0, A
    jp nz, _fdc_enableMotor_turnOn ; wrong drive was selected. need to spin up other motor
    ; right motor was already on. we're done
    jp _fdc_enableMotor_end
    
_fdc_enableMotor_turnOn:
    ld A, B
    and $01
    or [1 << BIT_FDC_MOTOR_ENABLE_1] | [1 << BIT_FDC_MOTOR_ENABLE_2] | [1 << BIT_FDC_SOFT_RESET] | [1 << BIT_FDC_DMA_ENABLE]
    out (IO_FDC_OPER), A

    ; wait spinup time
    ld A, 32
    call rtc_delay  ; 32 ticks -> 500ms

    ; store current motor state
    ld A, $02
    or B
    ld (DAT_DISK_MOTOR_DRIVE), A

_fdc_enableMotor_end:
    ; create timeout to disable motor again
    ;  (or refresh an existing one)
    ld HL, fdc_disableMotor
    ld A, 64*3  ; turn off motor after 3 seconds
    call rtc_setTimeout

    ret



; Disables motors of both drives
fdc_disableMotor:
    ld A, (DAT_DISK_NUMBER)
    and $01
    or [1 << BIT_FDC_SOFT_RESET] | [1 << BIT_FDC_DMA_ENABLE]
    out (IO_FDC_OPER), A

    xor A
    ld (DAT_DISK_MOTOR_DRIVE), A

    ret



; Sets drive parameters for the currently selected unit
;  Uses fixed parameters for step rate and load time that should provide
;  stable operation (Load = 80ms, Step = 10ms, Unload = 16ms). DMA mode is disabled.
fdc_specify:
    call fdc_preCommandCheck
    ret c

    ; specify command
    ld A, $03
    out (IO_FDC_DATA), A

    call fdc_waitForRFM
    ret c
    ld A, $51   ; unload time and step rate
    out (IO_FDC_DATA), A

    call fdc_waitForRFM
    ret c
    ld A, [$28 << 1] | 1 ; load time and Non-DMA-Bit (set)
    out (IO_FDC_DATA), A

    ; no exec or result phase

    xor A
    jp resetCarryReturn



; Loads status register 0 from controller. ST0 will be stored in B, the current cylinder
;  index of the drive will be stored in A. This routine will only execute when the controller
;  is accepting commands. It will not perform resets and won't wait for RQM/DIO to assume the right
;  state. If the SENSEI command can't be issued, this routine will return with carry set. If the command
;  executes successfully, carry will be reset.
fdc_senseInterruptStatus:
    ; we can't make a pre-command check here since a reset would cause
    ;  an interrupt, which would then cause the ISR to issue another SENSEI
    ;  command and so on, never finishing since the SENSEI routine hangs during the preCheck
    ;  reset and never issues the actual command that resets the IRQ line.
    ; instead, make a check that won't trigger a reset
    in A, (IO_FDC_STAT)
    and [1 << BIT_FDC_BUSY] | [1 << BIT_FDC_REQUEST_FOR_MASTER] | [1 << BIT_FDC_DATA_INPUT]
    cp [1 << BIT_FDC_REQUEST_FOR_MASTER]
    jp nz, setCarryReturn

    ; sense interrupt status command
    ld A, $08
    out (IO_FDC_DATA), A
    ; command issued. this command has no execution phase and won't produce
    ;  an interrupt. data can be read back right away (hopefully)

    ; read ST0
    call fdc_waitForRFM
    jp nc, setCarryReturn   ; FDC should have two bytes to read now
    in A, (IO_FDC_DATA)
    ld B, A

    ; read current cylinder index
    call fdc_waitForRFM
    jp nc, setCarryReturn
    in A, (IO_FDC_DATA)

    jp resetCarryReturn



; Recalibrates the selected drive. Drive is moved to track 0 
;  and controller internal track counters are reset.
;  If no errors occur, the carry bit is reset or set otherwise.
;  A will contain an FDC error code (see R/W commands)
fdc_recalibrate:
    call fdc_preCommandCheck  ; make sure FDC accepts commands
    ret c

    call fdc_enableMotor

    ; recalibrate command
    ld A, $07 
    out (IO_FDC_DATA), A

    ; unit address
    call fdc_waitForRFM
    ret c  ; direction must still be input~ recalibrate requires 2 bytes
    ld A, (DAT_DISK_NUMBER)
    and $03 ; we only need the two lower bits for the drive number. HS is 0
    out (IO_FDC_DATA), A
    
    ; the FDC is stepping the drive now. we need to wait until stepping is finished
    ;  and head has has reached track 0.
    call fdc_waitForSeekEnd

    ; stepping is finished. no result phase.

    ; load interrupt status byte as received by ISR
    ld A, (DAT_DISK_INT_SR0)
    and [1 << BIT_FDC_SEEK_END] | [1 << BIT_FDC_EQUIPMENT_CHECK]
    cp [1 << BIT_FDC_SEEK_END]  ; only seek end must be set
    jp nz, setCarryReturn

    xor A
    ld (DAT_DISK_TRACK), A
    ld (DAT_DISK_TRACK_PHYS), A
    ld (DAT_DISK_HEAD), A
    jp resetCarryReturn



; Positions the head of selected drive over selected track. This routine performs
;  the mapping of even tracks to side 0 and odd tracks to side 1.
fdc_seek:
    ; store previous physical track
    ld A, (DAT_DISK_TRACK_PHYS)
    ld B, A

    ; bit 0 of logical track is side. shift around a bit to isolate the two
    ld A, (DAT_DISK_TRACK)
    or A  ; clear carry flag
    rra
    ld (DAT_DISK_TRACK_PHYS), A
    ld A, $00   ; can't use xor A. need to preserve carry flag
    rla
    ld (DAT_DISK_HEAD), A

    ; we probably have to check if the disk changed here. since the CBIOS always
    ;  recalibrates the drive after changing drive number this is not strictly 
    ;  neccesary but could cause some errors if we change something later

    ; did the physical track change? if not there's no need to issue a command
    ld A, (DAT_DISK_TRACK_PHYS)
    cp B
    jp z, _fdc_seek_end

    call fdc_preCommandCheck
    ret c

    call fdc_enableMotor

    ; seek command
    ld A, $0f
    out (IO_FDC_DATA), A

    ; unit & head address
    call fdc_waitForRFM
    ret c
    ld A, (DAT_DISK_NUMBER)
    and $03  ; mask out lower 2 bits. Head select is always 0 (can't step heads individually)
    out (IO_FDC_DATA), A

    ; cylinder address
    call fdc_waitForRFM
    ret c
    ld A, (DAT_DISK_TRACK_PHYS)
    out (IO_FDC_DATA), A

    ; controller is stepping now. no bytes are transferred, so just wait for
    ;  IRQ at end of exec phase
    call fdc_waitForSeekEnd

    ; no result phase.

    ; load interrupt status byte as received by ISR
    ld A, (DAT_DISK_INT_SR0)
    and [1 << BIT_FDC_SEEK_END] | [1 << BIT_FDC_EQUIPMENT_CHECK]
    cp [1 << BIT_FDC_SEEK_END]  ; only seek end must be set
    jp nz, setCarryReturn

_fdc_seek_end:
    xor A
    jp resetCarryReturn



; Reads data from the selected drive. Uses the parameters (track, sector, target
;  data buffer etc.) as specified in the respective data area fields.
;  If reading succeeds, the carry flag is reset and A will contain $00. If any
;  error occurs, carry wil be set and A contains $01 if the FDC was in an invalid
;  state or $03 if the FDC reported errors after executing the command.
fdc_readData:
    call fdc_preCommandCheck
    ret c

    call fdc_enableMotor

    ; read command (including MT,MF & SK bits)
    ld A, $66  ; no multitrack, MFM mode, skip deleted sectors
    out (IO_FDC_DATA), A

    ; rest of command is handled by common routine
    call fdc_rwCommand
    ret c

    ; all command bytes transferred. controller will start reading now
    call rtc_disableInterrupt ; make sure transfer is not interrupted
    call fdc_rTransfer
    call rtc_enableInterrupt  ; turn back on so motors get disabled on time

    ; execution mode ended. let subroutine read result bytes and return
    call fdc_rwCommandStatusCheck
    ei ; rTransfer will leave interrupts deactivated. only reenable after result bytes have been read
    ret



; Writes data to the selected drive. Uses the parameters (track, sector, target
;  data buffer etc.) as specified in the respective data area fields.
;  If writing succeeds, the carry flag is reset and A will contain $00. If any
;  error occurs, carry wil be set and A contains $01 if the FDC was in an invalid
;  state, $02 if the selected disk was write protected or $03 if the FDC reported
;  errors after executing the command.
fdc_writeData:
    call fdc_preCommandCheck
    ret c

    call fdc_enableMotor

    ; write command (including MT & MF bits)
    ld A, $45  ; no multitrack, MFM mode
    out (IO_FDC_DATA), A

    ; rest of command is handled by common routine
    call fdc_rwCommand
    ret c

    ; controller starts writing now
    call rtc_disableInterrupt ; make sure transfer is not interrupted
    call fdc_wTransfer
    call rtc_enableInterrupt  ; turn back on so motors get disabled on time

    ; command execution finished. let helper routine read result bytes and set error codes
    call fdc_rwCommandStatusCheck
    ei  ; wTransfer will leave interrupts deactivated. only reenable after result bytes have been read
    ret



; This routine can be used by both to issue the address part of the command since
;  the command layout for read and write commands is pretty much the same. The first
;  command byte has to be issued to the FDC by the caller to differentiate between R/W.
;  This routine returns right before the caller should call the appropriate transfer
;  method. Error states are handled appropriately. If this routine returns with carry set,
;  the calling routine may return immediately and should not modify A to preserve the error code.
fdc_rwCommand:
    ; unit & head address
    call fdc_waitForRFM
    ret c
    ld A, (DAT_DISK_HEAD)
    rlca ; shift left head address by 2
    rlca
    ld B, A
    ld A, (DAT_DISK_NUMBER)
    and $03  ; mask out lower 2 bits
    or B     ; or-in head address
    out (IO_FDC_DATA), A

    ; cylinder address
    call fdc_waitForRFM
    ret c
    ld A, (DAT_DISK_TRACK_PHYS)
    out (IO_FDC_DATA), A

    ; head address (repeated value, same as in unit address)
    call fdc_waitForRFM
    ret c
    ld A, (DAT_DISK_HEAD)
    out (IO_FDC_DATA), A
    
    ; sector address
    call fdc_waitForRFM
    ret c
    ld A, (DAT_DISK_SECTOR)
    out (IO_FDC_DATA), A

    ; bytes per sector
    call fdc_waitForRFM
    ret c
    ld A, FDC_PARAM_N
    out (IO_FDC_DATA), A

    ; end of track
    ;  If the controller reads past this, it will terminate the command with
    ;  an end-of-cylinder error. since we can't keep track of sector count during
    ;  read this is exactly what we want
    call fdc_waitForRFM
    ret c
    ld A, (DAT_DISK_SECTOR)
    out (IO_FDC_DATA), A

    ; gap length
    call fdc_waitForRFM
    ret c
    ld A, FDC_PARAM_GPL_RW
    out (IO_FDC_DATA), A

    ; data length
    call fdc_waitForRFM
    ret c
    xor A   ; meaningless for N > 128
    out (IO_FDC_DATA), A

    xor A
    jp resetCarryReturn



; Loads the result data buffer with result bytes from the FDC. Then checks
;  the status bytes returned in result phase of a read/write or format command
;  and sets the carry flag and error codes in A appropriately.
fdc_rwCommandStatusCheck:
    call fdc_readResultBuffer
    ret c

    ; ST0
    ld A, (DAT_DISK_RES_BUFFER)
    and [1 << BIT_FDC_INTERRUPT_CODE0] | [1 << BIT_FDC_INTERRUPT_CODE1]
    jp z, resetCarryReturn  ; if both IC bits are zero, command was successful. no need to check further

    ; if IC bits were not zero, something went wrong. check for exact cause

    ; ST1
    ld A, (DAT_DISK_RES_BUFFER+1)
    and $7f   ; ignore end-of-cylinder bit
    jp nz, setCarryReturn
    ; Note: End of cylinder is not an error. Since the CPU is too slow to check the transferred byte count
    ;  and issue a TC once a sector has been transferred, we set the end-of-track marker to the sector we want
    ;  to read, which will cause the FDC to terminate the transfer after one sector, albeit with an End-of-Cylinder-error

    ; ST2
    ld A, (DAT_DISK_RES_BUFFER+2)
    and $33  ; neither of these bits must be set (sorry, bitmask was to long)
    jp nz, setCarryReturn

    jp resetCarryReturn



; Reads 7 result bytes from the FDC to the result data buffer.
;  Carry flag used as error code (always indicates invalid state; A is not
;  set accordingly)
fdc_readResultBuffer:
    ld HL, DAT_DISK_RES_BUFFER
    ld B, 7  ; read all 7 result bytes

_fdc_readResultBuffer_loop:
    call fdc_waitForRFM
    jp nc, setCarryReturn
    in A, (IO_FDC_DATA)
    ld (HL), A
    inc HL
    djnz _fdc_readResultBuffer_loop

    jp resetCarryReturn
    


; Checks MSR repeatedly and returns if no drives are stepping anymore.
;  This implies that an IRQ has been made and the ISR issued a SENSEI
;  command, as only then the busy bits get reset.
fdc_waitForSeekEnd:
    in A, (IO_FDC_STAT)
    and $1f ; all busy bits must be 0
    jp nz, fdc_waitForSeekEnd  ; no halting here!! polling is safer since interrupts are handled async for seeks
    ret



; Half polling/half IRQ routine for read data transfer. Uses interrupts in mode 0 to wait for the FDC.
;  Transfers data read from the FDC to the buffer pointed by (HL), growing upwards. 
;  There is no bounds checking. Transfer ends when controller ends it.
;  When finished, HL points to the location AFTER where the last byte was stored.
;  Since this routine has some overhead before actually able to react to IRQs, the controller may 
;  be overrun when it locks the PLL and finds the start sector faster than this routine can enter
;  it's transfer loop. This, however, seems unlikely to happen in practice.
fdc_rTransfer:
    ; Initialize IVR to a NOP so we can use the HLT instruction in IM0 to wait for the IRQ
    ;  without the cycle penalty of calling an ISR. When HLT is lifted, the CPU will execute a NOP
    ;  for 6 cycles and then continue after the HLT. This is faster and more elegant than polling the MSR
    di  ; don't interrupt during intialization
    ld A, IVR_FDC_NOP
    out (IO_IVR_FDC), A

    exx  ; use alternative registers so we don't trash so much

    ld HL, (DAT_DISK_DATAPTR)
    ld D, [1 << BIT_FDC_EXEC_MODE] ; constant for bit masking. used instead of immediate value for speed
    ld C, IO_FDC_DATA  ; IO port constant for ini instruction (again used for speed)

    call _fdc_rTransfer_loop ; call loop instead of jumping to it. a conditional return is faster than a branch

    exx

    ; restore original IVR contents
    ld A, IVR_FDC_RESTART
    out (IO_IVR_FDC), A
    ret

; time critical part begins here
_fdc_rTransfer_loop:
    ei
    hlt
    ; HALT was lifted~ check for interrupt cause
    in A, (IO_FDC_STAT)
    and D  ; [1 << BIT_FDC_EXEC_MODE], use D as constant for speed
    ret z  ; if not in exec mode anymore, we're done
    ini
    jp _fdc_rTransfer_loop



; Initiates an IRQ-driven data transfer to the FDC from the location pointed to by (HL).
;  Same details apply as to read routine above.
fdc_wTransfer:
    di
    ; initialize IVR to alternative NOP since we want to save cycles
    ld A, IVR_FDC_NOP
    out (IO_IVR_FDC), A

    exx

    ld HL, (DAT_DISK_DATAPTR)
    ld D, [1 << BIT_FDC_EXEC_MODE] ; constant for bit masking. used instead of immediate value for speed
    ld C, IO_FDC_DATA  ; IO port constant for outi instruction (again used for speed)

    call _fdc_wTransfer_loop

    exx

    ld A, IVR_FDC_RESTART
    out (IO_IVR_FDC), A
    ret

; time critical part begins here
_fdc_wTransfer_loop:
    ei
    hlt
    ; HALT was lifted~ check for interrupt cause
    in A, (IO_FDC_STAT)
    and D  ; [1 << BIT_FDC_EXEC_MODE], use D as constant for speed
    ret z  ; if not in exec mode anymore, we're done
    outi
    jp _fdc_wTransfer_loop



; Checks initial condition for issuing commands to the FDC. If controller is busy,
;  it will be reset and busy state checked again. Then this routine checks for RQM 
;  signal and if DIO signal is set to "input". If any of this fails, the carry flag
;  will be set upon return or reset otherwise.
fdc_preCommandCheck:
    in A, (IO_FDC_STAT)  ; is controller busy? if yes -> reset
    bit BIT_FDC_BUSY, A
    jp z, _fdc_preCommandCheck_checkRQM

    call fdc_reset
    in A, (IO_FDC_STAT)   ; did resetting fix busy state?
    bit BIT_FDC_BUSY, A
    jp nz, setCarryReturn ; nope -> error. else check RQM and DIO state

_fdc_preCommandCheck_checkRQM:
    and [1 << BIT_FDC_REQUEST_FOR_MASTER] | [1 << BIT_FDC_DATA_INPUT]
    cp [1 << BIT_FDC_REQUEST_FOR_MASTER]
    jp nz, setCarryReturn
    jp resetCarryReturn



; Waits until the FDC reports a Request For Master. After that, it checks the data direction
;  bit. If it is is set to "waiting for input" the carry flag is reset, if bit is set to "data available"
;  carry bit is set.
fdc_waitForRFM:
    in A, (IO_FDC_STAT)
    bit BIT_FDC_REQUEST_FOR_MASTER, A
    jp z, fdc_waitForRFM

    bit BIT_FDC_DATA_INPUT, A
    jp nz, setCarryReturn ; DIO = 1 -> data register expects to be read
    jp resetCarryReturn



; Interrupt handler for floppy controller. This is normally used when controller is idling or
;  for seek/recalibrate commands. During read/write operations and other commands with
;  result phase the IVR is temporarily overwritten with a NOP and this routine is not used.
fdc_isr:
    ex AF, AF'
    exx

    ; check for interrupt cause using SENSEI command
    ;  the SENSEI routine won't cause resets and will return with error if initial
    ;  condition not met (if that happens we are screwed)
    call fdc_senseInterruptStatus
    jp c, biosPanic  ; sensei could not execute -> can't reset IRQ line. PANIC!!!
    ; store in result buffer
    ld (DAT_DISK_INT_CYLINDER), A
    ld A, B
    ld (DAT_DISK_INT_SR0), A
    
    exx
    ex AF, AF'
    ei
    ret



