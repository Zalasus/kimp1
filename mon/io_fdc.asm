

;----------------------------- DISK IO --------------------------------

; drive parameter table
fdc_dat_bps:
    db $03



; Initializes the FDC. This routine will soft reset the FDC, set
;  data rate and place the FDC in AT mode. 
fdc_init:
    ; initialize IVR to the dedicated restart vector
    ld A, IVR_FDC_DEF
    out (IO_IVR_FDC), A
    
    xor A
    ld (DAT_DISK_SECTOR), A
    ld (DAT_DISK_TRACK), A
    ld (DAT_DISK_NUMBER), A
    ld HL, DAT_DISK_DATABUFFER
    ld (DAT_DISK_DATAPTR), HL

    call fdc_reset

    ; the soft reset will also have initialized AT mode

    ; initialize data rate
    ; 125Kb/s @ FM for 16MHz default clock
    ld A, [1 << BIT_FDC_DATARATE1] 
    out (IO_FDC_CONT), A

    jp fdc_specify



; Issues a soft reset to the FDC, thereby stopping any running commands.
;  This will not affect the data rate setting or the mode, but when called
;  on an FDC in hard reset mode, AT mode will be initialized.
fdc_reset:
    ; set all bits in operations register to 0,
    ;  including the active low /SRST bit.
    ; once reset is finished, controller will interrupt
    di
    xor A
    out (IO_FDC_OPER), A
    ei

    hlt

    ; controller is now reset
    ; ISR will have issued SENSEI command, which will reset the IRQ line
    ;  make sure soft reset bit is high and mode bit 0 for AT mode
    ld A, [1 << BIT_FDC_SOFT_RESET]
    out (IO_FDC_OPER), A

    ret



; Enables motor of selected drive and waits the spinup time.
;  If the right motor was already enabled, this method just returns.
fdc_enableMotor:
    ; just set drive select bit appropriately and enable both motors.
    ;  only the selected drive should spin up
    ld A, (DAT_DISK_NUMBER)
    ld B, A
    ld A, (DAT_DISK_MOTOR_DRIVE)
    bit 1, A
    jp z, _fdc_enableMotor_doStuff ; no motor was enabled yet. do it now
    xor B
    bit 0, A
    jp z, _fdc_enableMotor_doStuff ; wrong drive was selected. need to spin up other motor
    ; right motor was already on. we're done
    ret
    
_fdc_enableMotor_doStuff:
    ld A, B
    and $01
    or [1 << BIT_FDC_MOTOR_ENABLE_1] | [1 << BIT_FDC_MOTOR_ENABLE_2] | [1 << BIT_FDC_SOFT_RESET]
    out (IO_FDC_OPER), A

    ; wait spinup time
    ld A, 32
    call rtc_delay  ; 32 ticks -> 500ms

    ; store current motor state
    ld A, $02
    or B
    ld (DAT_DISK_MOTOR_DRIVE), A

    ret



; Disables motors of both drives
fdc_disableMotor:
    ld A, (DAT_DISK_NUMBER)
    and $01
    or [1 << BIT_FDC_SOFT_RESET]
    out (IO_FDC_OPER), A

    xor A
    ld (DAT_DISK_MOTOR_DRIVE), A

    ret



; Sets drive parameters for the currently selected unit
;  Uses fixed parameters for step rate and load time that should provide
;  stable operation (Load = 80ms, Step = 10ms, Unload = 16ms). DMA mode is disabled.
fdc_specify:
    call fdc_preCommandCheck

    ; specify command
    ld A, $03
    out (IO_FDC_DATA), A

    call fdc_waitForRFM
    jp c, setCarryReturn
    ld A, $51   ; unload time and step rate
    out (IO_FDC_DATA), A    

    call fdc_waitForRFM
    jp c, setCarryReturn
    ld A, [$28 << 1] | 1 ; load time and Non-DMA-Bit (set)
    out (IO_FDC_DATA), A

    ; no exec or result phase

    jp resetCarryReturn



; Loads status register 0 from controller. ST0 will be stored in B, the current cylinder
;  index of the drive will be stored in A. This routine will only execute when the controller
;  is accepting commands. It will not perform resets and won't wait for RQM/DIO to assume the right
;  state. If the SENSEI command can't be issued, this routine will return with carry set. Otherwise,
;  carry will be reset.
fdc_senseInterruptStatus:
    ; we can't make a pre-command check here since a reset would cause
    ;  an interrupt, which would then cause the ISR to issue another SENSEI
    ;  command and so on, never finishing since the SENSEI routine hangs during the preCheck
    ;  reset and never issues the actual command that resets the IRQ line.
    ; instead, make a check that won't trigger a reset
    in A, (IO_FDC_STAT)
    bit BIT_FDC_BUSY, A
    jp z, setCarryReturn
    bit BIT_FDC_REQUEST_FOR_MASTER, A
    jp z, setCarryReturn
    bit BIT_FDC_DATA_INPUT
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
 


; Recalibrates the selected drive. Drive is moved 
;  to track 0 and controller internal track counters are reset.
;  If no errors occur, the carry bit is reset or set otherwise.
;  A will contain an FDC error code (see R/W commands)
fdc_recalibrate:
    call fdc_preCommandCheck  ; make sure FDC accepts commands

    ; recalibrate command
    ld A, $07 
    out (IO_FDC_DATA), A

    ; unit address
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState  ; direction must still be input~ reclibrate requires 2 bytes
    ld A, (DAT_DISK_NUMBER)
    and $03 ; we only need the two lower bits for the drive number. HS is 0
    out (IO_FDC_DATA), A
    
    ; the FDC is stepping the drive now. we need to wait until stepping is finished
    ;  and head has has reached track 0.
    call fdc_waitForExecEndIrq

    ; stepping is finished. no result phase.

    ; load interrupt status byte as received by ISR
    ld A, (DAT_DISK_INT_SR0)
    bit BIT_FDC_SEEK_END, B
    jp z, fdc_commandError_commandUnsuccessful
    bit BIT_FDC_EQUIPMENT_CHECK, B
    jp nz, fdc_commandError_commandUnsuccessful

    xor A
    ld (DAT_DISK_TRACK), A
    jp resetCarryReturn



; Positions the head of selected drive over selected track
fdc_seek:
    call fdc_preCommandCheck

    ; seek command
    ld A, $0f
    out (IO_FDC_DATA), A

    ; unit & head address
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, (DAT_DISK_NUMBER)
    and $03  ; mask out lower 2 bits. Head select is always 0
    out (IO_FDC_DATA), A

    ; cylinder address
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, (DAT_DISK_TRACK)
    out (IO_FDC_DATA), A

    ; controller is stepping now. no bytes are transferred, so just wait for
    ;  IRQ at end of exec phase
    call fdc_waitForExecEndIrq

    ; no result phase.

    ; load interrupt status byte as received by ISR
    ld A, (DAT_DISK_INT_SR0)
    bit BIT_FDC_SEEK_END, B
    jp z, fdc_commandError_commandUnsuccessful
    bit BIT_FDC_EQUIPMENT_CHECK, B
    jp nz, fdc_commandError_commandUnsuccessful

    xor A
    jp resetCarryReturn



fdfdfdf
; Big problem: all commands with result bytes can't use the isr since it does not read from the data reg,
;  therefore not clearing the irq line. but once isr returns, interrupt will be reeanbled, triggering the isr
;  once again. need to find a way to handle error codes etc. while still correctly switching between interrupt modes


; Reads data from the selected drive. Uses the parameters (track, sector, target
;  data buffer etc.) as specified in the respective data area fields.
;  If reading succeeds, the carry flag is reset and A will contain $00. If any
;  error occurs, carry wil be set and A contains $01 if the FDC was in an invalid
;  state or $03 if the FDC reported errors after executing the command.
fdc_readData:
    ; first, enable and spin up motors
    call fdc_enableMotor
    ld HL, fdc_disableMotor
    ld A, 128  ; turn off motor after 2 seconds
    call rtc_setTimeout

    call fdc_preCommandCheck

    ; read command (including MT,MF & SK bits)
    ld A, $26  ; no multitrack, FM mode, skip deleted sectors
    out (IO_FDC_DATA), A

    ; rest of command is handled by common routine
    call fdc_rwCommand
    ret c

    ; all command bytes transferred. controller will start reading now
    call rtc_disableInterrupt ; make sure transfer is not interrupted
    ld HL, (DAT_DISK_DATAPTR) ; HL is used as target address during transfer
    call fdc_rTransfer
    call rtc_enableInterrupt  ; turn back on so motors get disabled on time

    ; execution mode ended. let subroutine read result bytes and return
    jp fdc_rwCommandStatusCheck



; Writes data to the selected drive. Uses the parameters (track, sector, target
;  data buffer etc.) as specified in the respective data area fields.
;  If writing succeeds, the carry flag is reset and A will contain $00. If any
;  error occurs, carry wil be set and A contains $01 if the FDC was in an invalid
;  state, $02 if the selected disk was write protected or $03 if the FDC reported
;  errors after executing the command.
fdc_writeData:
    ; first, enable and spin up motors
    call fdc_enableMotor
    ld HL, fdc_disableMotor
    ld A, 128  ; turn off motor after 2 seconds
    call rtc_setTimeout

    call fdc_preCommandCheck

    ; write command (including MT & MF bits)
    ld A, $05  ; no multitrack, FM mode
    out (IO_FDC_DATA), A

    ; rest of command is handled by common routine
    call fdc_rwCommand
    jp c, fdc_selectDefaultInterrupt

    ; controller starts writing now
    call rtc_disableInterrupt ; make sure transfer is not interrupted
    ld HL, (DAT_DISK_DATAPTR) ; HL is used as target address during transfer
    call fdc_wTransfer
    call rtc_enableInterrupt  ; turn back on so motors get disabled on time

    ; command execution finished. let helper routine read result bytes and set error codes
    jp fdc_rwCommandStatusCheck



; Formats the currently selected track on the selected drive.
;  Error codes same as for write command.
fdc_format:
    ; first, enable and spin up motors
    call fdc_enableMotor
    ld HL, fdc_disableMotor
    ld A, 128  ; turn off motor after 2 seconds
    call rtc_setTimeout

    call fdc_preCommandCheck

    ; format track command (including MF bits)
    ld A, $0D  ; FM mode
    out (IO_FDC_DATA), A

    ; unit & head address
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, (DAT_DISK_NUMBER)
    and $03  ; mask out lower 2 bits. Head select is always 0
    out (IO_FDC_DATA), A

    ; bytes per sector (always 128)
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    xor A  ; N=0 in FM mode -> 128 bytes/sector
    out (IO_FDC_DATA), A

    ; sectors per track
    ;  This value depends on the drive type. We'll assume we have a 3.5" floppy for now
    ;  and use a fixed value
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, $0f  ; $0f for 3.5" floppies
    out (IO_FDC_DATA), A

    ; gap length
    ;  again, this depends on drive type when formating. use value for 3.5" floppy again
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, $18  ; $18 ($1B ????) for 3.5" floppies  TODO: datasheet is really blurry. can't tell if 8 or B. fix pls.
    out (IO_FDC_DATA), A

    ; data filler
    ;  this byte wil be used to fill the data field
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, 42  ; what else should I use?
    out (IO_FDC_DATA), A
    
    ; command is running now. this will transfer no data, so we simply wait for the interrupt
    ;  at the end of execution phase. Since this process is not time-critical and no two
    ;  interrupts from different sources can be lost, we don't need to pause the motor-off timeout
    call fdc_waitForExecEndIrq

    ; let subroutine check result bytes
    jp fdc_rwCommandStatusCheck



; This routine can be used by both to issue the address part of the command since
;  the command layout for read and write commands is pretty much the same. The first
;  command byte has to be issued to the FDC by the caller to differentiate between R/W.
;  This routine returns right before the caller should call the appropriate transfer
;  method. Error states are handled appropriately. If this routine returns with carry set,
;  the calling routine may return immediately and should not modify A to preserve the error code.
fdc_rwCommand:
    ; unit & head address
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, (DAT_DISK_NUMBER)
    and $03  ; mask out lower 2 bits. Head select is always 0
    out (IO_FDC_DATA), A

    ; cylinder address
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, (DAT_DISK_TRACK)
    out (IO_FDC_DATA), A

    ; head address (repeated value, same as in unit address)
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    xor A ; head is always 0 for now
    out (IO_FDC_DATA), A
    
    ; sector address
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, (DAT_DISK_SECTOR)
    out (IO_FDC_DATA), A

    ; bytes per sector (always 128)
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    xor A  ; N=0 in FM mode -> 128 bytes/sector
    out (IO_FDC_DATA), A

    ; end of track
    ;  Some sources say this is the sector count to be accessed, some say it's the final
    ;  sector address. The datasheet is not clear on that. Assume it's the address, so
    ;  this is the same as sent as sector address
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, (DAT_DISK_SECTOR)
    out (IO_FDC_DATA), A

    ; gap length
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, $07  ; this is the value the datasheet recommends for all drive types while reading/writing
    out (IO_FDC_DATA), A

    ; data length
    call fdc_waitForRFM
    jp c, fdc_commandError_invalidState
    ld A, 128   ; if N=0, this is the absolute value of bytes per sector. we want the full amount of course
    out (IO_FDC_DATA), A

    xor A
    jp resetCarryReturn



; Checks the status bytes returned in result phase of a read/write or format command
;  and sets the carry flag and error codes in A appropriately. Ensures data buffer is
;  emptied if no state errors occur.
fdc_rwCommandStatusCheck:
    ; ST0
    call fdc_waitForRFM
    jp nc, fdc_commandError_invalidState
    in A, (IO_FDC_DATA)
    and [1 << BIT_FDC_INTERRUPT_CODE0] | [1 << BIT_FDC_INTERRUPT_CODE1]
    jp nz, fdc_commandError_commandUnsuccessful  ; both IC bits must be zero

    ; ST1
    call fdc_waitForRFM
    jp nc, fdc_commandError_invalidState
    in A, (IO_FDC_DATA)
    bit BIT_FDC_NOT_WRITEABLE, A
    jp nz, fdc_commandError_writeProtected
    or A  ; if any other bit is set, use generic fail error code
    jp nz, fdc_commandError_commandUnsuccessful

    ; ST2
    call fdc_waitForRFM
    jp nc, fdc_commandError_invalidState
    in A, (IO_FDC_DATA)
    and $33  ; neither of these bits must be set (sorry, bitmask was to long)
    jp nz, fdc_commandError_commandUnsuccessful

    ; address information after command. we can discard this
    call fdc_emptyResultBuffer

    xor A
    jp resetCarryReturn



fdc_commandError_invalidState:
    ; this error is probably urecoverable. no use in emptying the data
    ;  buffer. let a reset handle this if neccessary
    ld A, $01
    jp setCarryReturn     
   
fdc_commandError_writeProtected:
    call fdc_emptyResultBuffer
    ld A, $02
    jp setCarryReturn

fdc_commandError_commandUnsuccessful:
    call fdc_emptyResultBuffer
    ld A, $03
    jp setCarryReturn



fdc_selectDefaultInterrupt:
    ld B, A  ; save error code
    ld A, IVR_FDC_DEF
    out (IO_IVR_FDC), A
    ld A, B
    ei
    ret



fdc_selectNopInterrupt:
    ld A, IVR_FDC_NOP
    out (IO_IVR_FDC), A
    ei
    ret


; Checks MSR of FDC and returns if exec phase has ended. If not,
;  waits for an IRQ and loops, checking again and so forth.
;  Will leave interrupts in the enable state that they would have
;  been if a simple hlt had been executed instead of this routine.
;  If the interrupt calls an ISR that reactivates interrupts, interrupts
;  will be active when this returns.
fdc_waitForExecEndIrq:
    in A, (IO_FDC_STAT)
    bit BIT_FDC_EXEC_MODE, A
    ret z

    ei
    hlt

    jp fdc_waitForExecEndIrq



; Experimental half polling/half IRQ routine for read data transfer. Uses interrupts
;  in mode 0 to wait for the FDC. Transfers data read from the FDC to the buffer pointed
;  by (HL), growing upwards. There is no bounds checking. Transfer ends when controller ends it.
;  Trashes B,C,D. When finished, HL points to the location AFTER where the last byte was stored. 
;  Since this routine has some overhead before actually able to react to IRQs, the controller may 
;  be overrun when it locks the PLL and finds the start sector faster than this routine can enter
;  it's transfer loop. This, however, seems unlikely to happen in practice.
fdc_rTransfer:
    ; Initialize IVR to a NOP so we can use the HLT instruction in IM0 to wait for the IRQ
    ;  without the cycle penalty of calling an ISR. When HLT is lifted, the CPU will execute a NOP
    ;  for 6 cycles and then continue after the HLT. This is faster and more elegant than polling the MSR
    ld A, IVR_FDC_NOP
    out (IO_IVR_FDC), A     

    ld D, [1 << BIT_FDC_EXEC_MODE] ; constant for bit masking. used instead of immediate value for speed
    ld C, IO_FDC_DATA  ; IO port constant for ini instruction (again used for speed)

    call _fdc_rTransfer_loop ; call loop instead of jumping to it. a conditional return is faster than a branch

    ; restore original IVR contents
    ld A, IVR_FDC_DEF
    out (IO_IVR_FDC), A
    ei
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
    ; initialize IVR to alternative NOP since we want to save cycles
    ld A, IVR_FDC_NOP
    out (IO_IVR_FDC), A

    ld D, [1 << BIT_FDC_EXEC_MODE] ; constant for bit masking. used instead of immediate value for speed
    ld C, IO_FDC_DATA  ; IO port constant for outi instruction (again used for speed)

    call _fdc_wTransfer_loop

    ld A, IVR_FDC_DEF
    out (IO_IVR_FDC), A
    ei
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



; Reads and discards bytes from the FDC data register until Request For Master is set and
;  direction bit reports "waiting for input".
fdc_emptyResultBuffer:
    in A, (IO_FDC_STAT)
    bit BIT_FDC_REQUEST_FOR_MASTER, A
    jp z, fdc_emptyResultBuffer

    bit BIT_FDC_DATA_INPUT, A
    ret z ; DIO = 0 -> data register expects input. we are done
    
    in A, (IO_FDC_DATA)
    jp fdc_emptyResultBuffer



; Checks initial condition for issuing commands to the FDC. If controller is busy,
;  it will be reset. Then this routine waits for RQM signal and checks if DIO signal 
;  is set to "input". If not, the controller will be reset and DIO checked again. 
;  This will bring the controller in the right state eventually or just hang forever.
fdc_preCommandCheck:
    in A, (IO_FDC_STAT)
    bit BIT_FDC_BUSY, A
    jp z, _fdc_preCommandCheck_wait
    call fdc_reset

_fdc_preCommandCheck_wait:
    call fdc_waitForRFM
    ret nc  ; if carry reset -> FDC awaits input. all ready

    call fdc_reset
    jp _fdc_preCommandCheck_wait



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
    
    ; check for interrupt cause (this hopefully resets the IRQ line)
    ;  this won't execute if controller does not accept commands, so we are safe in case
    ;  the interrupt came from a command with result phase (format, scan, read ID), although
    ;  such a case should never occur
    call fdc_senseInterruptStatus
    jp c, _fdc_isr_end  ; sensei could not execute -> don't touch result byte
    ld A, B
    ld (DAT_DISK_INT_SR0), A
    
_fdc_isr_end:
    exx
    ex AF, AF'
    ei
    ret



; Waits for approximately 15us
;  (recommended but not required before reading MSR in command phase~
;   we use polling instead. might remove routine if no other useful application is found)
fdc_delay:
    ld A, (CPU_SPEED/67000)/14  ; 1/15us = 67kHz, delay loop is 14 cycles long

_fdc_delay_loop:
    dec A
    jp nz, _fdc_delay_loop

    ret





