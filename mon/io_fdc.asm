

;----------------------------- DISK IO --------------------------------

; drive parameter table
fdc_dat_bps:
    db $03



; Initializes the FDC to AT mode, setting data rate etc.
;  This will initialize the FDC-IVR to a NOP.
fdc_init:
    ; initialize IVR to a NOP since all routines just wait for
    ;  IRQs using halt mode.
    xor A
    out (IO_IVR_FDC), A
    
    xor A
    ld (DAT_DISK_SECTOR), A
    ld (DAT_DISK_TRACK), A
    ld (DAT_DISK_NUMBER), A
    ld HL, DAT_DISK_DATABUFFER
    ld (DAT_DISK_DATAPTR), HL

_fdc_reset:
    ; issue a soft reset to make sure FDC is in reset mode
    xor A
    out (IO_FDC_OPER), A

    ; once reset is finished, controller will interrupt
    ei
    hlt

    ; controller is now reset
    ;  write to OP once again, leaving /SRST high, to initialize AT mode
    ld A, [1 << BIT_FDC_SOFT_RESET]
    out (IO_FDC_OPER), A
    
    ; initialize data rate
    ; 125Kb/s @ FM for 16MHz default clock
    ld A, [1 << BIT_FDC_DATARATE1] 
    out (IO_FDC_CONT), A
    
    ret



; The following routines are provided as simple adapters for the CP/M CBIOS.

; Sets the target track number to be accessed to the value in BC.
;  Then, issues a seek command to move head to the selected track.
;  As of now, this ignores B. Track number is one byte only.
fdc_setTrack:
    ld A, C
    ld (DAT_DISK_TRACK), A
    jp fdc_seek
    

    
; Sets the target sector number to be accessed to the value in BC.
;  As of now, this ignores B. Sector number is one byte only.
fdc_setSector:
    ld A, C
    ld (DAT_DISK_SECTOR), A
    ret



; Sets the address of the data buffer to the value in BC.
fdc_setDataPointer:
    ld (DAT_DISK_DATAPTR), BC
    ret



; Selects the disk given by C
fdc_selectDisk:
    ld A, C
    ld (DAT_DISK_NUMBER), A
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

    ret



; Loads status register 0 from controller. ST0 will be stored in B, the current cylinder
;  index of the drive will be stored in A.
;  Carry used as error bit.
fdc_senseInterruptStatus:
    call fdc_preCommandCheck

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

    ret
 


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

    ; FDC requires us to issue a Sense Interrupt Status command now, or
    ;  the next command will be reported as invalid. Use result bytes to
    ;  verify that stepping was successful
    call fdc_senseInterruptStatus
    jp c, fdc_commandError_invalidState
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

    ; FDC requires us to issue a Sense Interrupt Status command now, or
    ;  the next command will be reported as invalid. Use result bytes to
    ;  verify that stepping was successful
    call fdc_senseInterruptStatus
    jp c, fdc_commandError_invalidState
    bit BIT_FDC_SEEK_END, B
    jp z, fdc_commandError_commandUnsuccessful
    bit BIT_FDC_EQUIPMENT_CHECK, B
    jp nz, fdc_commandError_commandUnsuccessful

    xor A
    jp resetCarryReturn



; Reads data from the selected drive. Uses the parameters (track, sector, target
;  data buffer etc.) as specified in the respective data area fields.
;  If reading succeeds, the carry flag is reset and A will contain $00. If any
;  error occurs, carry wil be set and A contains $01 if the FDC was in an invalid
;  state or $03 if the FDC reported errors after executing the command.
fdc_readData:
    call fdc_preCommandCheck

    ; read command (including MT,MF & SK bits)
    ld A, $26  ; no multitrack, FM mode, skip deleted sectors
    out (IO_FDC_DATA), A

    ; rest of command is handled by common routine
    call fdc_rwCommand
    ret c

    ; all command bytes transferred. controller will start reading now
    ld HL, (DAT_DISK_DATAPTR) ; HL is used as target address during transfer
    call fdc_rTransfer

    ; execution mode ended. let subroutine read result bytes and return
    jp fdc_rwCommandStatusCheck



; Writes data to the selected drive. Uses the parameters (track, sector, target
;  data buffer etc.) as specified in the respective data area fields.
;  If writing succeeds, the carry flag is reset and A will contain $00. If any
;  error occurs, carry wil be set and A contains $01 if the FDC was in an invalid
;  state, $02 if the selected disk was write protected or $03 if the FDC reported
;  errors after executing the command.
fdc_writeData:
    call fdc_preCommandCheck

    ; write command (including MT & MF bits)
    ld A, $05  ; no multitrack, FM mode
    out (IO_FDC_DATA), A

    ; rest of command is handled by common routine
    call fdc_rwCommand
    ret c

    ; controller starts writing now
    ld HL, (DAT_DISK_DATAPTR) ; HL is used as target address during transfer
    call fdc_wTransfer

    ; command execution finished. let helper routine read result bytes and set error codes
    jp fdc_rwCommandStatusCheck



; Formats the currently selected track on the selected drive.
;  Error codes same as for write command.
fdc_format:
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
    ;  at the end of execution phase
    call fdc_waitForExecEndIrq

    ; let subroutine check result bytes and return
    jp fdc_rwCommandStatusCheck



; This routine can be used by both to issue the address part of the command since
;  the command layout for read and write commands is pretty much the same. The first
;  command byte has to be issued to the FDC by the caller to differentiate between R/W.
;  This routine returns right before the caller should call the appropriate transfer
;  method. Error states are handled appropriately. If this routine returns with carry set,
;  the calling routine may return immediatly and should not modify A to preserve the error code.
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
    ld A, $01
    ; this error is probably urecoverable. no use in emptying the data
    ;  buffer. let a reset handle this if neccessary
    jp setCarryReturn     
   
fdc_commandError_writeProtected:
    ld A, $02
    call fdc_emptyResultBuffer
    jp setCarryReturn

fdc_commandError_commandUnsuccessful:
    ld A, $03
    call fdc_emptyResultBuffer
    jp setCarryReturn



; Checks MSR of FDC and returns if exec phase has ended. If not,
;  waits for an IRQ and loops, checking again and so forth.
fdc_waitForExecEndIrq:
    in A, (IO_FDC_STAT)
    bit BIT_FDC_EXEC_MODE, A
    ret z

    ei
    hlt
    
    jp fdc_waitForExecEndIrq



; Experimental half polling/half IRQ routine for read data transfer. Uses interrupts
;  in mode 0 to wait for the FDC. Transfers data read from the FDC to the buffer pointed
;  by (HL), growing upwards. Leaves interrupts disabled when finished.
;  Trashes B,C,D. When finished, HL points to the location AFTER where the last byte was stored.
fdc_rTransfer:
    ; Since the FDC-IVR contains a NOP, we can use the HLT instruction in IM0 to wait for the IRQ
    ;  without the cycle penalty of calling an ISR. When HLT is lifted, the CPU will execute a NOP
    ;  for 6 cycles and then continue after the HLT. This is faster and more elegant than polling the MSR
    di
    ; TODO: maybe we should make sure no RTC interrupts can occur here
    
    ld D, [1 << BIT_FDC_EXEC_MODE] ; constant for bit masking. used instead of immediate value for speed
    ld C, IO_FDC_DATA  ; IO port constant for ini instruction (again used for speed)

_fdc_rTransfer_loop:
    ei
    hlt
    ; HALT was lifted~ check for interrupt cause
    in A, (IO_FDC_STAT)
    and D  ; [1 << BIT_FDC_EXEC_MODE], use D as constant for speed
    ret z  ; if not in exec mode anymore, we're done
    ini
    jp _fdc_rTransfer_loop



; Same as routine above, but writes data read from buffer at (HL) to FDC instead.
fdc_wTransfer:
    di
    ; TODO: maybe we should make sure no RTC interrupts can occur here
    
    ld D, [1 << BIT_FDC_EXEC_MODE] ; constant for bit masking. used instead of immediate value for speed
    ld C, IO_FDC_DATA  ; IO port constant for outi instruction (again used for speed)

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



; Selects and enables motor of drive specified by LSb of B and waits the appropriate delay for
;  spinning up to speed. Disables motor of other drive. If B > 1, both motors are deactivated.
;  There's no spindown time if both motors are deactivated.
; TODO: Routine has errors. Rewrite pls. Also remember to store selected drive in data area
fdc_driveSelect:
    in A, (IO_FDC_OPER)
    and ~[[1 << BIT_FDC_MOTOR_ON_ENABLE_1] | [1 << BIT_FDC_MOTOR_ON_ENABLE_2]] ; turn off both motor bits

    ld A, B
    and $fe
    jp nz, _fdc_driveSelect_disableBoth ; B is > 1. we're done
    
    bit 0, B
    jp nz, _fdc_driveSelect_drive2

    set BIT_FDC_MOTOR_ON_ENABLE_1, A
    res BIT_FDC_DRIVE_SELECT, A
    jp _fdc_driveSelect_spinup

_fdc_driveSelect_drive2:
    set BIT_FDC_MOTOR_ON_ENABLE_2, A
    set BIT_FDC_DRIVE_SELECT, A

_fdc_driveSelect_spinup:
    out (IO_FDC_OPER), A

    ; recommended spinup time is 500ms, which equals 32 RTC delay cycles
    ld A, 32
    call rtc_delay

    ret

_fdc_driveSelect_disableBoth:
    out (IO_FDC_OPER), A
    ret



; Checks initial condition for issuing commands to the FDC. Waits for 
;  RQM signal and checks if DIO signal is set to "input".
;  If not, this routine will reset the controller and try again. This will bring the 
;  controller in the right state eventually or just hang forever.
;  TODO: Might check for busy bit also
fdc_preCommandCheck:
    call fdc_waitForRFM
    ret c  ; if carry set -> FDC awaits input. all ready

    call _fdc_reset
    jp fdc_preCommandCheck



; Waits until the FDC reports a Request For Master. After that, it checks the data direction
;  bit. If it is is set to "waiting for input" the carry flag is reset, if bit is set to "data available"
;  carry bit is set.
fdc_waitForRFM:
    in A, (IO_FDC_STAT)
    bit BIT_FDC_REQUEST_FOR_MASTER, A
    jp z, fdc_waitForRFM

    bit BIT_FDC_DATA_INPUT, A
    jp nz, resetCarryReturn ; DIO = 1 -> data register expects to be read
    jp setCarryReturn



; Waits for approximately 15us
;  (recommended but not required before reading MSR in command phase~
;   we use polling instead. might remove routine if no other useful application is found)
fdc_delay:
    ld A, (CPU_SPEED/67000)/14  ; 1/15us = 67kHz, delay loop is 14 cycles long

_fdc_delay_loop:
    dec A
    jp nz, _fdc_delay_loop

    ret
