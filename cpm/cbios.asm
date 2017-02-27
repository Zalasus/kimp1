
;-----------------------------------
;        CBIOS FOR CP/M 2.2
;
;       for the KIMP1 system
;
;  written for the zmac assembler
;
;      Copyleft 2017 Zalasus
;       all wrongs reversed
;-----------------------------------

    ; include definition file for KIMP1 computer
    include ../kimp1def.inc

    ; CP/M config (shared by bootloader and BDOS assembly files)
    include conf.inc


; interrupt locations and corresponding restart instructions
INT_FDC_VECTOR:   equ $0008
INT_FDC_RESTART:  equ $CF

INT_RTC_VECTOR:   equ $0010
INT_RTC_RESTART:  equ $D7


bias:    equ (CONF_CPM_MEM-20)*1024
ccp:     equ 3400H+bias ;base of ccp
bdos:    equ ccp+806h   ;base of bdos
bios:    equ ccp+1600h  ;base of bios
    
    org bios

; CBIOS jump vector
    jp  boot        ;cold start
    jp  wboot       ;warm start
    jp  const       ;console status
    jp  conin       ;console character in
    jp  conout      ;console character out
    jp  listout     ;list character out (renamed as zmac uses list as keyword)
    jp  punch       ;punch character out
    jp  reader      ;reader character out
    jp  home        ;move head to home position
    jp  seldsk      ;select disk
    jp  settrk      ;set track number
    jp  setsec      ;set sector number
    jp  setdma      ;set dma address
    jp  read        ;read disk
    jp  write       ;write disk
    jp  listst      ;return list status
    jp  sectran     ;sector translate

    

; ------------------- FIXED DATA AREAS -----------------------

DAT_CPM_DPH_BASE:  ; base address for disk parameter header
    ; DPH for drive 0
    dw $0000           ; sector translation table address (no translation done)
    dw $0000           ; current track number
    dw $0001           ; current sector number
    dw $0000           ; current directory number
    dw DAT_CPM_DIRBUF  ; directory buffer address, shared by all drives
    dw DAT_CPM_DPB     ; DPB address
    dw DAT_CPM_CSV_0   ; check sum vector address
    dw DAT_CPM_ALV_0   ; allocation vector address

    ; DPH for drive 1
    dw $0000           ; sector translation table address (no translation done)
    dw $0000           ; current track number
    dw $0001           ; current sector number
    dw $0000           ; current directory number
    dw DAT_CPM_DIRBUF  ; directory buffer address, shared by all drives
    dw DAT_CPM_DPB     ; DPB address
    dw DAT_CPM_CSV_1   ; check sum vector address
    dw DAT_CPM_ALV_1   ; allocation vector address    


; Disk parameter block for allocation block size of 2kiB
DAT_CPM_DPB:
    dw 36    ; count of 128-byte sectors per track
    db 4     ; block shift factor
    db $0f   ; block mask
    db 0     ; extent mask
    dw 355   ; storage capacity (index of last allocation block, 0-based)
    dw 511   ; number of last directory entry
    db $ff   ; allocation table stuff
    db $00   ;     "
    db 128   ; checksum vector size
    dw 2     ; number of reserved track at start of disk



str_welcome:
    db '63k CP/M 2.2 FOR THE KIMP1', $0D, $0A, '(C) 1979 DIGITAL RESEARCH'
    db $0D, $0A, $00

str_bootFail:
    db 'FAILED TO BOOT', $0D, $0A, $00

str_biosPanic:
    db 'BIOS PANIC', $0D, $0A, $00

;------------------------CP/M BIOS ROUTINES----------------------------
    
; cold boot. only called by bootloader. this does not have to load anything since the bootloader
;  will already have loaded the BIOS, BDOS and CCP once it transfers control to here.
;  All that is left to do is turning off the ROM, initializing interrupt vectors 
;  (as they were previously stored in ROM) and the rest of the zeropage and eventually giving 
;  control to CP/M.
boot:
    ; Turn off ROM mapping.
    ;  After ROM is turned off interrupt vectors need to be redefined.
    ;  Don't interrupt during this period.
    di 
    ld A, [1 << BIT_TCCR_ROM_GATE] | [1 << BIT_TCCR_C2_GATE]  ; don't turn off the UART timer
    out (IO_TCCR), A

    ; print logon string
    ld HL, str_welcome
    call printString

    jp gocpm
    
    

; this is called when the CCP needs to be reloaded. basically only called when CP/M has been
;  running on this machine before, as such we don't need to fiddle with ROM mapping etc.
wboot:
    ; since CP/M uses a different stack location, we should restore a safe location at the end
    ; of memory for this routine. CP/M can change it back if it wishes to do so
    ld SP, $0000

    xor A
    ld (DAT_DISK_NUMBER), A
    ld (DAT_DISK_TRACK), A
    ld A, 2   ; we reserve sector 1 for later. a dedicated bootloader perhaps
    ld (DAT_DISK_SECTOR), A

    ld C, 0   ; number of sectors loaded

_wboot_loop:
    ; calculate target address for read
    ld HL, ccp
    ld A, C
    add A   ; shift left sector count 1 bit
    add H   ; add to H <=> shift left 8 bits -> sector count times 512
    ld H, A
    ld (DAT_DISK_DATAPTR), HL

    ld D, 10  ; 10 retries before fail
_wboot_retry:
    call fdc_readData
    jp nc, _wboot_noError
    dec D
    jp nz, _wboot_retry
    jp bootFail
_wboot_noError:

    inc C
    ld A, C
    cp 11   ; CCP+BDOS is 5632 bytes long -> exactly 11 sectors of 512 bytes needed
    jp z, gocpm  ; all sectors loaded. give control back to ccp (after intializing zeropage)

    ; increment sector number
    ld A, (DAT_DISK_SECTOR)
    inc A
    ld (DAT_DISK_SECTOR), A
    cp 10     ; check if carry to next track is neccessary
    jp nz, _wboot_loop
    ld A, 1
    ld (DAT_DISK_SECTOR), A
    ld A, (DAT_DISK_TRACK)
    inc A
    ld (DAT_DISK_TRACK), A
    call fdc_seek
    jp c, bootFail  ; no use in retrying seek operation
    jp _wboot_loop

    

gocpm:
    ; redefine interrupt vectors
    ;  note that these vectors are different than the ones used by the
    ;  monitor as CP/M reserves some interrupt vectors
    ld A, $C3    ; jp instruction

    ; fdc interrupt at rst 08h
    ld (INT_FDC_VECTOR), A
    ld HL, fdc_isr
    ld (INT_FDC_VECTOR+1), HL

    ; rtc interrupt at rst 10h
    ld (INT_RTC_VECTOR), A
    ld HL, rtc_isr
    ld (INT_RTC_VECTOR+1), HL

    ; redefine IVRs
    ld A, INT_FDC_RESTART
    out (IO_IVR_FDC), A

    ld A, INT_RTC_RESTART
    out (IO_IVR_RTC), A

    ei  ; now interrupts are safe again

    ; initialize system vectors in zeropage
    ; address 0 contains a jump to wboot
    ld A, $C3  ; jp instruction
    ld ($0000), A
    ld HL, wboot
    ld ($0001), HL

    ; address 5 contains a jump to BDOS
    ld ($0005), A
    ld HL, bdos
    ld ($0006), HL

    ld HL, $0080  ; initalize default DMA address
    call setdma

    call printNewLine

    ld C, $00  ; ccp will select this drive

    jp ccp  ; give control to CP/M
    
    

; Prints error message and hangs
bootFail:
    ld HL, str_bootFail
    call printString
_bootFail_hang:
    jp _bootFail_hang



; Same as above, but for runtime errors
biosPanic:
    ld HL, str_biosPanic
    call printString
_biosPanic_hang:
    jp _biosPanic_hang



; sets A to $FF if console has readable byte, $00 if not
const:
    in A, (IO_UART_COM) ; read in status byte of UART
    and [1 << BIT_UART_RXRDY]
    jp z, _const_notReady ; bit is zero -> not ready
    ld A, $ff
    ret
_const_notReady:
    xor A ; set A to 0
    ret


    
; prints character in C to console
conout:
    in A, (IO_UART_COM) ; read in status byte of UART
    and [1 << BIT_UART_TXRDY]
    jp z, conout
    
    ; UART is ready to send another byte now
    ld A, C
    out (IO_UART_DAT), A ; UART will start sending the byte now
    
    ret


    
; reads character from console into A
conin:
    in A, (IO_UART_COM) ; read in status byte of UART
    and [1 << BIT_UART_RXRDY]
    jp z, conin ; do this until UART has a valid byte
    
    in A, (IO_UART_DAT) ; read in data byte
    
    ret
    


; Dummy method for list/punch since we don't have such
;  devices
listout:
punch:
    ld A, C
    ret



; Returns status of listing device. Since there
;  is none, returns not ready always.
listst:
    xor A
    ret



; No, there is no paper strip reader either. Return
;  $1A (end-of-file) as suggested by Alteration Guide.
reader:
    ld A, $1A
    ret



; Move currently selected drive to track 0
home:
    call fdc_recalibrate
    jp c, biosPanic  ; if seeking fails, there is no way to report back to CP/M. panic in that case
    ret



; Selects drive in C, return DPH address in HL
seldsk:
    ; check if disk exists
    ld A, C
    and $fe  ; only last bit may be set (only support 2 drives)
    jp nz, _seldsk_nonExistentDrive

    ; get DPH address
    ;  no shifting etc. needed. we only have two drives. condition is faster
    ld A, C
    ld (DAT_CPM_DISK), A
    or A
    jp nz, _seldsk_disk1
    
    ld HL, DAT_CPM_DPH_BASE
    ret

_seldsk_disk1:
    ld HL, DAT_CPM_DPH_BASE+16
    ret

_seldsk_nonExistentDrive:
    ld HL, $0000
    ret



; Selects track in BC
settrk:
    ld A, C
    ld (DAT_DISK_TRACK), A
    call fdc_seek
    jp c, biosPanic  ; if seeking fails, there is no way to report back to CP/M. panic in that case
    ret



; Selects sector in BC
setsec:
    ld A, C
    ld (DAT_DISK_SECTOR), A
    ret

    

; Selects DMA address in BC
setdma:
    ld (DAT_CPM_DMAPTR), BC
    ret



read:
write:
    ; need to figure this blocking/deblocking stuff out somehow



; Translates sector given in BC using translation table in DE.
;  Returns translated sector in HL
sectran:
    ld H, B   ; no translation done
    ld L, C
    ret


;----------------------------------------------------------------------
    
    
; prints CRLF characters
printNewLine:
    ld A, $0D
    call conout
    ld A, $0A
    call conout
    ret
    


printString:
    ld A, (HL) ; fetch byte
    or A
    ret z ; if byte is zero, we are done
    
    ;byte is not zero, so print it out
    ld C, A
    call conout
    
    inc HL ; increment HL and loop
    jp printString
    


resetCarryReturn:
    scf
    ccf
    ret



setCarryReturn:
    scf
    ret





; Note: All following IO routines only trash A, B and HL. C, DE are safe to use

;=============================================
;
;          Floppy IO for Minimon
;
; for the WD37C65 floppy subsystem controller
;
;=============================================


; fixed config for 512 bytes/sector MFM
FDC_PARAM_N:       equ 2
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

    ret



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
    jp z, _fdc_enableMotor_turnOn ; wrong drive was selected. need to spin up other motor
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
    ; create timeout do disable motor again
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
    ld A, $51   ; unload time and step rate
    out (IO_FDC_DATA), A

    call fdc_waitForRFM
    ld A, [$28 << 1] | 1 ; load time and Non-DMA-Bit (set)
    out (IO_FDC_DATA), A

    ; no exec or result phase

    jp resetCarryReturn



; Loads status register 0 from controller. ST0 will be stored in B, the current cylinder
;  index of the drive will be stored in A. This routine will not perform resets in case 
;  the controller is busy. It only waits for the RQM signal. No error flags are generated.
fdc_senseInterruptStatus:
    ; we can't make a pre-command check here since a reset would cause
    ;  an interrupt, which would then cause the ISR to issue another SENSEI
    call fdc_waitForRFM

    ; sense interrupt status command
    ld A, $08
    out (IO_FDC_DATA), A
    ; command issued. this command has no execution phase and won't produce
    ;  an interrupt. data can be read back right away (hopefully)

    ; read ST0
    call fdc_waitForRFM
    in A, (IO_FDC_DATA)
    ld B, A

    ; read current cylinder index
    call fdc_waitForRFM
    in A, (IO_FDC_DATA)

    ret



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
    ld A, (DAT_DISK_NUMBER)
    and $03 ; we only need the two lower bits for the drive number. HS is 0
    out (IO_FDC_DATA), A
    
    ; the FDC is stepping the drive now. we need to wait until stepping is finished
    ;  and head has has reached track 0.
    call fdc_waitForSeekEnd

    ; stepping is finished. no result phase.

    ; load interrupt status byte as received by ISR
    ld A, (DAT_DISK_INT_SR0)
    bit BIT_FDC_SEEK_END, A
    jp z, setCarryReturn
    bit BIT_FDC_EQUIPMENT_CHECK, A
    jp nz, setCarryReturn

    xor A
    ld (DAT_DISK_TRACK), A  ; track is now 0

    jp resetCarryReturn



; Positions the head of selected drive over selected track
fdc_seek:
    call fdc_preCommandCheck
    ret c

    call fdc_enableMotor

    ; seek command
    ld A, $0f
    out (IO_FDC_DATA), A

    ; unit & head address
    call fdc_waitForRFM
    ld A, (DAT_DISK_NUMBER)
    and $03  ; mask out lower 2 bits. Head select is always 0
    out (IO_FDC_DATA), A

    ; cylinder address
    call fdc_waitForRFM
    ld A, (DAT_DISK_TRACK)
    out (IO_FDC_DATA), A

    ; controller is stepping now. no bytes are transferred, so just wait for
    ;  IRQ at end of exec phase
    call fdc_waitForSeekEnd

    ; no result phase.

    ; load interrupt status byte as received by ISR
    ld A, (DAT_DISK_INT_SR0)
    bit BIT_FDC_SEEK_END, A
    jp z, setCarryReturn
    bit BIT_FDC_EQUIPMENT_CHECK, A
    jp nz, setCarryReturn

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

    ; all command bytes transferred. controller will start reading now
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
    ret c

    call fdc_enableMotor

    ; write command (including MT & MF bits)
    ld A, $45  ; no multitrack, MFM mode
    out (IO_FDC_DATA), A

    ; rest of command is handled by common routine
    call fdc_rwCommand

    ; controller starts writing now
    call fdc_wTransfer

    ; command execution finished. let helper routine read result bytes and set error codes
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
    ld A, (DAT_DISK_NUMBER)
    and $03  ; mask out lower 2 bits. Head select is always 0 TODO: add support for both sides
    out (IO_FDC_DATA), A

    ; cylinder address
    call fdc_waitForRFM
    ld A, (DAT_DISK_TRACK)
    out (IO_FDC_DATA), A

    ; head address (repeated value, same as in unit address)
    call fdc_waitForRFM
    xor A ; head is always 0 for now
    out (IO_FDC_DATA), A
    
    ; sector address
    call fdc_waitForRFM
    ld A, (DAT_DISK_SECTOR)
    out (IO_FDC_DATA), A

    ; bytes per sector
    call fdc_waitForRFM
    ld A, FDC_PARAM_N
    out (IO_FDC_DATA), A

    ; end of track
    ;  Some sources say this is the sector count to be accessed, some say it's the final
    ;  sector address. The datasheet is not clear on that. Assume it's the address, so
    ;  this is the same as sent as sector address when we want to read/write only one sector
    call fdc_waitForRFM
    ld A, (DAT_DISK_SECTOR)
    out (IO_FDC_DATA), A

    ; gap length
    call fdc_waitForRFM
    ld A, FDC_PARAM_GPL_RW
    out (IO_FDC_DATA), A

    ; data length
    call fdc_waitForRFM
    xor A  ; with sectors > 128 bytes this field is meaningless
    out (IO_FDC_DATA), A

    ret



; Loads the result data buffer with result bytes from the FDC. Then checks
;  the status bytes returned in result phase of a read/write or format command
;  and sets the carry flag accordingly.
fdc_rwCommandStatusCheck:
    call fdc_readResultBuffer

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
fdc_readResultBuffer:
    ld HL, DAT_DISK_RES_BUFFER
    ld B, 7  ; read all 7 result bytes

_fdc_readResultBuffer_loop:
    call fdc_waitForRFM
    in A, (IO_FDC_DATA)
    ld (HL), A
    inc HL
    djnz _fdc_readResultBuffer_loop

    ret
    


; Checks MSR repeatedly and returns if no drives are stepping anymore.
;  This implies that an IRQ has been made and the ISR issued a SENSEI
;  command, as only then the busy bits get reset.
fdc_waitForSeekEnd:
    in A, (IO_FDC_STAT)
    and $1f ; all busy bits must be 0
    jp nz, fdc_waitForSeekEnd  ; no halting here!! polling is safer since interrupts are handled async for seeks
    ret



; Experimental half polling/half IRQ routine for read data transfer. Uses interrupts
;  in mode 0 to wait for the FDC. Transfers data read from the FDC to the buffer pointed
;  by (HL), growing upwards. There is no bounds checking. Transfer ends when controller ends it.
;  Trashes B,C,D. When finished, HL points to the location AFTER where the last byte was stored.
;  Since this routine has some overhead before actually able to react to IRQs, the controller may 
;  be overrun when it locks the PLL and finds the start sector faster than this routine can enter
;  it's transfer loop. This, however, seems unlikely to happen in practice.
fdc_rTransfer:
    exx  ; use alternative registers for transfer so we trash less
    call rtc_disableInterrupt ; make sure transfer is not interrupted

    ; Initialize IVR to a NOP so we can use the HLT instruction in IM0 to wait for the IRQ
    ;  without the cycle penalty of calling an ISR. When HLT is lifted, the CPU will execute a NOP
    ;  for 6 cycles and then continue after the HLT. This is faster and more elegant than polling the MSR
    ld A, $00
    out (IO_IVR_FDC), A  

    ld HL, (DAT_DISK_DATAPTR) ; HL is used as target address during transfer
    ld D, [1 << BIT_FDC_EXEC_MODE] ; constant for bit masking. used instead of immediate value for speed
    ld C, IO_FDC_DATA  ; IO port constant for ini instruction (again used for speed)

    call _fdc_rTransfer_loop ; call loop instead of jumping to it. a conditional return is faster than a branch

    ; restore original IVR contents
    ld A, INT_FDC_RESTART
    out (IO_IVR_FDC), A

    call rtc_enableInterrupt

    exx
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
    exx
    call rtc_disableInterrupt ; make sure transfer is not interrupted

    ; initialize IVR to alternative NOP since we want to save cycles
    ld A, $00
    out (IO_IVR_FDC), A

    ld HL, (DAT_DISK_DATAPTR) ; HL is used as target address during transfer
    ld D, [1 << BIT_FDC_EXEC_MODE] ; constant for bit masking. used instead of immediate value for speed
    ld C, IO_FDC_DATA  ; IO port constant for outi instruction (again used for speed)

    call _fdc_wTransfer_loop

    ld A, INT_FDC_RESTART
    out (IO_IVR_FDC), A

    call rtc_enableInterrupt

    exx
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




; Waits until the FDC reports a Request For Master
fdc_waitForRFM:
    in A, (IO_FDC_STAT)
    and [1 << BIT_FDC_REQUEST_FOR_MASTER]
    jp z, fdc_waitForRFM
    ret



; Interrupt handler for floppy controller. This is normally used when controller is idling or
;  for seek/recalibrate commands. During read/write operations and other commands with
;  result phase the IVR is temporarily overwritten with a NOP and this routine is not used.
fdc_isr:
    ex AF, AF'
    exx

    ; check for interrupt cause using SENSEI command
    call fdc_senseInterruptStatus
    ; store in result buffer
    ld A, B
    ld (DAT_DISK_INT_SR0), A
    
    exx
    ex AF, AF'
    ei
    ret




;=========================================
;
;          RTC IO for Minimon
;
;    for the MSM6242B real time clock
;
;=========================================

; Uses the RTC interrupt to delay a time interval given by A. The delay time
;  equals A*1/64 seconds +/- a few clock cycles. Will enable interrupts.
;  Any pending timouts will get called as soon as the delay is over, regardless
;  of their previously remaining time.
rtc_delay:
    di

    ld (DAT_RTC_COUNTER), A

    ; reset internal second counter
    ld A, [1 << BIT_RTC_24_12] | [1 << BIT_RTC_REST]
    out (IO_RTC_CF), A
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
    ld A, [1 << BIT_RTC_24_12]
    out (IO_RTC_CF), A

    ld A, $af
    out ($ff), A

    ld A, $02  ; interrupt mode, 1/64 interval, mask bit = 0
    out (IO_RTC_CE), A

    ei

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
    exx
    ex af,af'

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
    ld HL, $0000   ; delete timeout
    ld (DAT_RTC_CALLBACK), HL

_rtc_isr_end:
    exx
    ex af, af'
    ei
    ret




    
    
bios_end:



; ----------------- NON-FIXED DATA AREA -------------------

; The following are just reserved space. They do not need to be
;  included in the final image

DAT_RTC_COUNTER:
    ds 1

DAT_RTC_CALLBACK:
    ds 2

DAT_DISK_MOTOR_DRIVE:
    ds 1

DAT_DISK_INT_SR0:
    ds 1

DAT_DISK_TRACK:
    ds 1

DAT_DISK_SECTOR:
    ds 1

DAT_DISK_DATAPTR:
    ds 2

DAT_DISK_RES_BUFFER:
    ds 7

DAT_DISK_NUMBER:
DAT_CPM_DISK:    ; currently selected disk
    ds 1

DAT_CPM_DMAPTR:
    ds 2

DAT_CPM_CSV_0:
    ds 128

DAT_CPM_ALV_0:
    ds 46

DAT_CPM_CSV_1:
    ds 128

DAT_CPM_ALV_1:
    ds 46

DAT_CPM_DIRBUF:
    ds 128

; Buffer for blocking/deblocking
DAT_CPM_HOSTBUF:
    ds 512

    end
    
    
    
    
    
    
    
    