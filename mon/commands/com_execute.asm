
;-------------------------------
;       Execute command
;-------------------------------

; Monitor command to jump/call to given location.

command_execute:
    ; check for ! token to see if user wants to jump instead of call
    ld A, (HL)
    cp MON_COM_EXEC_JUMP
    jp z, _command_execute_nocall

    ; there's no indirect call, so we put the return address on the stack ourself.
    ;  let called code return to cleanup routine (let's just hope the program doesn't ruin anything)
    ld DE, _command_execute_cleanup
    push DE
    dec HL  ; we simply fall into nocall section. dec HL so HL is not affected when parsing

_command_execute_nocall:
    inc HL ; skip ! token for expression parser

    call expression
    jp c, monitor_syntaxError

    ; push target address. we will simulate indirect jump by using a return
    ;  (can't use jp (HL) since we must restore register stash before jumping)
    push DE
    jp restoreRegisterStash ; this will return to our fake destination, which is the given address
    
_command_execute_cleanup:
    ; we end up here when called routine returns
    ;  stash register file in buffer to be examined by Register command
    call stashRegisters

    jp monitorPrompt_loop



