
;----------------------------STRING DATA-------------------------------

; NOTE: Use only LF ($0A) for linefeeds for saving ROM / portability. 
; The printing routine substitutes CRLF when needed.

str_welcome:
    db 'MINIMON 0.5 FOR KIMP1', $0A
    db 'COPYLEFT 2017 ZALASUS', $0A
    db ' ALL WRONGS REVERSED', $0A, $00
    
str_pressPlayOnTape:
    db 'PRESS PLAY ON TAPE', $0A, $00
    
str_loading:
    db 'LOADING. PLEASE WAIT...', $0A, $00
    
str_unknownCommand:
    db 'UNKNOWN COMMAND', $0A, $00
    
str_syntaxError:
    db 'SYNTAX ERROR', $0A, $00
    
str_diskError:
    db 'DISK ERROR', $0A, $00

str_fdcError:
    db 'DISK CONTROLLER ERROR', $0A, $00

str_fdcWriteProtectedError:
    db 'WRITE PROTECTED ERROR', $0A, $00

str_notBootable:
    db 'NOT BOOTABLE', $0A, $00

str_noExtPresent:
    db 'NO '
str_extPresent:
    db 'EXTENSION BOARD PRESENT', $0A, $00

str_readingHex:
    db 'READING HEX...', $0A, $00

str_hexOK:
    db 'HEX OK', $0A, $00

str_hexError:
    db 'HEX ERROR', $0A, $00

str_hexChecksumError:
    db 'HEX CHECKSUM ERROR', $0A, $00

str_hexRecordEof:
    db ':00000001FF', $0A, $00

str_hitSpace:
    db 'HIT SPACE TO CONTINUE', $0A, $00

str_notImplemented:
    db 'NOT IMPLEMENTED CAUSE ZAL IS A LAZY ASS', $0A, $00


if CONF_INCLUDE_HELP == 0
str_help:
str_disktoolHelp:
    db 'NO HELP AVAILABLE', $0A, $00
else
str_help:
    db 'First character of input denotes command' , $0A
    db 'Whitespace is always optional', $0A
    db 'Defined commands:', $0A
    db 'h          Show this message', $0A
    db 'e S [,E]   Examine address S to E', $0A
    db 's X        Store to address X', $0A
    db 'x[!] X     Call to address X. Use x! to jump to X instead', $0A
    db 'f [R=X]*   Print or modify flag and register stash', $0A
    db 'c S, D, C  Copy C bytes from S to D', $0A
    db 'l X        Load from tape to address X', $0A
    db 'b          Boot from floppy', $0A
    db 'v          Show version', $0A
    db 'p X*       Parses and prints X', $0A
    db 'i          Starts reading of Intel HEX', $0A
    db 'o S [,E]   Dumps S to E as Intel HEX', $0A
    db 'd *        Disk tool. Use dh for help', $0A
    db 'r          Soft reset', $0A
    db 'Arguments in square brackets optional', $0A
    db 'Math expressions in arguments are possible. Allowed: + - * ()', $0A
    db '$ is the last parsed number', $0A
    db 'Numbers interpreted as hexadecimal, prefix with # for decimal', $0A, $00

str_disktoolHelp:
    db 'h        Show this message', $0A
    db 'i        Prints information on current disk', $0A
    db 'd D      Select disk D', $0A
    db 't T      Select track T', $0A
    db 's S      Select sector S', $0A
    db 'r A[,N]  Read N sectors to A', $0A
    db 'w A[,N]  Write N sectors from A', $0A
    db 'f [X]    Format selected disk. Use filler X', $0A, $00

endif
    
str_disktoolInfo:
    db 'DISK', $00, 'LTRK', $00, 'PTRK', $00, 'HEAD', $00, 'SECT', $00

str_disktoolDiskchange:
    db 'DSKCHG=', $00

str_cls:
    db $1B, '[2J', $00 ; the VT100 way to clear screen

