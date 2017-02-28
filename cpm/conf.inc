
;-----------------------------------------------
;
;           CP/M configuration
;
; shared by bootloader and BDOS assembly files
;
;-----------------------------------------------


; Memory size in kiB
; NOTE: We have a 64k system, but our BIOS is unusually large, so we make a compromise here
CONF_CPM_MEM:     equ 63  

