
all: clean zout/minimon.hex

clean:
	rm -rf zout/

zout/minimon.hex:
	zmac -z minimon.asm


.PHONY: all clean
.DEFAULT_GOAL := all