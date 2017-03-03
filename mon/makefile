
all: clean zout/minimon.cim bin/minimon.bin

clean:
	rm -rf zout/* bin/*
	mkdir -p bin

zout/minimon.cim:
	zmac -z minimon.asm

bin/minimon.bin: zout/minimon.cim
	dd if=./zout/minimon.cim of=./bin/minimon.bin bs=1 count=8192

flash: bin/minimon.bin
	minipro -p "TMS2764 @DIP28" -w bin/minimon.bin

.PHONY: all clean flash
.DEFAULT_GOAL := all