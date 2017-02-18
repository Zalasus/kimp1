
all: clean zout/iomon.cim bin/iomon.bin

clean:
	rm -rf zout/* bin/*
	mkdir -p bin

zout/iomon.cim:
	zmac -z iomon.asm

bin/iomon.bin: zout/iomon.cim
	dd if=./zout/iomon.cim of=./bin/iomon.bin bs=1 count=8192

.PHONY: all clean
.DEFAULT_GOAL := all