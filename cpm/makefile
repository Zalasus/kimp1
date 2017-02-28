
all: clean bin/cpm22_full.bin

clean:
	rm -rf zout/* bin/*
	mkdir -p bin

zout/cbios.cim: cbios.asm
	zmac -z cbios.asm

zout/cpm22.cim: cpm22.asm
	zmac -z cpm22.asm

zout/bootloader.cim: bootloader.asm
	zmac -z bootloader.asm

bin/cpm22_trimmed.bin: zout/cpm22.cim
	dd if=./zout/cpm22.cim of=./bin/cpm22_trimmed.bin bs=1 count=5632

bin/cpm22_full.bin: bin/cpm22_trimmed.bin zout/cbios.cim zout/bootloader.cim
	cat zout/bootloader.cim bin/cpm22_trimmed.bin zout/cbios.cim > bin/cpm22_full.bin
	objcopy -I binary -O ihex --change-addresses=57344 bin/cpm22_full.bin bin/cpm22_full.hex


.PHONY: all clean
.DEFAULT_GOAL := all