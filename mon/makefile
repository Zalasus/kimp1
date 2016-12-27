
all: clean build/minimon.hex

clean:
	mkdir -p build
	rm -f build/minimon.bin build/minimon.hex


build/minimon.bin:
	z80asm -o build/minimon.bin minimon.asm

build/minimon.hex: build/minimon.bin
	objcopy -I binary -O ihex build/minimon.bin build/minimon.hex


.PHONY: all clean
.DEFAULT_GOAL := all