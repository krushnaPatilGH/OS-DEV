# Makefile for bootloader and floppy image creation

# Variables
ASM = nasm 
BOOT_SRC = bootloader.asm
BOOT_BIN = boot.bin
FLOPPY_IMG = floppy.img
DD = dd
DD_FLAGS = bs=512 count=2880
DD_BOOT_FLAGS = bs=512 count=1 conv=notrunc

# Default target
all: $(FLOPPY_IMG)

# Assemble the bootloader
$(BOOT_BIN): $(BOOT_SRC)
	$(ASM) -f bin -o $@ $<

# Create the floppy image and write the bootloader
$(FLOPPY_IMG): $(BOOT_BIN)
	$(DD) if=/dev/zero of=$@ $(DD_FLAGS)
	$(DD) if=$< of=$@ $(DD_BOOT_FLAGS)

# Clean up generated files
clean:
	rm -f $(BOOT_BIN) $(FLOPPY_IMG)

# Phony targets
.PHONY: all clean