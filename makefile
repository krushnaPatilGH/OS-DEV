# Makefile for bootloader and floppy image creation

# Variables
ASM = nasm 
BOOT_SRC = bootloader.asm
STAGE2_SRC = stage2.asm
BOOT_BIN = boot.bin 
STAGE2_BIN = stage2.bin
FLOPPY_IMG = floppy.img
DD = dd
DD_FLAGS = bs=512 count=2880
DD_BOOT_FLAGS = bs=512 count=1 conv=notrunc
BUILD_DIR = build
SOURCE_DIR = src

# Default target
all: $(BUILD_DIR)/$(FLOPPY_IMG)

# Assemble the bootloader
$(BUILD_DIR)/$(BOOT_BIN): $(SOURCE_DIR)/$(BOOT_SRC) 
	mkdir -p build
	$(ASM) -f bin -o $@ $<

$(BUILD_DIR)/$(STAGE2_BIN): $(SOURCE_DIR)/$(STAGE2_SRC)
	$(ASM) -f bin -o $@ $<

# Create the floppy image and write the bootloader
$(BUILD_DIR)/$(FLOPPY_IMG): $(BUILD_DIR)/$(BOOT_BIN) $(BUILD_DIR)/$(STAGE2_BIN)
	$(DD) if=/dev/zero of=$@ $(DD_FLAGS)
	$(DD) if=$< of=$@ $(DD_BOOT_FLAGS)
	mcopy -i $(BUILD_DIR)/$(FLOPPY_IMG) $(BUILD_DIR)/$(STAGE2_BIN) '::STAGE2.BIN'


# Clean up generated files
clean:
	rm -f $(BUILD_DIR)/$(BOOT_BIN) $(BUILD_DIR)/$(STAGE2_BIN) $(BUILD_DIR)/$(FLOPPY_IMG)

# Phony targets
.PHONY: all clean