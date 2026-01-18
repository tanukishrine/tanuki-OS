# .PHONY:

default: build run

build: disk.img

run: 
	qemu-system-i386 \
	-drive format=raw,file=disk.img

clean:
	rm *.bin

load: 
	diskutil unmountDisk /dev/disk4
	sudo dd if=disk.img of=/dev/rdisk4 bs=512 status=progress
	sync
	diskutil eject /dev/disk4

# 512 bytes
bootloader.bin: bootloader.asm
	nasm $< -o $@

# 8192 bytes => 16 sectors
kernel.bin: kernel.asm
	nasm $< -o $@
	truncate -s 8192 $@

# 32768 bytes => 64 sectors
source.bin: source.fs blocks.py
	python3 blocks.py $< $@
	truncate -s 32768 $@

# 41472 bytes
disk.img: bootloader.bin kernel.bin source.bin
	cat bootloader.bin kernel.bin source.bin > disk.img

truncate-%:
	@size=$$(stat -f %z $*); \
	newsize=$$(( (size + 511) / 512 * 512)); \
	truncate -s $$newsize $*
