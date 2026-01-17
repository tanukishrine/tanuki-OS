.PHONY:

default: build run clean

clean:
	rm *.img

build: disk.img

run: 
	qemu-system-i386 \
	-drive format=raw,file=disk.img

disk.img: bootloader.img kernel.img source.bin
	cat bootloader.img kernel.img source.bin > disk.img

# 512 bytes
bootloader.img: bootloader.asm
	nasm $< -o $@

# 8192 bytes => 16 sectors
kernel.img: kernel.asm
	nasm $< -o $@
	truncate -s 8192 $@

source.bin: source.fs blocks.py
	python3 blocks.py $< $@

load: build
	truncate -s 4096 disk.img
	diskutil unmountDisk /dev/disk4
	sudo dd if=disk.img of=/dev/rdisk4 bs=512 status=progress
	sync
	diskutil eject /dev/disk4
