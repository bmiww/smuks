# NOTE: Stealing ideas from here
# https://openqa-bites.github.io/posts/2022/2022-04-13-qemu-aarch64/

# qemu-system-aarch64 \
    # -machine virt \
    # -cpu cortex-a55 \
    # -smp 4 \
    # -m 1024 \
    # -device virtio-gpu-pci \
    # -device virtio-blk,drive=drive0,bootindex=0 \
    # -drive file=/home/bmiww/qemages/danctnix-pine.qcow2,if=none,id=drive0,cache=writeback \
    # -drive if=pflash,format=raw,file=/home/bmiww/qemages/danctnix-pine-efi.img \
    # -drive if=pflash,format=raw,file=/home/bmiww/qemages/danctnix-pine-varstore.img
qemu-system-aarch64 \
    -kernel /home/bmiww/qemages/u-boot \
    -machine virt \
    -cpu cortex-a55 \
    -m 1024 \
    -device virtio-blk-pci,drive=drive0,bootindex=0 \
    -drive file=/home/bmiww/qemages/danctnix-pine.qcow2,if=none,id=drive0,cache=writeback \
    -nographic \
    -no-reboot




    # -dtb /home/bmiww/qemages/merged.dtb \

    # -append 'root=/dev/vda4' \

    # -device virtio-gpu-pci \

# qemu-system-aarch64 -machine virt -machine type=virt -m 2048 -cpu cortex-a57 <img>


# image=openSUSE-Leap-15.3-ARM-JeOS-efi.aarch64-2022.03.04-Build9.443.qcow2
# qemu-system-aarch64 -machine virt,gic-version=max -m 1G -cpu max -smp 2 \
  # -drive "file=$image,if=none,id=drive0,cache=writeback" \
  # -device virtio-blk,drive=drive0,bootindex=0 \
  # -drive file=efi.img,format=raw,if=pflash -drive file=varstore.img,format=raw,if=pflash








# qemu-system-aarch64 -m 1024 -cpu cortex-a57 -M virt \
		    # -nographic \
		    # -pflash flash0.img \
		    # -pflash flash1.img \
		    # -drive if=none,file=vivid-server-cloudimg-arm64-uefi1.img,id=hd0 \
		    # -device virtio-blk-device,drive=hd0 \
		    # -netdev type=tap,id=net0 \
		    # -device virtio-net-device,netdev=net0,mac=$randmac

# qemu-system-aarch64 -m 1024 -cpu cortex-a57 -M virt \
		    # -nographic \
		    # -pflash flash0.img \
		    # -pflash flash1.img \
		    # -drive if=none,file=vivid-server-cloudimg-arm64-uefi1.img,id=hd0 \
		    # -device virtio-blk-device,drive=hd0 \
		    # -netdev type=tap,id=net0 \
		    # -device virtio-net-device,netdev=net0,mac=$randmac




# qemu-system-aarch64 -M virt -cpu cortex-a53 -smp $(nproc) -m 1G \
		    # -accel kvm \
		    # -device usb-ehci \
		    # -drive if=virtio,id=archiso,file=$image,format=raw \
		    # -drive if=virtio,id=sdcard,file=/dev/mmcblk1,format=raw \
		    # -device usb-kbd \
		    # -bios ./u-boot.bin \
		    # -nic user,model=virtio-net-pci \
		    # -serial stdio \
		    # -device virtio-gpu


# qemu-system-aarch64 -cpu cortex-a72 -smp 8 -m 2G \
		    # -accel tcg,thread=multi \
		    # -drive if=virtio,id=archiso,file=$image,format=raw${ro} \
		    # -device nec-usb-xhci \
		    # -device usb-kbd \
		    # -device virtio-tablet \
		    # -bios ./u-boot.bin \
		    # -nic user,model=virtio-net-pci \
		    # -device virtio-gpu
