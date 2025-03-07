qemu-system-x86_64 \
    -machine q35,accel=kvm \
    -enable-kvm \
    -m 2048 \
    -drive file=/home/toms/qemages/smuks-arch.qcow2,format=qcow2,media=disk,if=virtio \
    -vga virtio -device virtio-gpu,max_outputs=0,xres=1920,yres=1200 \
    -net user,hostfwd=tcp::10022-:22,hostfwd=tcp::10023-:10023,hostfwd=tcp::25252-:25252 \
    -net nic,model=virtio \
    -display none -spice port=5900,addr=127.0.0.1,disable-ticketing=on \
    -chardev spicevmc,id=charchannel0,name=vdagent \
    -device virtio-serial-pci,id=virtio-serial0 -device virtserialport,bus=virtio-serial0.0,nr=1,chardev=charchannel0,id=channel0,name=com.redhat.spice.0
