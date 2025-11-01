# mpmt1 Zephyr implementation

## How to run

1. Initialize your zephyr project following 
    * Tested west 1.4.0 and zephyr-sdk-0.17.0 on Ubuntu 24.04(arm64/amd64)
2. copy mpmt/zephyr into your zephyr project as mpmt1
    * Or use 'zephyr-env.sh' if you want to put this out of the zephyr tree.
3. Build
    * `$ west build -p always -b qemu_x86_64 mpmt1` for (x86_64)
    * `$ west build -p always -b qemu_kvm_arm64 mpmt1` for (arm64)
3. Run
    * `$ west build -t run`
    * You would be asked to input 2 integers, num_context and duration.

## Notes
* In case of qemu_kvm_arm64, it looks like the line 'CONFIG_MP_MAX_NUM_CPUS'
    needs to be commented out.
        
## Notes when invoking directly from qemu-system-*

Instead of running the genrated binary using west, you can run it
directly using qemu-system-*.    

You can find binaries under YOUR_ZEPHYR_PROJECT_DIR/zephyr/build/zephyr/.

    
### ARM64 case

You can run the target binary after you just built.
    
```
$ sudoudo qemu-system-aarch64 \
    -nographic \
    -accel kvm \
    -machine virt \
    -cpu max \
    -smp 4 \
    -m 512 \
    -kernel zephyr.elf \
    -boot strict=on
```

    
### x86_64 case

Below binaries are generated after you run `zephyr build -r run`.
    
1. kernel: zephyr-qemu-locore.elf
2. loader: zephyr-qemu-main.elf
    
```
$ sudo qemu-system-x86_64 -accel kvm \
    -cpu qemu64  \
    -machine q35 \
    -machine acpi=off \
    -smp 8 \
    -m 128 \
    -serial mon:stdio \
    -serial null \
    -net none \
    -nographic \
    -no-reboot \
    -kernel zephyr-qemu-locore.elf \
    -device loader,file=zephyr-qemu-main.elf
```
    
## REFERENCES:
* Getting started (installation)
    * https://docs.zephyrproject.org/latest/develop/getting_started/index.html
* x86/x86_64 qemu target board
    * https://docs.zephyrproject.org/latest/boards/qemu/x86/doc/index.html
* ARM64 qemu target board
    * https://docs.zephyrproject.org/latest/boards/qemu/kvm_arm64/doc/index.html
