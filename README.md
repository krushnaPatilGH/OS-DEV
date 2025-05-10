# OS-dev 

  This is a personal project created for fun and earning, not intended for commercial use or production environments, as it lacks safety and robustness. Currently, it’s a minimal operating system with basic functionality: it reads a FAT12 file system, loads code into memory, and transitions to 64-bit long mode with paging and protection enabled. Future updates will introduce additional features, but for now, it’s purely an educational endeavor driven by curiosity!


### files
- build
- src ->
  - bootloader.asm
  - stage2.asm
- makefile
- Readme.md

### current updates
- fixed stage2.asm loading offset error
- 64 bit/ long mode of cpu enabled
- LME paging enabled

### To Run
To build the project

``` 
make all
```

To clean the project 
```
make clean
```

### Where to find the compiled OS
after building the project, the OS i.e. a floppy image is stored in the build dir with the name "floppy.img"
