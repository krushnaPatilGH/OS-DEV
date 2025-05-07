# OS-dev 

### files
- build
- src ->
  - bootloader.asm
  - stage2.asm
- makefile
- Readme.md

### current updates
- Restructured the directories of the project
- Implemented stage2.asm, the transfer of programflow is successful

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
