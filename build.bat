del target/*.d81
del target/*.lst
del target/*.lbl
del target/c128
64tass --cbm-prg -a src\main.asm -l target\main.lbl -L target\main.lst -o target\c128
cd target
c1541 -format "c128,01" d81 c128.d81
c1541 -attach c128.d81 -write c128 c128
c1541 -attach c128.d81 -write ../src/chargen-390059-01.bin chargen.bin
c1541 -attach c128.d81 -write ../src/kernal-318020-05.bin kernal.bin
c1541 -attach c128.d81 -write ../src/basiclo-318018-04.bin basiclo.bin
c1541 -attach c128.d81 -write ../src/basichi-318019-04.bin basichi.bin
c1541 -attach c128.d81 -write ../src/roms.bin roms.bin
cd ..