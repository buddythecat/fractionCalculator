cd obj
\masm32\bin\cvtres /machine:ix86 ../res/calcDisplay.res
\masm32\bin\ml /c /coff "../src/fracCalc.asm"
\masm32\bin\ml /c /coff "../src/stack.asm"
\masm32\bin\ml /c /coff "../src/fraction.asm"
\masm32\bin\ml /c /coff "../src/stack.asm"
cd ../bin/
\masm32\bin\Link /SUBSYSTEM:CONSOLE /OUT:fCalcConsole.exe "../obj/fracCalc.obj" "../obj/fraction.obj" "../obj/stack.obj" "../res/calcDisplay.res"
dir
cd ..