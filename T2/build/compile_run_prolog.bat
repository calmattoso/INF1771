cd ../src

g++ -c -w -O3 -D__WINDOWS__ -D_WINDOWS -D__SWI_PROLOG__ -D__SWI_EMBEDDED__ -I"c:/program files (x86)/swipl/include" -o main.obj run_prolog.cpp

g++ -o ../bin/run_prolog_x86.exe main.obj -L"c:/program files (x86)/swipl/lib" -lswipl
del main.obj

cd ../build