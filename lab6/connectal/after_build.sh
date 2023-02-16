pkill bsim
pkill ubuntu.exe
sudo chown -R $USER ../
cp ../programs/build/assembly/bin/addi.riscv bluesim/program
make run.bluesim > log
pkill bsim
pkill ubuntu.exe
