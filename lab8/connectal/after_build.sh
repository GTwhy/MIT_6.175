pkill bsim
pkill ubuntu.exe
sudo chown -R $USER ../
cp ../programs/build/assembly/bin/bpred_ras.riscv bluesim/program
make run.bluesim > log
pkill bsim
pkill ubuntu.exe
