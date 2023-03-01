pkill bsim
pkill ubuntu.exe
sudo chown -R $USER ../
./run_asm.sh > $1_asm.log 2>&1
grep -v -r 'warning' $1_asm.log | sed '/^$/d' > temp_asm_$1.log
grep -v -r '/home/' temp_asm_$1.log | sed '/^$/d' > $1_asm.log

./run_bmarks.sh > $1_bmark.log 2>&1
grep -v -r 'warning' $1_bmark.log | sed '/^$/d' > temp_bmark_$1.log
grep -v -r '/home/' temp_bmark_$1.log | sed '/^$/d' > $1_bmark.log

rm temp*.log
