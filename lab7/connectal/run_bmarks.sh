#!/bin/bash

asm_tests=(
	median
	multiply
	qsort
	towers
	vvadd
	)

# asm_tests=(		#dump	  log_isnt 
# 	median 		#20a0006f 1c20006f
# 	multiply 	#1c20006f 2ce0006f
# 	qsort 		#2ce0006f 5d20006f
# 	towers 		#5d20006f 1c20006f
# 	vvadd 		#1c20006f 1c20006f
# 	)

vmh_dir=../programs/build/benchmarks/bin
log_dir=logs
# wait_time=3

# create bsim log dir
mkdir -p ${log_dir}

pkill ubuntu.exe
pkill bsim
# run each test
for test_name in ${asm_tests[@]}; do
	echo "-- benchmark test: ${test_name} --"
	# copy vmh file
	mem_file=${vmh_dir}/${test_name}.riscv
	if [ ! -f $mem_file ]; then
		echo "ERROR: $mem_file does not exit, you need to first compile"
		exit
	fi
	cp ${mem_file} bluesim/program

	# run test
	make run.bluesim > ${log_dir}/${test_name}.log # run bsim, redirect outputs to log
	# sleep ${wait_time} # wait for bsim to setup
done
pkill ubuntu.exe
pkill bsim
