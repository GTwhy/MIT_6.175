Use connectal to fill memory
1. cp  375/lab5/connectal
2. fill icache and dcache in main.cc
3. delete codes about copying vmh files in run_asms.sh
4. repalce mkRegFileFullLoad("mem.vmh") with mkRegFileFull
5. change the value of vmh_dir in run_asm.sh to load bins
6. mem.upd(truncate(l.addr), l.data) -> mem.upd(truncate(l.addr>>2), l.data)


problem:
1. asm test begin with unsupport inst(fill cache by request)
    I can see 'Request 00000200 00000093' in log, whcih is same as dump, 
    but get an error: 'pc: 00000200 inst: (00000000) expanded: unsupport 0x0'.
    reason: l.addr should be >> 2 to become an addr for a 32bit word.
2. bmark test end with unsupport inst at 46a(fill cache by vmh or request)
    ERROR: Executing unsupported instruction at pc: 0000046a. Exiting
