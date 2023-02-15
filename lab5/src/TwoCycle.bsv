// TwoCycle.bsv
//
// This is a two cycle implementation of the RISC-V processor.

import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import MemInit::*;
import RFile::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Vector::*;
import Fifo::*;
import Ehr::*;
import GetPut::*;

(* synthesize *)
module mkProc(Proc);
    Reg#(Addr) pc <- mkRegU;
    RFile rf <- mkRFile;
    DMemory mem <- mkDMemory;
	let dummyInit <- mkDummyMemInit;
    CsrFile csrf <- mkCsrFile;
    Reg#(Maybe#(DecodedInst)) f2e <- mkReg(tagged Invalid);
    Bool memReady = mem.init.done && dummyInit.done;

    // In the instruction fetch stage, the processor reads the current instruction from the 
    // memory and decodes it.
    rule fetch(csrf.started && !isValid(f2e));
        Data inst <- mem.req(MemReq{op: Ld, addr: pc, data: ?});
        
        $display("pc: %h inst: (%h) expanded: ", pc, inst, showInst(inst));
        $fflush(stdout);
        f2e <= tagged Valid decode(inst);
    endrule
    
    // In the execute stage, the processor Reads the register file, executes instructions, 
    // does ALU operations, does memory operations, and writes the result to the register file.
    rule exec(csrf.started && isValid(f2e));
        let dInst = fromMaybe(?, f2e);

        Data rVal1 = rf.rd1(fromMaybe(?, dInst.src1));
        Data rVal2 = rf.rd2(fromMaybe(?, dInst.src2));
        Data csrVal = csrf.rd(fromMaybe(?, dInst.csr));

        ExecInst eInst = exec(dInst, rVal1, rVal2, pc, ?, csrVal);

        if (eInst.iType == Ld) begin
            eInst.data <- mem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
        end else if (eInst.iType == St) begin
            let d <- mem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
        end

        if(eInst.iType == Unsupported) begin
            $fwrite(stderr, "ERROR: Executing unsupported instruction at pc: %x. Exiting\n", pc);
            $finish;
        end

        if (isValid(eInst.dst)) begin
            rf.wr(fromMaybe(?, eInst.dst), eInst.data);
        end

        pc <= eInst.brTaken? eInst.addr : pc+4;

        csrf.wr(eInst.iType == Csrw? eInst.csr : Invalid, eInst.data);

        f2e <= tagged Invalid;
    endrule

    method ActionValue#(CpuToHostData) cpuToHost;
        let ret <- csrf.cpuToHost;
        return ret;
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if (!csrf.started && memReady);
        csrf.start(0);
        $display("Start at pc %h\n", startpc);
	    $fflush(stdout);
        pc <= startpc;
    endmethod

    interface iMemInit = dummyInit;
    interface dMemInit = mem.init;
endmodule
