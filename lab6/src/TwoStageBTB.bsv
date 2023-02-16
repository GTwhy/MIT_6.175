// TwoStageBTB.bsv
//
// This is a two stage pipelined (with BTB) implementation of the RISC-V processor.

import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import MemInit::*;
import RFile::*;
import DMemory::*;
import IMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Vector::*;
import FIFO::*;
import Ehr::*;
import GetPut::*;
import Btb::*;


typedef struct {
    DecodedInst dInst;
    Addr pc;
    Addr predPc;
} F2EMsg deriving(Bits, Eq);

(* synthesize *)
module mkProc(Proc);
    Ehr#(2, Addr) pc <- mkEhrU;
    RFile rf <- mkRFile;
    IMemory iMem <- mkIMemory;
    DMemory dMem <- mkDMemory;
    CsrFile csrf <- mkCsrFile;
    FIFO#(F2EMsg) f2e <- mkFIFO;
    Btb#(8) btb <- mkBtb;
    Bool memReady = dMem.init.done && iMem.init.done;

    // In the instruction fetch stage, the processor reads the current instruction from the 
    // memory and decodes it.
    rule fetch(csrf.started);
        Data inst = iMem.req(pc[0]);
        let ppc = btb.predPc(pc[0]);
        pc[0] <= ppc;
        $display("pc: %h inst: (%h) expanded: ", pc[0], inst, showInst(inst));
        $fflush(stdout);
        f2e.enq(F2EMsg{dInst: decode(inst), pc: pc[0],predPc: ppc});
    endrule
    
    // In the execute stage, the processor Reads the register file, executes instructions, 
    // does ALU operations, does memory operations, and writes the result to the register file.
    rule exec(csrf.started);
        let msg = f2e.first;
        let dInst = msg.dInst;

        Data rVal1 = rf.rd1(fromMaybe(?, dInst.src1));
        Data rVal2 = rf.rd2(fromMaybe(?, dInst.src2));
        Data csrVal = csrf.rd(fromMaybe(?, dInst.csr));

        ExecInst eInst = exec(dInst, rVal1, rVal2, msg.pc, msg.predPc, csrVal);

        if (eInst.iType == Ld) begin
            eInst.data <- dMem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
        end else if (eInst.iType == St) begin
            let d <- dMem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
        end

        if(eInst.iType == Unsupported) begin
            $fwrite(stderr, "ERROR: Executing unsupported instruction at pc: %x. Exiting\n", msg.pc);
            $finish;
        end

        if (isValid(eInst.dst)) begin
            rf.wr(fromMaybe(?, eInst.dst), eInst.data);
        end

        csrf.wr(eInst.iType == Csrw? eInst.csr : Invalid, eInst.data);

        if ( eInst.mispredict ) begin
            pc[1] <= eInst.addr;
            f2e.clear;
            btb.update(msg.pc, eInst.addr);
        end else begin
            f2e.deq;
        end
    endrule

    method ActionValue#(CpuToHostData) cpuToHost;
        let ret <- csrf.cpuToHost;
        return ret;
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if (!csrf.started && memReady);
        csrf.start(0);
        $display("Start at pc %h\n", startpc);
	    $fflush(stdout);
        pc[0] <= startpc;
    endmethod

    interface iMemInit = iMem.init;
    interface dMemInit = dMem.init;
endmodule
