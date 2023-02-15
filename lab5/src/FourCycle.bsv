// FourCycle.bsv
//
// This is a four cycle implementation of the RISC-V processor.

import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import MemInit::*;
import RFile::*;
import DelayedMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Vector::*;
import FIFO::*;
import Ehr::*;
import GetPut::*;

typedef enum {
    Fetch,
    Decode,
    Execute,
    WriteBack
} Stage deriving(Bits, Eq, FShow);
    
(* synthesize *)
module mkProc(Proc);
    Reg#(Addr) pc <- mkRegU;
    RFile rf <- mkRFile;
    DelayedMemory mem <- mkDelayedMemory;
    let dummyInit <- mkDummyMemInit;
    CsrFile csrf <- mkCsrFile;
    FIFO#(DecodedInst) d2e <- mkFIFO;
    FIFO#(ExecInst) e2w <- mkFIFO;
    Reg#(Stage) stage <- mkReg(Fetch);
    Bool memReady = mem.init.done && dummyInit.done;
        
    // The instruction fetch stage, as before, sets the address lines on the memory to PC to read the current instruction.
    rule fetch(csrf.started && stage == Fetch);
        mem.req(MemReq{op: Ld, addr: pc, data: ?});
        stage <= Decode;
    endrule
    
    // The instruction decode stage gets the instruction from memory, decodes it, and reads registers.
    rule decode(csrf.started && stage == Decode);
        let inst <- mem.resp;
        $display("pc: %h inst: (%h) expanded: ", pc, inst, showInst(inst));
        $fflush(stdout);
        d2e.enq(decode(inst));
        stage <= Execute;
    endrule

    
    // The execute stage performs ALU operations, writes data to the memory for store instructions, and sets memory address lines for read instructions.
    rule exec(csrf.started && stage == Execute);
        let dInst = d2e.first;
        d2e.deq;

        Data rVal1 = rf.rd1(fromMaybe(?, dInst.src1));
        Data rVal2 = rf.rd2(fromMaybe(?, dInst.src2));
        Data csrVal = csrf.rd(fromMaybe(?, dInst.csr));

        ExecInst eInst = exec(dInst, rVal1, rVal2, pc, ?, csrVal);

        if ( eInst.iType == Ld ) begin
            mem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
        end else if (eInst.iType == St) begin
            mem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
        end

        if ( eInst.iType == Unsupported ) begin
            $fwrite(stderr, "ERROR: Executing unsupported instruction at pc: %x. Exiting\n", pc);
            $finish;
        end

        pc <= eInst.brTaken? eInst.addr : pc+4;

        e2w.enq(eInst);

        stage <= WriteBack;
    endrule

    // The write back stage obtains the result from the ALU or reads the result from memory (if any) and writes the register file.
    rule writeback(csrf.started && stage == WriteBack);
        let eInst = e2w.first;
        e2w.deq;

        if ( eInst.iType == Ld ) begin
            eInst.data <- mem.resp;
        end

        if (isValid(eInst.dst)) begin
            rf.wr(fromMaybe(?, eInst.dst), eInst.data);
        end

        csrf.wr(eInst.iType == Csrw? eInst.csr : Invalid, eInst.data);
        
        stage <= Fetch;
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
