// Six stage

import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import MemInit::*;
import RFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Fifo::*;
import Ehr::*;
import Btb::*;
import Scoreboard::*;
import FPGAMemory::*;
import DelayedMemory::*;
import Bht::*;

// Data structure for Fetch to Execute stage
typedef struct {
    Addr pc;
    Addr predPc;
    Bool decEpoch;
    Bool exeEpoch;
    Bool regEpoch;
} Fetch2Decode deriving (Bits, Eq);

typedef struct {
    Addr pc;
    Addr predPc;
    DecodedInst dInst;
    Bool exeEpoch;
    Bool regEpoch;
} Decode2Register deriving (Bits, Eq);

typedef struct {
    Addr pc;
    Addr predPc;
    DecodedInst dInst;
    Data rVal1;
    Data rVal2;
    Data csrVal;
    Bool exeEpoch;
} Register2Execute deriving (Bits, Eq);

typedef struct {
    Addr pc;
    Maybe#(ExecInst) eInst;
} Execute2Memory deriving (Bits, Eq);

typedef struct {
    Addr pc;
    Maybe#(ExecInst) eInst;
} Memory2WriteBack deriving (Bits, Eq);

// redirect msg from Execute stage
typedef struct {
	Addr pc;
	Addr nextPc;
    Bool isBranch;
    Bool isTaken;
} ExeRedirect deriving (Bits, Eq);

typedef struct {
	Addr nextPc;
} DecRedirect deriving (Bits, Eq);


typedef struct {
	Addr nextPc;
} RegRedirect deriving (Bits, Eq);

typedef struct {
	Addr pc;
	Addr nextPc;
    Bool isBranch;
    Bool isTaken;
} BxtUpdate deriving (Bits, Eq);


(* synthesize *)
module mkProc(Proc);
    Ehr#(2, Addr) pcReg <- mkEhr(?);
    RFile            rf <- mkBypassRFile;
	Scoreboard#(6)   sb <- mkPipelineScoreboard;
	FPGAMemory     iMem <- mkFPGAMemory;
    FPGAMemory     dMem <- mkFPGAMemory;
    CsrFile        csrf <- mkCsrFile;
    Btb#(6)         btb <- mkBtb; // 64-entry BTB
    BHT#(8)         bht <- mkBHT;

	// global epoch for redirection from Execute stage
	Reg#(Bool) exeEpoch <- mkReg(False);
    Reg#(Bool) decEpoch <- mkReg(False);
    Reg#(Bool) regEpoch <- mkReg(False);

	// EHR for redirection
	Ehr#(2, Maybe#(ExeRedirect)) exeRedirect <- mkEhr(Invalid);
	Ehr#(2, Maybe#(DecRedirect)) decRedirect <- mkEhr(Invalid);
	Ehr#(2, Maybe#(RegRedirect)) regRedirect <- mkEhr(Invalid);

	// Fifo#(2, Fetch2Decode) f2dFifo <- mkBypassFifo;
	// Fifo#(2, Decode2Register) d2rFifo <- mkBypassFifo;
	// Fifo#(2, Register2Execute) r2eFifo <- mkBypassFifo;
	// Fifo#(2, Execute2Memory) e2mFifo <- mkBypassFifo;
	// Fifo#(2, Memory2WriteBack) m2wFifo <- mkBypassFifo;

	// Fifo#(2, Fetch2Decode) f2dFifo <- mkCFFifo;
	// Fifo#(2, Decode2Register) d2rFifo <- mkCFFifo;
	// Fifo#(2, Register2Execute) r2eFifo <- mkCFFifo;
	// Fifo#(2, Execute2Memory) e2mFifo <- mkCFFifo;
	// Fifo#(2, Memory2WriteBack) m2wFifo <- mkCFFifo;

	// Fifo#(2, Fetch2Decode) f2dFifo <- mkPipelineFifo;
	// Fifo#(2, Decode2Register) d2rFifo <- mkPipelineFifo;
	// Fifo#(2, Register2Execute) r2eFifo <- mkPipelineFifo;
	// Fifo#(2, Execute2Memory) e2mFifo <- mkPipelineFifo;
	// Fifo#(2, Memory2WriteBack) m2wFifo <- mkPipelineFifo;

    Fifo#(1, Fetch2Decode) f2dFifo <- mkPipelineFifo;
	Fifo#(1, Decode2Register) d2rFifo <- mkPipelineFifo;
	Fifo#(1, Register2Execute) r2eFifo <- mkPipelineFifo;
	Fifo#(1, Execute2Memory) e2mFifo <- mkPipelineFifo;
	Fifo#(1, Memory2WriteBack) m2wFifo <- mkPipelineFifo;

    Bool memReady = iMem.init.done && dMem.init.done;
    
    function Addr getTargetPc(Data val, Maybe#(Data) imm) = {truncateLSB(val + fromMaybe(?, imm)), 1'b0};

    Ehr#(2, Maybe#(BxtUpdate)) bxtUpdate <- mkEhr(Invalid);

    // Instruction Fetch -- request instruction from iMem and update PC
	// fetch, decode, reg read stage
	rule doFetch(csrf.started);
		// fetch
		iMem.req(MemReq{op: Ld, addr: pcReg[0], data: ?});
		Addr predPc = btb.predPc(pcReg[0]);
        pcReg[0] <= predPc;
        
        let f2d = Fetch2Decode {
            pc: pcReg[0],
            predPc: predPc,
            decEpoch: decEpoch,
            exeEpoch: exeEpoch,
            regEpoch: regEpoch
        };
        f2dFifo.enq(f2d);
        $display("doFetch: PC = %x", f2d.pc);
    endrule
    
    // Decode -- receive response from iMem and decode instruction
    rule doDecode(csrf.started);
        let f2d = f2dFifo.first;
        f2dFifo.deq;
        
        let inst <- iMem.resp;

        if ( f2d.decEpoch != decEpoch || f2d.regEpoch != regEpoch || f2d.exeEpoch != exeEpoch) begin
            $display("doDecode and killed inst with wrong epoch: PC = %x, inst = %x, expanded = ", f2d.pc, inst, showInst(inst));
        end else begin
            // decode
            DecodedInst dInst = decode(inst);

            let ppc = dInst.iType == Br? bht.predPc(f2d.pc, f2d.pc + fromMaybe(?, dInst.imm)) : f2d.predPc;
            
            if ( f2d.predPc != ppc ) begin
                decRedirect[0] <= Valid(DecRedirect{nextPc: ppc});
                $display("doDecode and PC redirect by BHT: PC = %x, PPC = %x, inst = %x, expanded = ", f2d.pc, ppc, inst, showInst(inst));
            end
            
            let d2r = Decode2Register{
                pc: f2d.pc,
                predPc: ppc,
                dInst: dInst,
                exeEpoch: f2d.exeEpoch,
                // regEpoch: regEpoch // bug
                regEpoch: f2d.regEpoch
            };

            d2rFifo.enq(d2r);
            $display("doDecode: PC = %x, inst = %x, expanded = ", f2d.pc, inst, showInst(inst));
        end
    endrule

    // Register Fetch -- read from the register file
    rule doRegisterFetch(csrf.started);
        let d2r = d2rFifo.first;
        let dInst = d2r.dInst;

		// reg read
		Data rVal1 = rf.rd1(fromMaybe(?, dInst.src1));
		Data rVal2 = rf.rd2(fromMaybe(?, dInst.src2));
		Data csrVal = csrf.rd(fromMaybe(?, dInst.csr));

        if ( d2r.regEpoch != regEpoch || d2r.exeEpoch != exeEpoch ) begin
            d2rFifo.deq;
			$display("Register Fetch and killed inst with wrong epoch: PC = %x", d2r.pc);
        end else begin
            let ppc = (d2r.dInst.iType == Jr)? bht.predPc(d2r.pc, getTargetPc(rVal1, dInst.imm)) : d2r.predPc;

            let r2e = Register2Execute {
                pc: d2r.pc,
                predPc: ppc,
                dInst: d2r.dInst,
                rVal1: rVal1,
                rVal2: rVal2,
                csrVal: csrVal,
                exeEpoch: d2r.exeEpoch
            };
            
            // search scoreboard to determine stall
            if(!sb.search1(dInst.src1) && !sb.search2(dInst.src2)) begin
                if ( ppc != d2r.predPc ) begin
                    regRedirect[0] <= Valid(RegRedirect{nextPc: ppc});
                    $display("RegisterFetch and PC redirect by BHT: PC = %x, PPC = %x", d2r.pc, ppc);
                end
                // enq & update PC, sb
                r2eFifo.enq(r2e);
                sb.insert(dInst.dst);
                d2rFifo.deq;
                $display("Register Fetch: PC = %x", d2r.pc);
            end else begin
                $display("Register Fetch Stalled: PC = %x", d2r.pc);
            end
        end
	endrule

    // Execute -- execute the instruction and redirect the processor if necessary    
	rule doExecute(csrf.started);
		let r2e = r2eFifo.first;
		r2eFifo.deq;

        Maybe#(ExecInst) newEInst = Invalid;

		if(r2e.exeEpoch != exeEpoch) begin
			$display("Execute: Kill instruction at pc: %x.\n", r2e.pc);
		end else begin
			// execute
			let eInst = exec(r2e.dInst, r2e.rVal1, r2e.rVal2, r2e.pc, r2e.predPc, r2e.csrVal);
            // check unsupported instruction at commit time. Exiting
            if (eInst.iType == Unsupported) begin
                $fwrite(stderr, "ERROR: Executing unsupported instruction at pc: %x. Exiting\n", r2e.pc);
                $finish;
            end

            newEInst = Valid(eInst);

            let jump = eInst.iType == J || eInst.iType == Jr || eInst.iType == Br;
            let npc = jump? eInst.addr : r2e.pc+4;
            let isBranch = eInst.iType == Br;

            bxtUpdate[0] <= Valid(BxtUpdate{pc: r2e.pc, nextPc: npc, isBranch: isBranch, isTaken: eInst.brTaken});
            
            if (eInst.mispredict) begin
                exeRedirect[0] <= Valid(ExeRedirect{pc: r2e.pc, nextPc: npc, isBranch: isBranch, isTaken: eInst.brTaken});
                $display("Execute finds misprediction: PC = %x, misPC = %x, nextPC = %x", r2e.pc, r2e.predPc, npc);
            end else begin
                $display("Execute: PC = %x", r2e.pc);
            end
        end

        let e2m = Execute2Memory{
                pc: r2e.pc,
                eInst: newEInst
            };
        e2mFifo.enq(e2m);

    endrule

    // Memory -- send memory request to dMem
    rule doMemory(csrf.started);
        let e2m = e2mFifo.first;
        e2mFifo.deq;    

        if ( isValid(e2m.eInst) ) begin
            let eInst = fromMaybe(?, e2m.eInst);
            // memory
            if(eInst.iType == Ld) begin
                dMem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
            end else if(eInst.iType == St) begin
                dMem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
            end
            $display("doMemory : PC = %x", e2m.pc);
        end else begin
            $display("doMemory : PC = %x, poisoned instruction", e2m.pc);
        end

        let m2w = Memory2WriteBack{
            pc: e2m.pc,
            eInst: e2m.eInst
        };
        m2wFifo.enq(m2w);
    endrule

    // Write Back -- receive memory response from dMem (if applicable) and write to register file
    rule doWriteBack(csrf.started);
        let m2w = m2wFifo.first;
        m2wFifo.deq;
        
        if (isValid(m2w.eInst)) begin
            let eInst = fromMaybe(?, m2w.eInst);
            if(eInst.iType == Ld) begin
                eInst.data <- dMem.resp;
            end
            if(isValid(eInst.dst)) begin
                rf.wr(fromMaybe(?, eInst.dst), eInst.data);
            end
            csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);
            $display("doWriteBack : PC = %x", m2w.pc);
        end else begin
            $display("doWriteBack : PC = %x, poisoned instruction", m2w.pc);
        end
        // remove from scoreboard
        sb.remove;
	endrule

    (* fire_when_enabled *)
    (* no_implicit_conditions *)
    rule canonicalizeRedirect(csrf.started);
        
        if ( bxtUpdate[1] matches tagged Valid .r ) begin
            btb.update(r.pc, r.nextPc);
            if ( r.isBranch ) bht.update(r.pc, r.isTaken);
        end

        if ( exeRedirect[1] matches tagged Valid .r ) begin
            pcReg[1] <= r.nextPc;
            exeEpoch <= !exeEpoch;
        end else if ( regRedirect[1] matches tagged Valid .r ) begin
            pcReg[1] <= r.nextPc;
            regEpoch <= !regEpoch;
        end else if ( decRedirect[1] matches tagged Valid .r ) begin
            pcReg[1] <= r.nextPc;
            decEpoch <= !decEpoch;
        end 

        exeRedirect[1] <= Invalid;
        decRedirect[1] <= Invalid;
        regRedirect[1] <= Invalid;
        
    endrule

    method ActionValue#(CpuToHostData) cpuToHost if(csrf.started);
        let ret <- csrf.cpuToHost;
        return ret;
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if ( !csrf.started && memReady );
	$display("Start cpu");
        csrf.start(0); // only 1 core, id = 0
        pcReg[0] <= startpc;
    endmethod

	interface iMemInit = iMem.init;
    interface dMemInit = dMem.init;
endmodule

