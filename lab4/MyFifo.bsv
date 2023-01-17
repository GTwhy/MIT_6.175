import Ehr::*;
import Vector::*;

//////////////////
// Fifo interface 

interface Fifo#(numeric type n, type t);
    method Bool notFull;
    method Action enq(t x);
    method Bool notEmpty;
    method Action deq;
    method t first;
    method Action clear;
endinterface

/////////////////
// Conflict FIFO

module mkMyConflictFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))     data     <- replicateM(mkRegU());
    Reg#(Bit#(TLog#(n)))    enqP     <- mkReg(0);
    Reg#(Bit#(TLog#(n)))    deqP     <- mkReg(0);
    Reg#(Bool)              empty    <- mkReg(True);
    Reg#(Bool)              full     <- mkReg(False);

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    method Bool notFull();
        return !full;
    endmethod

    method Action enq (t x) if (!full);
        empty <= False;
        data[enqP] <= x;
        let nextEnqP = enqP + 1;
        if (nextEnqP > max_index) begin nextEnqP = 0; end
        if (nextEnqP == deqP) begin full <= True; end
        enqP <= nextEnqP;
    endmethod

    method Bool notEmpty();
        return !empty;
    endmethod

    method Action deq() if (!empty);
        full <= False;
        let nextDeqP = deqP + 1;
        if (nextDeqP > max_index) begin nextDeqP = 0; end
        if (nextDeqP == enqP) begin empty <= True; end
        deqP <= nextDeqP;
    endmethod

    method t first() if (!empty);
        return data[deqP];
    endmethod

    method Action clear();
        deqP <= 0;
        enqP <= 0;
        empty <= True;
        full <= False;
    endmethod

endmodule

/////////////////
// Pipeline FIFO

// Pipeline FIFO
// Intended schedule:
//      {notEmpty, first, deq} < {notFull, enq} < clear
module mkMyPipelineFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))     data     <- replicateM(mkRegU());
    Ehr#(3, Bit#(TLog#(n))) enqP     <- mkEhr(0);
    Ehr#(3, Bit#(TLog#(n))) deqP     <- mkEhr(0);
    Ehr#(3, Bool)           empty    <- mkEhr(True);
    Ehr#(3, Bool)           full     <- mkEhr(False);

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    method Bool notFull();
        return !full[1];
    endmethod

    method Action enq (t x) if (!full[1]);
        empty[1] <= False;
        data[enqP[1]] <= x;
        let nextEnqP = enqP[1] + 1;
        if (nextEnqP > max_index) begin nextEnqP = 0; end
        if (nextEnqP == deqP[1]) begin full[1] <= True; end
        enqP[1] <= nextEnqP;
    endmethod

    method Bool notEmpty();
        return !empty[0];
    endmethod

    method Action deq() if (!empty[0]);
        full[0] <= False;
        let nextDeqP = deqP[0] + 1;
        if (nextDeqP > max_index) begin nextDeqP = 0; end
        if (nextDeqP == enqP[0]) begin empty[0] <= True; end
        deqP[0] <= nextDeqP;
    endmethod

    method t first() if (!empty[0]);
        return data[deqP[0]];
    endmethod

    method Action clear();
        deqP[2] <= 0;
        enqP[2] <= 0;
        empty[2] <= True;
        full[2] <= False;
    endmethod

endmodule

//////////////
// Bypass FIFO

// Intended schedule:
//      {notFull, enq} < {notEmpty, first, deq} < clear
module mkMyBypassFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Ehr#(2, t))     data     <- replicateM(mkEhrU());
    Ehr#(3, Bit#(TLog#(n))) enqP     <- mkEhr(0);
    Ehr#(3, Bit#(TLog#(n))) deqP     <- mkEhr(0);
    Ehr#(3, Bool)           empty    <- mkEhr(True);
    Ehr#(3, Bool)           full     <- mkEhr(False);

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    method Bool notFull();
        return !full[0];
    endmethod

    method Action enq (t x) if (!full[0]);
        empty[0] <= False;
        data[enqP[0]][0] <= x;
        let nextEnqP = enqP[0] + 1;
        if (nextEnqP > max_index) begin nextEnqP = 0; end
        if (nextEnqP == deqP[0]) begin full[0] <= True; end
        enqP[0] <= nextEnqP;
    endmethod

    method Bool notEmpty();
        return !empty[1];
    endmethod

    method Action deq() if (!empty[1]);
        full[1] <= False;
        let nextDeqP = deqP[1] + 1;
        if (nextDeqP > max_index) begin nextDeqP = 0; end
        if (nextDeqP == enqP[1]) begin empty[1] <= True; end
        deqP[1] <= nextDeqP;
    endmethod

    method t first() if (!empty[1]);
        return data[deqP[1]][1];
    endmethod

    method Action clear();
        deqP[2] <= 0;
        enqP[2] <= 0;
        empty[2] <= True;
        full[2] <= False;
    endmethod
endmodule

//////////////////////
// Conflict-free fifo

// Intended schedule:
//      {notFull, enq} CF {notEmpty, first, deq}
//      {notFull, enq, notEmpty, first, deq} < clear
module mkMyCFFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))     data     <- replicateM(mkRegU());
    //To enforce {enq, deq} < clear, I replaced the Regs with Ehrs
    Ehr#(2, Bit#(TLog#(n))) enqP     <- mkEhr(0);
    Ehr#(2, Bit#(TLog#(n))) deqP     <- mkEhr(0);
    Ehr#(2, Bool)           empty    <- mkEhr(True);
    Ehr#(2, Bool)           full     <- mkEhr(False);

    Ehr#(2, Bool)        req_deq     <- mkEhr(False);
    Ehr#(2, Bool)       req_clear    <- mkEhr(False);
    Ehr#(2, Maybe#(t))   req_enq     <- mkEhr(tagged Invalid); 

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    (*no_implicit_conditions, fire_when_enabled*)
    rule canonicalizie;
        $display("%d, %d, %d, %d : %d, %d, %d, %d" ,enqP[0], deqP[0], full[0], empty[0], enqP[1], deqP[1], full[1], empty[1]);
        let nextEnqP = enqP[0] + 1;
        if (nextEnqP > max_index) begin nextEnqP = 0; end

        let nextDeqP = deqP[0] + 1;
        if (nextDeqP > max_index) begin nextDeqP = 0; end

        // enq and deq
        if ((!full[0] && isValid(req_enq[1])) && (!empty[0] && req_deq[1])) begin
            empty[0] <= False;
            full[0] <= False;
            data[enqP[0]] <= fromMaybe(?, req_enq[1]);
            enqP[0] <= nextEnqP;
            deqP[0] <= nextDeqP;
        // deq only
        end else if (!empty[0] && req_deq[1]) begin
            if (nextDeqP == enqP[0]) begin
                empty[0] <= True;
            end
            full[0] <= False;
            deqP[0] <= nextDeqP;
        // enq only
        end else if (!full[0] && isValid(req_enq[1])) begin
            if (nextEnqP == deqP[0]) begin
                full[0] <= True;
            end
            empty[0] <= False;
            data[enqP[0]] <= fromMaybe(?, req_enq[1]);
            enqP[0] <= nextEnqP;
        end

        if (req_clear[1]) begin
            enqP[1] <= 0;
            deqP[1] <= 0;
            empty[1] <= True;
            full[1] <= False;
        end

        req_enq[1]   <= tagged Invalid;
        req_deq[1]   <= False;
        req_clear[1] <= False;
    endrule

    method Bool notFull();
        return !full[0];
    endmethod

    method Action enq (t x) if (!full[0]);
        req_enq[0] <= tagged Valid (x);
    endmethod

    method Bool notEmpty();
        return !empty[0];
    endmethod

    method Action deq() if (!empty[0]);
        req_deq[0] <= True;

    endmethod

    method t first() if (!empty[0]);
        return data[deqP[0]];
    endmethod

    method Action clear();
        req_clear[0] <= True;
    endmethod

endmodule

