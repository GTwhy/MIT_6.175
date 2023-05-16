// Return Address Stack

import Types::*;
import ProcTypes::*;
import RegFile::*;
import Vector::*;

interface RAS#(numeric type stackSize);
    method Action push(Addr addr);
    method ActionValue#(Addr) pop;
endinterface

module mkRAS(RAS#(stackSize)) provisos(Add#(1, a__, stackSize));
    Vector#(stackSize, Reg#(Addr)) stack <- replicateM(mkRegU);
    Reg#(Bit#(TLog#(stackSize))) top <- mkReg(0);
    Bit#(TLog#(stackSize)) max_index = fromInteger(valueOf(stackSize)-1);

    method ActionValue#(Addr) pop();
        let index = top == 0? max_index : top-1;
        let addr = stack[index];
        top <= index;
        return addr;
    endmethod

    method Action push(Addr addr);
        stack[top] <= addr;
        top <= top == max_index? 0 : top+1;
    endmethod
endmodule

