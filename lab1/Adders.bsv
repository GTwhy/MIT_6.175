import Multiplexer::*;

function Bit#(1) fa_sum(Bit#(1) a, Bit#(1) b, Bit#(1) c);
    return a^b^c;
endfunction

function Bit#(1) fa_carry(Bit#(1) a, Bit#(1) b, Bit#(1) c);
    return (a&b)|(a&c)|(b&c);
endfunction

// Error: "Adders.bsv", line 13, column 22: (G0028)
//   `c[1]' uses uninitialized value (the position shown is the object's
//   declaration). If this error is unexpected, please consult KPNS #32.
//   During elaboration of the body of rule `test' at "TestBench.bsv", line 94,

// function Bit#(TAdd#(n,1)) addN(Bit#(n) a, Bit#(n) b, Bit#(1) c0);
//     Bit#(n) s;
//     Bit#(TAdd#(n,1)) c=0;
//     c[0] = c0;
//     for(Integer i = 0; i < valueOf(n); i=i+1) begin
//         s[i] = fa_sum(a[i], b[i], c[i]);
//         c[i+1] = fa_carry(a[i], b[i], c[i]);
//     end
//     return {c[valueOf(n)],s};
// endfunction

function Bit#(TAdd#(n,1)) addN(Bit#(n) a, Bit#(n) b, Bit#(1) c0);
    Bit#(n) s;
    Bit#(1) c=c0;
    for(Integer i = 0; i < valueOf(n); i=i+1) begin
        s[i] = fa_sum(a[i], b[i], c);
        c = fa_carry(a[i], b[i], c);
    end
    return {c,s};
endfunction

function Bit#(5) add4(Bit#(4) a, Bit#(4) b, Bit#(1) c0);
    return addN(a,b,c0);
endfunction

interface Adder8;
    method ActionValue#(Bit#(9)) sum(Bit#(8) a,Bit#(8) b, Bit#(1) c_in);
endinterface

module mkRCAdder(Adder8);
    method ActionValue#(Bit#(9)) sum(Bit#(8) a,Bit#(8) b,Bit#(1) c_in);
        let low = add4(a[3:0], b[3:0], c_in);
        let high = add4(a[7:4], b[7:4], low[4]);
        return {high, low[3:0]};
    endmethod
endmodule

module mkCSAdder(Adder8);
    method ActionValue#(Bit#(9)) sum(Bit#(8) a,Bit#(8) b,Bit#(1) c_in);
        let low = add4(a[3:0],b[3:0],c_in);
        let high0 = add4(a[7:4],b[7:4],1'b0);
        let high1 = add4(a[7:4],b[7:4],1'b1);
        let high = multiplexer5(low[4], high0, high1);
        return {high, low[3:0]};
    endmethod
endmodule