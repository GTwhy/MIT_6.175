import Types::*;
import ProcTypes::*;
import RegFile::*;
import Vector::*;

interface DirectionPred#(numeric type bhtIndex);
    method Addr ppcDP(Addr pc, Addr targetPC);
    method Action update(Addr pc, Bool taken);
endinterface
