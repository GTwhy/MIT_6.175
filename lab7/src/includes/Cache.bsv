import CacheTypes::*;
import MemUtil::*;
import Fifo::*;
import Vector::*;
import Types::*;
import MemTypes::*;

module mkTranslator(WideMem wideMem, Cache ifc);

    Fifo#(2, MemReq) reqFifo <- mkCFFifo;

    method Action req(MemReq r);
        if ( r.op == Ld ) reqFifo.enq(r);
        wideMem.req(toWideMemReq(r));
    endmethod

    method ActionValue#(MemResp) resp;
        let req = reqFifo.first;
        reqFifo.deq;

        let cacheLine <- wideMem.resp;
        CacheWordSelect offset = truncate(req.addr >> 2);
        
        return cacheLine[offset];
    endmethod
endmodule
