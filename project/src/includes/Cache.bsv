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

typedef enum {
    Ready,
    StartMiss, 
    SendFillReq, 
    WaitFillResp 
} ReqStatus deriving ( Bits, Eq );

//direct-mapped, write-miss allocate, writeback
module mkCache(WideMem wideMem, Cache ifc);

    Vector#(CacheRows, Reg#(CacheLine)) dataArray <- replicateM(mkRegU);
    Vector#(CacheRows, Reg#(Maybe#(CacheTag))) tagArray <- replicateM(mkReg(tagged Invalid));
    Vector#(CacheRows, Reg#(Bool)) dirtyArray <- replicateM(mkReg(False));
    
    // Fifo#(1, Data) hitQ <- mkBypassFifo;
    Fifo#(1, Data) hitQ <- mkPipelineFifo;
    Reg#(MemReq) missReq <- mkRegU;
    Reg#(ReqStatus) mshr <- mkReg(Ready);

    // Fifo#(2, MemReq) memReqQ <- mkCFFifo;
    // Fifo#(2, MemResp) memRespQ <- mkCFFifo;

    // log2(16*32/8) = 6
    function CacheIndex getIndex(Addr addr) = truncate(addr >> 6);
    // log2(32/8) = 2
    function CacheWordSelect getOffset(Addr addr) = truncate(addr >> 2);
    function CacheTag getTag(Addr addr) = truncateLSB(addr);

    rule startMiss(mshr == StartMiss);
        let idx = getIndex(missReq.addr);
        let tag = tagArray[idx];
        let dirty = dirtyArray[idx];

        if (isValid(tag) && dirty) begin
            let addr = {fromMaybe(?, tag), idx, 6'b0}; 
            let data = dataArray[idx];
            wideMem.req(WideMemReq {write_en: '1, addr: addr, data: data});
        end

        mshr <= SendFillReq;   
    endrule
    
    rule sendFillReq(mshr == SendFillReq);
        WideMemReq wideMemReq = toWideMemReq(missReq);
        wideMemReq.write_en = 0;
        wideMem.req(wideMemReq);

        mshr <= WaitFillResp;
    endrule
    
    rule waitFillResp(mshr == WaitFillResp);
        let idx = getIndex(missReq.addr);
        let tag = getTag(missReq.addr);
        let wOffset = getOffset(missReq.addr);
        let data <- wideMem.resp;
        tagArray[idx] <= tagged Valid tag;

        if(missReq.op == Ld) begin 
        	dirtyArray[idx] <= False;
        	dataArray[idx] <= data;
        	hitQ.enq(data[wOffset]); 
        end else begin
            dirtyArray[idx] <= True;
        	data[wOffset] = missReq.data; 
        	dataArray[idx] <= data;
        end     
        
        mshr <= Ready;
    endrule
    
    method Action req(MemReq r) if (mshr == Ready);
        let idx = getIndex(r.addr); 
        let tag = getTag(r.addr);
        let wOffset = getOffset(r.addr);
        let currTag = tagArray[idx]; 
        let hit = isValid(currTag) ? fromMaybe(?, currTag) == tag : False;

        if ( hit ) begin
        	let cacheLine = dataArray[idx];
        	if ( r.op == Ld ) hitQ.enq(cacheLine[wOffset]);
        	else begin
        	    cacheLine[wOffset] = r.data;
        	    dataArray[idx] <= cacheLine;
        		dirtyArray[idx] <= True;
        	end
        end else begin
        	missReq <= r;
        	mshr <= StartMiss;
        end
    endmethod
    
    method ActionValue#(Data) resp;
        hitQ.deq;
        return hitQ.first;
    endmethod
    
endmodule