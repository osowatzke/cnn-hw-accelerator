module counter (
    clkIn,
    rstIn,
    clrIn,
    advIn,
    endValIn,
    cntOut,
    doneOut);
    
    parameter CNT_WIDTH = 8;
    
    input clkIn;
    input rstIn;
    
    input clrIn;
    input advIn;
    input [CNT_WIDTH-1:0] endValIn;
    
    output doneOut;
    output [CNT_WIDTH-1:0] cntOut;
    
    reg doneR;
    reg [CNT_WIDTH-1:0] nextCntVar;
    reg [CNT_WIDTH-1:0] cntR;
        
    always @(posedge clkIn) begin
        if (rstIn) begin
            doneR       <= 0;
            cntR        <= 0;
        end else begin
            if (clrIn) begin
                nextCntVar  = 0;
            end else if (advIn && !doneR) begin
                nextCntVar  = cntR + 1;
            end
            cntR        <= nextCntVar;
            if (nextCntVar == endValIn) begin
                doneR   <= 1;
            end else begin
                doneR   <= 0;
            end
        end
    end
    
    assign doneOut = doneR;
    assign cntOut  = cntR;
    
endmodule
