module delay (
    clkIn,
    rstIn,
    dataIn,
    dataOut);
    
    parameter LATENCY = 8;
    parameter DATA_WIDTH = 32;
    
    input clkIn;
    input rstIn;
    
    input  [DATA_WIDTH-1:0] dataIn;
    output [DATA_WIDTH-1:0] dataOut;
    
    reg [DATA_WIDTH-1:0] dataR [0:LATENCY-1];
    
    integer i;
    
    always @(posedge clkIn) begin
        for (i = 0; i < LATENCY; i = i + 1) begin
            if (i == 0) begin
                dataR[i] <= dataIn;
            end else begin
                dataR[i] <= dataR[i-1];
            end
        end
    end
    
    assign dataOut = dataR[LATENCY-1];
        
endmodule
