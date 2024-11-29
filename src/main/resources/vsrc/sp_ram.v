`timescale 1ns/1ns

module sp_ram (
    clkIn,
    rstIn,
    addrIn,
    wrEnIn,
    wrDataIn,
    rdEnIn,
    rdDataOut,
    rdAckOut);

    parameter DATA_WIDTH = 32;
    parameter RAM_DEPTH  = 512;

    localparam WREN_WIDTH = (DATA_WIDTH+7)/8;
    localparam RAM_WIDTH  = WREN_WIDTH*8;
    localparam ADDR_WIDTH = $clog2(RAM_DEPTH);
    localparam PAD_WIDTH  = RAM_WIDTH - DATA_WIDTH;

    input clkIn;
    input rstIn;
    input [ADDR_WIDTH-1:0] addrIn;
    input [WREN_WIDTH-1:0] wrEnIn;
    input [DATA_WIDTH-1:0] wrDataIn;
    input rdEnIn;
    
    output [DATA_WIDTH-1:0] rdDataOut;
    output rdAckOut;
    
    
    reg [RAM_WIDTH-1:0] ram [0:(RAM_DEPTH-1)];

    reg rdAckR;
    reg [RAM_WIDTH-1:0] rdDataR;

    wire [RAM_WIDTH-1:0] wrDataPad;
    
    assign wrDataPad = {{PAD_WIDTH{1'b0}}, wrDataIn};
    
    always @(posedge clkIn) begin
        rdDataR <= ram[addrIn];
    end
        
    generate
    genvar i;
        for (i = 0; i < WREN_WIDTH; i = i + 1) begin
            always @(posedge clkIn) begin
                if (wrEnIn[i]) begin
                    ram[addrIn][(8*i)+:8] = wrDataPad[(8*i)+:8];
                end
            end
        end
    endgenerate
    
    
        
    always @(posedge clkIn) begin
        if (rstIn) begin
            rdAckR  <= 0;
        end else begin
            rdAckR  <= rdEnIn;
        end
    end

    assign rdDataOut = rdDataR[DATA_WIDTH-1:0];
    assign rdAckOut  = rdAckR;

endmodule

