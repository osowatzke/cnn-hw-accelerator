`timescale 1ns/1ns

module dp_ram_tb;

    parameter CLK_PERIOD   = 10;
    parameter RESET_TIME   = 100;
    
    parameter RAM_DEPTH    = 512;
    parameter DATA_WIDTH   = 32;
    parameter MAX_STALL    = 7;
    
    localparam ADDR_WIDTH  = $clog2(RAM_DEPTH);
    localparam STALL_WIDTH = $clog2(MAX_STALL+1);
    localparam WREN_WIDTH  = (DATA_WIDTH+7)/8;
    
    localparam WRITE = 0;
    localparam STALL = 1;
    localparam READ  = 2;
    localparam CHECK = 3;
    
    reg [1:0] stateR;
    
    reg [ DATA_WIDTH-1:0] dataR;
    reg [ ADDR_WIDTH-1:0] addrR;
    reg [ WREN_WIDTH-1:0] wrEnR;
    reg [STALL_WIDTH-1:0] stallR;
    reg [STALL_WIDTH-1:0] stallVar;
    reg rdEnR;
    
    wire [DATA_WIDTH-1:0] rdData;
    wire rdAck;
    wire clk;
    wire rst;
    
    clk_gen #(.CLK_PERIOD(CLK_PERIOD)) clk_gen_i (.clkOut(clk));
    rst_gen #(.RESET_TIME(RESET_TIME)) rst_gen_i (.rstOut(rst));
    
    dp_ram #(.DATA_WIDTH(DATA_WIDTH), .RAM_DEPTH(RAM_DEPTH)) ram (
        .clkIn(clk),
        .rstIn(rst),
        .addrAIn(addrR),
        .wrEnAIn(wrEnR),
        .wrDataAIn(dataR),
        .rdEnAIn(0),
        .addrBIn(addrR),
        .wrEnBIn(0),
        .wrDataBIn(dataR),
        .rdEnBIn(rdEnR),
        .rdDataBOut(rdData),
        .rdAckBOut(rdAck));
    
    always @(posedge clk) begin
        if (rst) begin
            stateR  <= WRITE;
            rdEnR   <= 0;
            wrEnR   <= 0;
            stallR  <= 0;
            addrR   <= 0;
            dataR   <= 0;
        end else begin
            rdEnR   <= 0;
            wrEnR   <= 0;
            case (stateR)
                WRITE : begin
                    wrEnR       <= {WREN_WIDTH{1'b1}};
                    addrR       <= $random;
                    dataR       <= $random;
                    stallVar     = $urandom_range(0, MAX_STALL);
                    stallR      <= stallVar;
                    if (stallVar == 0) begin
                        stateR  <= READ;
                    end else begin
                        stateR  <= STALL;
                    end
                end
                STALL : begin
                    stallR      <= stallR - 1;
                    if (stallR == 1) begin
                        stateR  <= READ;
                    end
                end
                READ : begin
                    rdEnR       <= 1;
                    stateR      <= CHECK;
                end
                CHECK : begin
                    if (rdAck == 1) begin
                        if (rdData !== dataR) begin
                            $error("Error Detected at Time %t: Received %08X, Expected %08X", $realtime, rdData, dataR);
                        end
                        stateR  <= WRITE;
                    end
                end
            endcase   
        end
    end
    
endmodule