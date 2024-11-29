module multiply_and_accumulate_tb;

    parameter CLK_PERIOD  = 10;
    parameter RESET_TIME  = 100;
    
    parameter VECTOR_SIZE = 8;
    parameter DATA_WIDTH  = 32;
    
    wire clk;
    wire rst;
    
    wire [VECTOR_SIZE*DATA_WIDTH-1:0] dataA;
    wire [VECTOR_SIZE*DATA_WIDTH-1:0] dataB;
    wire [VECTOR_SIZE-1:0] valid;
    wire last;
    
    wire [DATA_WIDTH-1:0] result;
    wire resultValid;
    wire error;
    
    // Create clock and reset
    clk_gen #(.CLK_PERIOD(CLK_PERIOD)) clk_gen_i (.clkOut(clk));
    rst_gen #(.RESET_TIME(RESET_TIME)) rst_gen_i (.rstOut(rst));
    
    file_driver #(
        .FILE_NAME("input_a.txt"),
        .VECTOR_SIZE(VECTOR_SIZE),
        .DATA_WIDTH(DATA_WIDTH)) driver_a (
        .clkIn(clk),
        .rstIn(rst),
        .readyIn(1'b1),
        .dataOut(dataA),
        .validOut(valid),
        .lastOut(last));
        
    file_driver #(
        .FILE_NAME("input_b.txt"),
        .VECTOR_SIZE(VECTOR_SIZE),
        .DATA_WIDTH(DATA_WIDTH)) driver_b (
        .clkIn(clk),
        .rstIn(rst),
        .readyIn(1'b1),
        .dataOut(dataB),
        .validOut(valid),
        .lastOut(last));
        
    multiply_and_accumulate mac (
        .clkIn(clk),
        .rstIn(rst),
        .dataAIn(dataA),
        .dataBIn(dataB),
        .validIn(valid),
        .lastIn(last),
        .dataOut(result),
        .validOut(resultValid));
    
    file_checker check (
        .clkIn(clk),
        .rstIn(rst),
        .validIn(resultValid),
        .dataIn(result),
        .errorOut(error));
        
endmodule