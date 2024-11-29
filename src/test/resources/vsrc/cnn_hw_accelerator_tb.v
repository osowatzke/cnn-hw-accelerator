module cnn_hw_accelerator_tb;

    parameter CLK_PERIOD     = 10;
    parameter RESET_TIME     = 100;
    
    // RISCV bus interface
    parameter BUS_ADDR_WIDTH = 32;
    parameter BUS_DATA_WIDTH = 64;
    
    // Hardware accelerator overrides
    parameter VECTOR_SIZE    = 8;
    parameter DATA_WIDTH     = 32;
    parameter MAX_SIZE       = 4096; // < Max Rows > * < Max Cols >
    
    // Dependent parameters for RISCV bus interface
    localparam BUS_WE_WIDTH  = BUS_DATA_WIDTH/8;
    localparam WE_WIDTH      = DATA_WIDTH/8;
    localparam NUM_WORDS     = BUS_DATA_WIDTH/DATA_WIDTH;
    localparam CNT_WIDTH     = $clog2(NUM_WORDS);
    localparam DIM_WIDTH     = $clog2(MAX_SIZE) + 1;
    localparam DATA_ADDR     = 0;
    localparam FILT_ADDR     = 1 << ($clog2(MAX_SIZE) + $clog2(WE_WIDTH));
    
    // State Enumerations
    localparam IDLE  = 0;
    localparam DCOLS = 1;
    localparam DROWS = 2;
    localparam DLOAD = 3;
    localparam FCOLS = 4;
    localparam FROWS = 5;
    localparam FLOAD = 6;
    
    wire clk;
    wire rst;
    
    wire [DATA_WIDTH-1:0] data;
    wire dataValid;
    wire dataLast;
    
    wire [DATA_WIDTH-1:0] filt;
    wire filtValid;
    wire filtLast;
    
    reg [2:0] stateR;
    reg dataReadyR;
    reg filtReadyR;
    
    reg [DIM_WIDTH-1:0] dataColsR;
    reg [DIM_WIDTH-1:0] dataRowsR;
    reg [DIM_WIDTH-1:0] filtColsR;
    reg [DIM_WIDTH-1:0] filtRowsR;
    reg startR;
    
    reg [BUS_WE_WIDTH-1:0] wrEnR;
    reg [BUS_DATA_WIDTH-1:0] wrDataR;
    reg [BUS_ADDR_WIDTH-1:0] addrR;
    
    reg [CNT_WIDTH-1:0] cntR;
    reg firstR;
    
    wire [DATA_WIDTH-1:0] resData;
    wire resValid;
    wire error;
    
    clk_gen #(.CLK_PERIOD(CLK_PERIOD)) clk_gen_i (.clkOut(clk));
    rst_gen #(.RESET_TIME(RESET_TIME)) rst_gen_i (.rstOut(rst));
    
    file_driver #(
        .FILE_NAME("data.txt")) data_driver(
        .clkIn(clk),
        .rstIn(rst),
        .readyIn(dataReadyR),
        .dataOut(data),
        .validOut(dataValid),
        .lastOut(dataLast));
        
    file_driver #(
        .FILE_NAME("filt.txt")) filt_driver(
        .clkIn(clk),
        .rstIn(rst),
        .readyIn(filtReadyR),
        .dataOut(filt),
        .validOut(filtValid),
        .lastOut(filtLast));
        
    integer i;
    always @(posedge clk) begin
        if (rst) begin
            stateR      <= IDLE;
            dataReadyR  <= 0;
            filtReadyR  <= 0;
            dataColsR   <= 0;
            dataRowsR   <= 0;
            filtColsR   <= 0;
            filtRowsR   <= 0;
            startR      <= 0;
            addrR       <= 0;
            wrEnR       <= 0;
            wrDataR     <= 0;
            cntR        <= 0;
            firstR      <= 0;
        end else begin
            startR      <= 0;
            wrEnR       <= 0;
            case (stateR)
                IDLE : begin
                    if (dataValid) begin
                        dataReadyR  <= 1;
                        stateR      <= DCOLS;
                    end
                end
                DCOLS : begin
                    dataColsR       <= data;
                    stateR          <= DROWS;
                end
                DROWS : begin
                    cntR            <= 0;
                    addrR           <= DATA_ADDR;
                    firstR          <= 1;
                    dataRowsR       <= data;
                    stateR          <= DLOAD;
                end
                DLOAD : begin
                    cntR            <= cntR + 1;
                    wrDataR[cntR*DATA_WIDTH+:DATA_WIDTH] <= data;
                    for (i = 0; i < NUM_WORDS; i = i + 1) begin
                        if ((cntR == (NUM_WORDS - 1)) || dataLast) begin
                            cntR        <= 0;
                            if (firstR) begin
                                firstR  <= 0;
                            end else begin
                                addrR   <= addrR + BUS_WE_WIDTH;
                            end
                            if (cntR >= i) begin
                                wrEnR[i*WE_WIDTH+:WE_WIDTH] <= {WE_WIDTH{1'b1}};
                            end
                        end
                    end
                    if (dataLast) begin
                        dataReadyR  <= 0;
                        filtReadyR  <= 1;
                        stateR      <= FCOLS;
                    end
                end
                FCOLS : begin
                    filtColsR       <= filt;
                    if (filtReadyR) begin
                        stateR      <= FROWS;
                    end
                end
                FROWS : begin
                    cntR            <= 0;
                    firstR          <= 1;
                    addrR           <= FILT_ADDR;
                    filtRowsR       <= filt;
                    stateR          <= FLOAD;
                end
                FLOAD : begin
                    cntR            <= cntR + 1;
                    wrDataR[cntR*DATA_WIDTH+:DATA_WIDTH] <= filt;
                    for (i = 0; i < NUM_WORDS; i = i + 1) begin
                        if ((cntR == (NUM_WORDS - 1)) || filtLast) begin
                            cntR        <= 0;
                            if (firstR) begin
                                firstR  <= 0;
                            end else begin
                                addrR   <= addrR + BUS_WE_WIDTH;
                            end
                            if (cntR >= i) begin
                                wrEnR[i*WE_WIDTH+:WE_WIDTH] <= {WE_WIDTH{1'b1}};
                            end
                        end
                    end
                    if (filtLast) begin
                        filtReadyR  <= 0;
                        startR      <= 1;
                        stateR      <= IDLE;
                    end
                end
            endcase
        end
    end
    
    cnn_hw_accelerator #(
        .BUS_ADDR_WIDTH(BUS_ADDR_WIDTH),
        .BUS_DATA_WIDTH(BUS_DATA_WIDTH),
        .VECTOR_SIZE(VECTOR_SIZE),
        .MAX_SIZE(MAX_SIZE)) accel (
        .clkIn(clk),
        .rstIn(rst),
        .startIn(startR),
        .filtRowsIn(filtRowsR),
        .filtColsIn(filtColsR),
        .dataRowsIn(dataRowsR),
        .dataColsIn(dataColsR),
        .addrIn(addrR),
        .wrEnIn(wrEnR),
        .wrDataIn(wrDataR),
        .readyIn(1'b1),
        .validOut(resValid),
        .dataOut(resData));
    
    file_checker check (
        .clkIn(clk),
        .rstIn(rst),
        .validIn(resValid),
        .dataIn(resData),
        .errorOut(error));
        
endmodule