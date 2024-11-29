module fifo_tb;

    // Parameters
    parameter CLK_PERIOD = 10;
    parameter RESET_TIME = 100;
    
    parameter FIFO_DEPTH = 32;
    parameter FIFO_SKID  = 4;
    
    wire clk;
    wire rst;

    clk_gen #(.CLK_PERIOD(CLK_PERIOD)) clk_gen_i (.clkOut(clk));
    rst_gen #(.RESET_TIME(RESET_TIME)) rst_gen_i (.rstOut(rst));
    
    reg [31:0] wrDataR;
    reg wrValidR;
    
    reg [31:0] rdDataR;
    reg rdReadyR;
    
    wire [31:0] wrDataPipe;
    wire wrValidPipe;
    
    wire [31:0] rdData;
    wire rdValid;
    wire wrReady;
    
    generate
    
        if (1) begin
            reg [5:0] countR;
            reg [3:0] probR;
            
            always @(posedge clk) begin
                if (rst) begin
                    wrValidR        <= 0;
                    wrDataR         <= 0;
                    probR           <= 4'h8;
                    countR          <= 0;
                end else begin
                    wrValidR        <= 0;
                    if ({1'b0, $random} < {probR, {29{1'b0}}}) begin
                        if (wrReady) begin
                            wrValidR    <= 1;
                            wrDataR     <= wrDataR + 1;
                            countR      <= countR + 1;
                            if (countR == 63) begin
                                countR  <= 0;
                                probR   <= probR >> 1;
                                if (probR == 1) begin
                                    probR   <= 4'h8;
                                end
                            end
                        end
                    end
                end
            end
        end
    endgenerate
    
    delay #(
        .DATA_WIDTH(1),
        .LATENCY(FIFO_SKID-1)) delay_valid(
        .clkIn(clk),
        .rstIn(rst),
        .dataIn(wrValidR),
        .dataOut(wrValidPipe));
    
    delay #(
        .DATA_WIDTH(32),
        .LATENCY(FIFO_SKID-1)) delay_data(
        .clkIn(clk),
        .rstIn(rst),
        .dataIn(wrDataR),
        .dataOut(wrDataPipe));
    
    fifo #(
        .FIFO_DEPTH(FIFO_DEPTH),
        .FIFO_SKID(FIFO_SKID)) fifo_i(
        .clkIn(clk),
        .rstIn(rst),
        .wrDataIn(wrDataPipe),
        .wrValidIn(wrValidPipe),
        .wrReadyOut(wrReady),
        .rdDataOut(rdData),
        .rdValidOut(rdValid),
        .rdReadyIn(rdReadyR));
        
    generate
        
        if (1) begin
            reg [8:0] countR;
            reg [3:0] probR;
            
            always @(posedge clk) begin
                if (rst) begin
                    rdReadyR        <= 0;
                    rdDataR         <= 1;
                    probR           <= 4'h8;
                    countR          <= 0;
                end else begin
                    rdReadyR        <= 0;
                    if ({1'b0, $random} < {probR, {29{1'b0}}}) begin
                        rdReadyR    <= 1;
                    end
                    if (rdValid) begin
                        if (rdReadyR) begin
                            if (rdDataR !== rdData) begin
                                $error("Error Detected at Time %t. Expected 0x%08X, Received 0x%08X", $realtime, rdDataR, rdData);
                            end
                            rdDataR     <= rdDataR + 1;
                            countR      <= countR + 1;
                            if (countR == 255) begin
                                countR  <= 0;
                                probR   <= probR >> 1;
                                if (probR == 1) begin
                                    probR   <= 4'h8;
                                end
                            end
                        end
                    end
                end
            end
        end
    endgenerate
endmodule