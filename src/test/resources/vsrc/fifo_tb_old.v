`timescale 1ns/1ns

module fifo_tb;

    // Parameters
    parameter CLK_PERIOD = 10;
    parameter RESET_TIME = 100;
    
    parameter FIFO_DEPTH = 32;
    parameter FIFO_SKID  = 4;

    // Derived parameters based on inputs
    localparam COUNT_WIDTH = $clog2(FIFO_DEPTH+1);

    wire clk;
    wire rst;

    clk_gen #(.CLK_PERIOD(CLK_PERIOD)) clk_gen_i (.clkOut(clk));
    rst_gen #(.RESET_TIME(RESET_TIME)) rst_gen_i (.rstOut(rst));

    localparam INIT     = 0;
    localparam WRITE    = 1;
    localparam WAIT     = 2;
    localparam READ     = 3;

    reg [1:0] stateR;
    reg initR;

    reg wrValidR;
    reg [31:0] wrDataR;

    reg rdReadyR;
    reg [31:0] rdDataR;

    reg [COUNT_WIDTH-1:0] countR;
    reg [COUNT_WIDTH-1:0] maxCountR;

    wire wrReady;
    wire rdValid;
    wire [31:0] rdData;
    
    always @(posedge clk) begin
        if (rst) begin
            stateR      <= INIT;
            wrValidR    <= 0;
            rdReadyR    <= 0;
            wrDataR     <= 0;
            rdDataR     <= 0;
            countR      <= 0;
            maxCountR   <= FIFO_DEPTH;
            initR       <= 0;
        end else begin
            wrValidR    <= 0;
            rdReadyR    <= 0;
            initR       <= 1;
            if (wrValidR) begin
                wrDataR <= wrDataR + 1;
            end
            if (rdReadyR) begin
                rdDataR <= rdDataR + 1;
            end
            case (stateR)
                INIT : begin
                    if (initR) begin                   
                        stateR      <= WRITE;
                        countR      <= 0;
                        if (maxCountR == FIFO_DEPTH) begin
                            maxCountR   <= 1;
                        end else begin
                            maxCountR   <= maxCountR + 1;
                        end
                        if (!wrReady) begin
                            $error("Expected wrReady = '1', Received wrReady = '0'");
                        end
                        if (rdValid) begin
                            $error("Expected rdValid = '0', Received rdValid = '1'");
                        end
                    end
                end
                WRITE : begin
                    wrValidR    <= 1;
                    countR      <= countR + 1;
                    if (countR < 2) begin
                        if (rdValid) begin
                            $error("Expected rdValid = '0', Received rdValid = '1'");
                        end
                    end else begin
                        if (!rdValid) begin
                            $error("Expected rdValid = '1', Received rdValid = '0'");
                        end
                    end
                    if (countR <= (FIFO_DEPTH - FIFO_SKID)) begin
                        if (!wrReady) begin
                            $error("Expected wrReady = '1', Received wrReady = '0'");
                        end
                    end else begin
                        if (wrReady) begin
                            $error("Expected wrReady = '0', Received wrReady = '1'");
                        end
                    end
                    if (countR == (maxCountR - 1)) begin
                        if (maxCountR == 1) begin
                            stateR  <= READ;
                        end else begin
                            stateR  <= READ;
                        end
                    end
                end
                /*WAIT : begin
                    if (rdValid) begin
                        $error("Expected rdValid = '0', Received rdValid = '1'");
                    end
                    stateR          <= READ;
                end*/
                READ : begin
                    rdReadyR        <= 1;
                    if (rdReadyR) begin
                        if (!rdValid) begin
                            $error("Expected rdValid = '1', Received rdValid = '0'");
                        end
                        if (rdData !== rdDataR) begin
                            $error("Expected rdData = 0x%08X, Received rdData = 0x%08X", rdDataR, rdData);
                        end
                        countR      <= countR - 1;
                        if (countR == 1) begin
                            stateR      <= INIT;
                            rdReadyR    <= 0;
                        end
                    end else begin
                        if (countR == 1) begin
                            if (rdValid) begin
                                $error("Expected rdValid = '0', Received rdValid = '1'");
                            end
                        end else begin
                            if (!rdValid) begin
                                $error("Expected rdValid = '1', Received rdValid = '0'");
                            end
                        end
                    end
                end
            endcase
        end
    end

    fifo #(
        .FIFO_DEPTH(FIFO_DEPTH),
        .FIFO_SKID(FIFO_SKID)) fifo_i(
        .clkIn(clk),
        .rstIn(rst),
        .wrDataIn(wrDataR),
        .wrValidIn(wrValidR),
        .wrReadyOut(wrReady),
        .rdDataOut(rdData),
        .rdValidOut(rdValid),
        .rdReadyIn(rdReadyR));

endmodule