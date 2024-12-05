`timescale 1ns/1ns

module fifo_sdpr (
    clkIn,
    rstIn,
    wrDataIn,
    wrValidIn,
    wrReadyOut,
    rdDataOut,
    rdValidOut,
    rdReadyIn);

    // FIFO parameters
    parameter DATA_WIDTH = 32;
    parameter FIFO_DEPTH = 512;
    parameter FIFO_SKID  = 0;

    // Derived FIFO parameters
    localparam ADDR_WIDTH  = $clog2(FIFO_DEPTH);
    localparam COUNT_WIDTH = $clog2(FIFO_DEPTH+1);
    localparam ALMOST_FULL = FIFO_DEPTH - FIFO_SKID;

    // Inputs and Outputs
    input  clkIn;
    input  rstIn;
    
    input  [DATA_WIDTH-1:0] wrDataIn;
    input  wrValidIn;
    output wrReadyOut;
    
    output [DATA_WIDTH-1:0] rdDataOut;
    output rdValidOut;
    input  rdReadyIn;
    
    // Control Registers
    reg [COUNT_WIDTH-1:0] countR;

    reg wrReadyR;
    reg rdValidR;
    reg fullR;
    reg initR;
    reg rdEnR;
    
    reg [ADDR_WIDTH-1:0] wrAddrR;
    reg [ADDR_WIDTH-1:0] rdAddrR;

    wire rdPipeReady;
    
    wire wrEn;
    wire rdEn;
    
    // Only write when there will be no overflows
    assign wrEn = wrValidIn & (!fullR | rdEn);
    
    // Only read when the data is valid
    assign rdEn = rdPipeReady & rdValidR;

    always @(posedge clkIn) begin
        if (rstIn) begin
            countR              <= 0;
            wrReadyR            <= 0;
            rdValidR            <= 0;
            fullR               <= 0;
            initR               <= 1;
            rdEnR               <= 0;
            wrAddrR             <= 0;
            rdAddrR             <= 0;
        end else begin
            
            // Pipeline the read enable signal to match the RAM read latency
            rdEnR               <= rdEn;
            
            // Determine next count and
            // control signals based on next count
            if (wrEn && !rdEn) begin
                countR          <= countR + 1;
                if (!rdValidR) begin
                    rdValidR    <= 1;
                end
                if (countR == (ALMOST_FULL - 1)) begin
                    wrReadyR    <= 0;
                end
                if (countR == (FIFO_DEPTH - 1)) begin
                    fullR       <= 1;
                end
            end else if (!wrEn && rdEn) begin
                countR          <= countR - 1;
                if (countR == 1) begin
                    rdValidR    <= 0;
                end
                if (countR == ALMOST_FULL) begin
                    wrReadyR    <= 1;
                end
                if (fullR) begin
                    fullR       <= 0;
                end
            end

            // Enable write ready as soon as reset is deasserted
            initR               <= 0;
            if (initR) begin
                wrReadyR        <= 1;
            end

            // Update write pointer
            if (wrEn) begin
                wrAddrR         <= wrAddrR + 1;
            end

            // Update read pointer
            if (rdEn) begin
                rdAddrR         <= rdAddrR + 1;
            end

            // Overflow detection
            if (wrValidIn && fullR && !rdEn) begin
                $error("Fifo overflow detected at time %t", $realtime);
            end
        end
    end

    // Ram signals
    reg [DATA_WIDTH-1:0] ram [0:FIFO_DEPTH-1];
    reg [DATA_WIDTH-1:0] rdDataR;

    // Instantiate RAM
    always @(posedge clkIn) begin
        rdDataR          <= ram[rdAddrR];
        if (wrEn) begin
            ram[wrAddrR] <= wrDataIn;
        end
    end
    
    // Add an SRL FIFO with skid to account for RAM read latency
    // Depth is 3 to provide at least 2 values (with skid)
    // while fetching the next value from RAM
    fifo_srl #(
        .DATA_WIDTH(DATA_WIDTH),
        .FIFO_DEPTH(3),
        .FIFO_SKID(1)) rd_pipe(
        .clkIn(clkIn),
        .rstIn(rstIn),
        .wrDataIn(rdDataR),
        .wrValidIn(rdEnR),
        .wrReadyOut(rdPipeReady),
        .rdDataOut(rdDataOut),
        .rdValidOut(rdValidOut),
        .rdReadyIn(rdReadyIn));

    // Assign outputs
    assign wrReadyOut = wrReadyR;
    
endmodule
