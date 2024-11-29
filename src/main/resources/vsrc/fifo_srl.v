`timescale 1ns/1ns

module fifo_srl (
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
    parameter FIFO_DEPTH = 16;
    parameter FIFO_SKID  = 0;

    // Derived FIFO parameters
    localparam ADDR_WIDTH  = $clog2(FIFO_DEPTH);
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
    reg [ADDR_WIDTH-1:0] addrR;
    reg wrReadyR;
    reg rdValidR;
    reg fullR;
    reg initR;

    wire wrEn;
    wire rdEn;
    
    // Only write when there will be no overflows
    assign wrEn = wrValidIn & (!fullR | rdEn);
    
    // Only read when the data is valid
    assign rdEn = rdReadyIn & rdValidR;
    
    always @(posedge clkIn) begin
        if (rstIn) begin
            addrR               <= {ADDR_WIDTH{1'b1}};
            wrReadyR            <= 0;
            rdValidR            <= 0;
            fullR               <= 0;
            initR               <= 1;
        end else begin
            
            // Determine next count and
            // control signals based on next count
            if (wrEn && !rdEn) begin
                addrR           <= addrR + 1;
                if (!rdValidR) begin
                    rdValidR    <= 1;
                end
                if ((ALMOST_FULL == 1) || (addrR == (ALMOST_FULL - 2))) begin
                    wrReadyR    <= 0;
                end
                if ((FIFO_DEPTH == 1) || (addrR == (FIFO_DEPTH - 2))) begin
                    fullR       <= 1;
                end
            end else if (!wrEn && rdEn) begin
                addrR           <= addrR - 1;
                if (addrR == 0) begin
                    rdValidR    <= 0;
                end
                if (addrR == (ALMOST_FULL - 1)) begin
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

            // Overflow detection
            if (wrValidIn && fullR && !rdEn) begin
                $error("Fifo overflow detected at time %t", $realtime);
            end            
        end
    end
    
    reg [DATA_WIDTH-1:0] dataR [0:FIFO_DEPTH-1];
    integer i;
    
    always @(posedge clkIn) begin
        if (wrEn) begin
            dataR[0] <= wrDataIn;
            for (i = 1; i < FIFO_DEPTH; i = i + 1) begin
                dataR[i] <= dataR[i-1];
            end
        end
    end
    
    assign rdDataOut  = dataR[addrR];
    assign rdValidOut = rdValidR;
    assign wrReadyOut = wrReadyR;
endmodule