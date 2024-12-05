`timescale 1ns/1ns

module fifo (
    clkIn,
    rstIn,
    wrDataIn,
    wrValidIn,
    wrReadyOut,
    rdDataOut,
    rdValidOut,
    rdReadyIn);
    
    // FIFO parameters
    parameter FIFO_STYLE = "RAM";
    parameter DATA_WIDTH = 32;
    parameter FIFO_DEPTH = 512;
    parameter FIFO_SKID  = 0;
    
    // Inputs and Outputs
    input  clkIn;
    input  rstIn;
    
    input  [DATA_WIDTH-1:0] wrDataIn;
    input  wrValidIn;
    output wrReadyOut;
    
    output [DATA_WIDTH-1:0] rdDataOut;
    output rdValidOut;
    input  rdReadyIn;
    
    // Validate FIFO style
    initial begin
        if ((FIFO_STYLE != "RAM") && (FIFO_STYLE != "SRL")) begin
            $error("Unsupported FIFO style \"%s\". Must be either \"RAM\" or \"SRL\"", FIFO_STYLE);
        end
    end
    
    generate
        if (FIFO_STYLE == "RAM") begin
        
            fifo_sdpr #(
                .DATA_WIDTH(DATA_WIDTH),
                .FIFO_DEPTH(FIFO_DEPTH),
                .FIFO_SKID(FIFO_SKID)) fifo_i(
                .clkIn(clkIn),
                .rstIn(rstIn),
                .wrDataIn(wrDataIn),
                .wrValidIn(wrValidIn),
                .wrReadyOut(wrReadyOut),
                .rdDataOut(rdDataOut),
                .rdValidOut(rdValidOut),
                .rdReadyIn(rdReadyIn));
                
        end else if (FIFO_STYLE == "SRL") begin
        
            fifo_srl #(
                .DATA_WIDTH(DATA_WIDTH),
                .FIFO_DEPTH(FIFO_DEPTH),
                .FIFO_SKID(FIFO_SKID)) fifo_i(
                .clkIn(clkIn),
                .rstIn(rstIn),
                .wrDataIn(wrDataIn),
                .wrValidIn(wrValidIn),
                .wrReadyOut(wrReadyOut),
                .rdDataOut(rdDataOut),
                .rdValidOut(rdValidOut),
                .rdReadyIn(rdReadyIn));
                
        end
    endgenerate
endmodule
