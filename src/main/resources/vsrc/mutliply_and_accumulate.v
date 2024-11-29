`timescale 1ns/1ns

module multiply_and_accumulate (    
    clkIn,
    rstIn,
    dataAIn,
    dataBIn,
    validIn,
    lastIn,
    dataOut,
    validOut);
    
    // Parameters to define floating-point type
    parameter FRAC_WIDTH    = 24;
    parameter EXP_WIDTH     = 8;
    
    // Number of vectorized inputs
    parameter VECTOR_SIZE   = 8;
    
    // Derived floating point Parameters
    localparam DATA_WIDTH   = FRAC_WIDTH + EXP_WIDTH;
    
    // Latency of Submodules
    localparam ADD_LATENCY  = 13;
    localparam MULT_LATENCY = 10;
    
    // Number of stages
    localparam NUM_STAGES   = $clog2(VECTOR_SIZE);
    
    // Port declarations
    input clkIn;
    input rstIn;
    
    input [DATA_WIDTH*VECTOR_SIZE-1:0] dataAIn;
    input [DATA_WIDTH*VECTOR_SIZE-1:0] dataBIn;
    input [VECTOR_SIZE-1:0] validIn;
    input lastIn;
    
    output [DATA_WIDTH-1:0] dataOut;
    output validOut;
    
    // Multiplication output
    wire [DATA_WIDTH-1:0] multData [0:VECTOR_SIZE-1];
    wire multValid [0:VECTOR_SIZE-1];
    wire multLast;
    
    // Stage data
    wire [DATA_WIDTH-1:0] stageIData [0:NUM_STAGES-1][0:VECTOR_SIZE-1];
    wire stageIValid [0:NUM_STAGES-1][0:VECTOR_SIZE-1];
    wire stageILast [0:NUM_STAGES-1];
    
    wire [DATA_WIDTH-1:0] stageOData [0:NUM_STAGES-1][0:VECTOR_SIZE-1];
    wire stageOValid [0:NUM_STAGES-1][0:VECTOR_SIZE-1];
    wire stageOLast [0:NUM_STAGES-1];
    
    
    genvar i, j;
    generate
        for (i = 0; i < VECTOR_SIZE; i = i + 1) begin
        
            wire [DATA_WIDTH-1:0] dataA;
            wire [DATA_WIDTH-1:0] dataB;
            
            assign dataA = dataAIn[(i*DATA_WIDTH) +: DATA_WIDTH];
            assign dataB = dataBIn[(i*DATA_WIDTH) +: DATA_WIDTH];
            
            // Elementwise multiplication
            floating_point_multiply #(.FRAC_WIDTH(FRAC_WIDTH), .EXP_WIDTH(EXP_WIDTH)) mult_j (
                .clkIn(clkIn),
                .rstIn(rstIn),
                .dataAIn(dataA),
                .dataBIn(dataB),
                .validIn(validIn[i]),
                .dataOut(multData[i]),
                .validOut(multValid[i]));
        end
    endgenerate
    
    // Pipeline last signal
    delay #(.DATA_WIDTH(1), .LATENCY(MULT_LATENCY)) delay_i (
        .clkIn(clkIn),
        .rstIn(rstIn),
        .dataIn(lastIn),
        .dataOut(multLast));
            
    // Create Adder Tree
    generate
        for (i = 0; i < NUM_STAGES; i = i + 1) begin
        
            localparam NUM_ADDS = VECTOR_SIZE/(2**(i+1));
            
            if (i == 0) begin
                assign stageILast[i] = multLast;
                for (j = 0; j < VECTOR_SIZE; j = j + 1) begin
                    assign stageIData[i][j] = multData[j];
                    assign stageIValid[i][j] = multValid[j];
                end
                
            end else begin
                assign stageILast[i] = stageOLast[i-1];
                for (j = 0; j < VECTOR_SIZE; j = j + 1) begin
                    assign stageIData[i][j] = stageOData[i-1][j];
                    assign stageIValid[i][j] = stageOValid[i-1][j];
                end
            end
            
            for (j = 0; j < VECTOR_SIZE; j = j + 1) begin
                
                if (j < NUM_ADDS) begin
                
                    wire [DATA_WIDTH-1:0] dataA;
                    wire [DATA_WIDTH-1:0] dataB;
                    wire valid;
                    
                    assign dataA = stageIValid[i][2*j]   ? stageIData[i][2*j]   : 0;
                    assign dataB = stageIValid[i][2*j+1] ? stageIData[i][2*j+1] : 0;
                    assign valid = stageIValid[i][2*j] | stageIValid[i][2*j+1];
                    
                    floating_point_add #(.FRAC_WIDTH(FRAC_WIDTH), .EXP_WIDTH(EXP_WIDTH)) add_j (
                        .clkIn(clkIn),
                        .rstIn(rstIn),
                        .dataAIn(dataA),
                        .dataBIn(dataB),
                        .validIn(valid),
                        .dataOut(stageOData[i][j]),
                        .validOut(stageOValid[i][j]));
                        
                end else begin
                    assign stageOData[i][j] = 0;
                    assign stageOValid[i][j] = 0;
                end
            end
            
            // Pipeline last signal
            delay #(.DATA_WIDTH(1), .LATENCY(ADD_LATENCY)) delay_i (
                .clkIn(clkIn),
                .rstIn(rstIn),
                .dataIn(stageILast[i]),
                .dataOut(stageOLast[i]));
        end
    endgenerate
    
    // Accumulate results
    floating_point_accumulator #(.FRAC_WIDTH(FRAC_WIDTH), .EXP_WIDTH(EXP_WIDTH)) accum (
        .clkIn(clkIn),
        .rstIn(rstIn),
        .dataIn(stageOData[NUM_STAGES-1][0]),
        .validIn(stageOValid[NUM_STAGES-1][0]),
        .lastIn(stageOLast[NUM_STAGES-1]),
        .dataOut(dataOut),
        .validOut(validOut));

endmodule