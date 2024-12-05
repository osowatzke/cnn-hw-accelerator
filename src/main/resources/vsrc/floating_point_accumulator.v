`timescale 1ns/1ns

module floating_point_accumulator (
    clkIn,
    rstIn,
    dataIn,
    validIn,
    lastIn,
    dataOut,
    validOut);

    // Parameters to define floating-point type
    parameter FRAC_WIDTH    = 24;
    parameter EXP_WIDTH     =  8;

    // Derived parameters for floating-point type
    localparam DATA_WIDTH   = FRAC_WIDTH + EXP_WIDTH;

    // Parameters based on adder latency
    localparam ADD_LATENCY  = 13;
    localparam ID_WIDTH     = $clog2(ADD_LATENCY+1);
    localparam NUM_STAGES   = $clog2(ADD_LATENCY+1);
    localparam REG_WIDTH    = 2**ID_WIDTH;

    // Inputs
    input clkIn;
    input rstIn;

    input [DATA_WIDTH-1:0] dataIn;
    input validIn;
    input lastIn;

    // Outputs
    output [DATA_WIDTH-1:0] dataOut;
    output validOut;

    // Control for last accumulation values
    reg [ ID_WIDTH-1:0] idR;
    reg [REG_WIDTH-1:0] lastR;

    // Feedback control
    reg inSelR;

    // Output pipeline
    reg [DATA_WIDTH-1:0] accumDataR;
    reg accumLastR;
    reg accumValidR;

    // Accumulator outputs
    wire [DATA_WIDTH-1:0] accumData;
    wire accumValid;

    wire [ID_WIDTH-1:0] accumId;
    wire accumLast;

    // Assign ID to each accumulation
    // Register with one-hot mapping specifies whether last value is in pipeline
    always @(posedge clkIn) begin
        if (rstIn) begin
            idR     <= 0;
            lastR   <= 0;
        end else begin
            if (accumValid && accumLast) begin
                lastR[accumId]  <= 0;
            end
            if (validIn && lastIn) begin
                idR             <= idR + 1;
                lastR[idR]      <= 1;
            end
        end
    end

    // Determine whether feedback path is enabled
    // Determine whether output is valid
    // All valid outputs must go to feedback or output path
    always @(posedge clkIn) begin
        if (rstIn) begin
            inSelR      <= 0;
            accumValidR <= 0;
        end else begin
            if (accumValid) begin
                if (lastR[accumId] | ((accumId == idR) && validIn && lastIn)) begin
                    inSelR      <= 0;
                    accumValidR <= 1;
                end else begin
                    inSelR      <= 1;
                    accumValidR <= 0;
                end
            end else begin
                inSelR      <= 0;
                accumValidR <= 0;
            end
        end
    end

    // Pipeline last and data values
    always @(posedge clkIn) begin
        accumDataR  <= accumData;
        accumLastR  <= accumLast;
    end

    // Select feedback data
    wire [DATA_WIDTH-1:0] dataB;

    assign dataB = inSelR ? accumDataR : 0;

    // Accumulator. Implements X(z)(1 + z^(-N) + z^(-2N) + ...)
    // Consecutive cycles contain X(z)(z^(-i) + z^(-i-N) + z^(-i-2N) + ...)
    floating_point_add #(.FRAC_WIDTH(FRAC_WIDTH), .EXP_WIDTH(EXP_WIDTH)) accum_i (
        .clkIn(clkIn),
        .rstIn(rstIn),
        .dataAIn(dataIn),
        .dataBIn(dataB),
        .validIn(validIn),
        .dataOut(accumData),
        .validOut(accumValid));

    // Pipeline last signal and ID
    wire [ID_WIDTH:0] iDelay;
    wire [ID_WIDTH:0] oDelay;

    assign iDelay = {lastIn, idR};

    delay #(.DATA_WIDTH(ID_WIDTH+1), .LATENCY(ADD_LATENCY)) delay_i (
        .clkIn(clkIn),
        .rstIn(rstIn),
        .dataIn(iDelay),
        .dataOut(oDelay));

    assign accumLast = oDelay[ID_WIDTH];
    assign accumId   = oDelay[ID_WIDTH-1:0];

    // Define stage I/O
    wire [DATA_WIDTH-1:0] stageIData [0:NUM_STAGES-1];
    wire [DATA_WIDTH-1:0] stageOData [0:NUM_STAGES-1];

    wire stageIValid [0:NUM_STAGES-1];
    wire stageOValid [0:NUM_STAGES-1];

    wire stageILast  [0:NUM_STAGES-1];
    wire stageOLast  [0:NUM_STAGES-1];

    genvar i;
    generate

        // Generate stages to combine consecutive accumulator outputs
        // Log2 decomposition of adder latency
        for (i = 0; i < NUM_STAGES; i = i + 1) begin

            reg [DATA_WIDTH-1:0] dataR;
            reg validR;

            wire valid;

            wire [1:0] iDelay;
            wire [1:0] oDelay;

            // Select stage inputs
            if (i == 0) begin
                assign stageIData [i] = accumDataR;
                assign stageIValid[i] = accumValidR;
                assign stageILast [i] = accumLastR;
            end else begin
                assign stageIData [i] = stageOData [i-1];
                assign stageIValid[i] = stageOValid[i-1];
                assign stageILast [i] = stageOLast [i-1];
            end

            // Adder input is valid if sample and delayed sample are valid
            // Or if last sample is among adder inputs
            always @(posedge clkIn) begin
                if (rstIn) begin
                    validR <= 0;
                end else begin
                    if (stageIValid[i]) begin
                        if (validR | stageILast[i]) begin
                            validR  <= 0;
                        end else begin
                            validR  <= 1;
                        end
                    end
                end
            end

            assign valid = stageIValid[i] & (validR | stageILast[i]);

            // Latch valid data samples
            always @(posedge clkIn) begin
                if (stageIValid[i]) begin
                    dataR   <= stageIData[i];
                end
            end

            // Select 0 for second output if only one is valid
            wire [DATA_WIDTH-1:0] dataB;

            assign dataB = validR ? dataR : 0;

            // Add consecutive samples
            floating_point_add #(.FRAC_WIDTH(FRAC_WIDTH), .EXP_WIDTH(EXP_WIDTH)) add_i (
                .clkIn(clkIn),
                .rstIn(rstIn),
                .dataAIn(stageIData[i]),
                .dataBIn(dataB),
                .validIn(valid),
                .dataOut(stageOData[i]),
                .validOut(stageOValid[i]));

            // Pipeline last signal
            delay #(.DATA_WIDTH(1), .LATENCY(ADD_LATENCY)) delay_i (
                .clkIn(clkIn),
                .rstIn(rstIn),
                .dataIn(stageILast[i]),
                .dataOut(stageOLast[i]));
        end

    endgenerate

    // Asign outputs
    assign dataOut  = stageOData[NUM_STAGES-1];
    assign validOut = stageOLast[NUM_STAGES-1] & stageOValid[NUM_STAGES-1];

endmodule
