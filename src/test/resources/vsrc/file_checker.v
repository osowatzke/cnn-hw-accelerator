module file_checker (
    clkIn,
    rstIn,
    validIn,
    dataIn,
    errorOut);
    
    parameter  FRAC_WIDTH       = 24;
    parameter  EXP_WIDTH        = 8;

    localparam DATA_WIDTH       = EXP_WIDTH + FRAC_WIDTH;
    localparam MANTISSA_WIDTH   = FRAC_WIDTH - 1;
    
    localparam MANTISSA_LO      = 0;
    localparam MANTISSA_HI      = MANTISSA_WIDTH - 1;
    localparam EXP_LO           = MANTISSA_HI + 1;
    localparam EXP_HI           = EXP_LO + EXP_WIDTH - 1;
    localparam SIGN_IDX         = EXP_HI + 1;
    
    localparam EXP_MAX          = 2**EXP_WIDTH - 1;
    
    input clkIn, rstIn, validIn;
    input [DATA_WIDTH-1:0] dataIn;
    output errorOut;
    
    integer outputFile;
    
    wire sign;
    wire [     EXP_WIDTH - 1:0] exp;
    wire [MANTISSA_WIDTH - 1:0] mantissa;

    reg  signRef, errorR;
    reg  [    DATA_WIDTH - 1:0] dataRef;
    reg  [     EXP_WIDTH - 1:0] expRef;
    reg  [MANTISSA_WIDTH - 1:0] mantissaRef;
    
    // Parse received floating point data
    assign sign     = dataIn[SIGN_IDX];
    assign exp      = dataIn[EXP_HI:EXP_LO];
    assign mantissa = dataIn[MANTISSA_HI:MANTISSA_LO];

    // Open field with reference
    initial begin
        outputFile = $fopen("output.txt", "r");
        if (outputFile == 0) begin
            $display("Could not open \"output.txt\"");
            $finish;
        end 
    end
    
    always @(posedge clkIn or posedge rstIn) begin
        if (rstIn) begin
            errorR <= 0;
        end else if (validIn == 1) begin
            // Ensure no extra data is received
            if ($feof(outputFile)) begin
                errorR <= 1;
                $error("Error Detected at Time %t: Unexpected Data (0x%08H) Received", $realtime, dataIn);
            end else begin
                // Read floating point data from file
                $fscanf(outputFile, "%h\n", dataRef);
                // Parse floating point reference data
                signRef     = dataRef[SIGN_IDX];
                expRef      = dataRef[EXP_HI:EXP_LO];
                mantissaRef = dataRef[MANTISSA_HI:MANTISSA_LO];
                // NaN expected
                if ((expRef == EXP_MAX) && (mantissaRef != 0)) begin
                    // NaN not received
                    if ((exp != EXP_MAX) || (mantissa == 0)) begin
                        errorR <= 1;
                        $error("Error Detected at Time %t: NaN expected but 0x%08H received", $realtime, dataIn);
                    end
                // Infinity or finite data detected
                end else begin
                    if (dataRef != dataIn) begin
                        errorR <= 1;
                        $error("Error Detected at Time %t: Meas = 0x%08H, Ref=0x%08H", $realtime, dataIn, dataRef);
                    end
                end
            end
        end
    end
    
    assign errorOut = errorR;
 
endmodule