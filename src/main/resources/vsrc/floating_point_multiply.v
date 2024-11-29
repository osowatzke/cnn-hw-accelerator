`timescale 1ns / 1ns

module floating_point_multiply (
     clkIn,
     rstIn,
     dataAIn,
     dataBIn,
     validIn,
     dataOut,
     validOut
);

    // Parameters to define floating-point type
    parameter FRAC_WIDTH        = 24;
    parameter EXP_WIDTH         = 8;

    // Derived parameters for floating-point type
    localparam DATA_WIDTH       = FRAC_WIDTH + EXP_WIDTH;
    localparam MANTISSA_WIDTH   = FRAC_WIDTH - 1;

    // Parameters to select sub-regions of float
    localparam MANTISSA_LO      = 0;
    localparam MANTISSA_HI      = MANTISSA_LO + MANTISSA_WIDTH - 1;
    localparam EXP_LO           = MANTISSA_HI + 1;
    localparam EXP_HI           = EXP_LO + EXP_WIDTH - 1;
    localparam SIGN_IDX         = EXP_HI + 1;

    // Maximum exponent value
    localparam MAX_EXP          = 2**EXP_WIDTH - 1;

    // Define mutliplier bias
    localparam BIAS             = 2**(EXP_WIDTH - 1) - 1;
    
    // Determine sizes of partial products
    localparam PRODA_IN_WIDTH   = FRAC_WIDTH/2;
    localparam PRODB_IN_WIDTH   = FRAC_WIDTH - PRODA_IN_WIDTH;

    // Define ranges for partial Product
    localparam PRODA_IN_LO      = 0;
    localparam PRODA_IN_HI      = PRODA_IN_WIDTH - 1;
    localparam PRODB_IN_LO      = PRODA_IN_HI + 1;
    localparam PRODB_IN_HI      = PRODB_IN_LO + PRODB_IN_WIDTH - 1;

    // Determine size of partial product outputs
    localparam PRODA_OUT_SIZE   = FRAC_WIDTH + PRODA_IN_WIDTH;
    localparam PRODB_OUT_SIZE   = FRAC_WIDTH + PRODB_IN_WIDTH;

    // Determine size of Product
    localparam PROD_WIDTH       = 2*FRAC_WIDTH;

    // Determine size of shift inputs
    localparam MAX_R_SHIFT      = FRAC_WIDTH + 1;
    localparam L_SHIFT_WIDTH    = $clog2(PROD_WIDTH-1);
    localparam R_SHIFT_WIDTH    = $clog2(MAX_R_SHIFT);

    // Latency of module
    localparam LATENCY          = 10;

    input clkIn, rstIn;
    input [DATA_WIDTH-1:0] dataAIn;
    input [DATA_WIDTH-1:0] dataBIn;
    input validIn;

    output [DATA_WIDTH-1:0] dataOut;
    output validOut;

    // Pipeline #1
    reg aInfR, bInfR;
    reg aNaNR, bNaNR;
    reg aZeroR, bZeroR;
    reg [FRAC_WIDTH-1:0] aOperandR, bOperandR;

    reg prodSignR;
    reg [EXP_WIDTH:0] prodExpR;

    // Pipeline #2
    reg aZero2R, bZero2R;
    reg prodSign2R;
    reg prodInf2R;
    reg prodNaN2R;
    reg signed [EXP_WIDTH+1:0] prodExp2R;

    reg [PRODA_OUT_SIZE-1:0] prodA2R;
    reg [PRODB_OUT_SIZE-1:0] prodB2R;

    // Pipeline #3
    reg prodSign3R;
    reg prodInf3R;
    reg prodNaN3R;

    reg signed [EXP_WIDTH+1:0] prodExp3R;

    reg [PROD_WIDTH-1:0] prod3R;

    // Pipeline #4
    reg prodSign4R;
    reg prodInf4R;
    reg prodNaN4R;

    reg signed [EXP_WIDTH+1:0] prodExp4R;

    reg [PROD_WIDTH-1:0] prod4R;

    reg [L_SHIFT_WIDTH-1:0] prodShift4R;

    // Pipeline #5
    reg prodSign5R;
    reg prodInf5R;
    reg prodNaN5R;

    reg signed [EXP_WIDTH+2:0] prodExp5R;

    reg [PROD_WIDTH-1:0] prod5R;

    // Pipeline #6
    reg prodSign6R;
    reg prodInf6R;
    reg prodNaN6R;

    reg [EXP_WIDTH+1:0] prodExp6R;

    reg [PROD_WIDTH-1:0] prod6R;

    reg [R_SHIFT_WIDTH-1:0] prodShift6R;

    // Pipeline #7
    reg prodSign7R;
    reg prodInf7R;
    reg prodNaN7R;
    reg prodNorm7R;

    reg [EXP_WIDTH-1:0] prodExp7R;

    reg [PROD_WIDTH+FRAC_WIDTH:0] prod7R;

    // Pipeline #8
    reg prodSign8R;
    reg prodNaN8R;
    reg prodInf8R;
    reg prodSoftInf8R;

    reg [FRAC_WIDTH-1:0] prodMantissaVar;
    reg [PROD_WIDTH:0] prodTruncVar;

    reg [MANTISSA_WIDTH-1:0] prodMantissa8R;
    reg [EXP_WIDTH-1:0] prodExp8R;

    reg maxMantissa8R;
    reg roundBit8R;

    // Pipeline #9
    reg prodSign9R;
    reg prodNaN9R;
    reg prodInf9R;

    reg [MANTISSA_WIDTH-1:0] prodMantissa9R;
    reg [EXP_WIDTH-1:0] prodExp9R;

    // Pipeline #10
    reg prodSign10R;
    reg [MANTISSA_WIDTH-1:0] prodMantissa10R;
    reg [EXP_WIDTH-1:0] prodExp10R;

    // Valid Pipeline
    reg [LATENCY-1:0] validR;

    wire aSign, bSign;
    wire [EXP_WIDTH-1:0] aExp, bExp;
    wire [MANTISSA_WIDTH-1:0] aMantissa, bMantissa;

    integer i;
    
    // Parse Portions of Floating Point Number
    assign aSign = dataAIn[SIGN_IDX];
    assign bSign = dataBIn[SIGN_IDX];

    assign aExp = dataAIn[EXP_HI:EXP_LO];
    assign bExp = dataBIn[EXP_HI:EXP_LO];

    assign aMantissa = dataAIn[MANTISSA_HI:MANTISSA_LO];
    assign bMantissa = dataBIn[MANTISSA_HI:MANTISSA_LO];

    // Data Process
    always @(posedge clkIn) begin

        /* Pipeline #1 */
        // Inf/NaN Checks
        aInfR           <= 0;
        aNaNR           <= 0;
        if (aExp == MAX_EXP) begin
            if (aMantissa == 0) begin
                aInfR   <= 1;
            end else begin
                aNaNR   <= 1;
            end
        end

        bInfR           <= 0;
        bNaNR           <= 0;
        if (bExp == MAX_EXP) begin
            if (bMantissa == 0) begin
                bInfR   <= 1;
            end else begin
                bNaNR   <= 1;
            end
        end

        // Determine if either input is zero
        aZeroR          <= 0;
        if ((aExp == 0) && (aMantissa == 0)) begin
            aZeroR      <= 1;
        end

        bZeroR          <= 0;
        if ((bExp == 0) && (bMantissa == 0)) begin
            bZeroR      <= 1;
        end

        // Compute exponent
        prodExpR        <= aExp + bExp;

        // Determine Sign of Product
        prodSignR       <= aSign ^ bSign;

        // Determine implicit bits in mantissa
        // and handle subnormal floating point numbers
        // subnormal => 2^(-126) * (0.fraction)
        // normal    => 2^(exp - 127) * (1.fraction)
        if (aExp == 0) begin
            aOperandR   <= {aMantissa, 1'b0};
        end else begin
            aOperandR   <= {1'b1, aMantissa};
        end

        if (bExp == 0) begin
            bOperandR   <= {bMantissa, 1'b0};
        end else begin
            bOperandR   <= {1'b1, bMantissa};
        end

        /* Pipeline #2 */
        aZero2R         <= aZeroR;
        bZero2R         <= bZeroR;
        prodSign2R      <= prodSignR;

        // Determine if product is infinity or NaN
        // Note that NaN takes precedence over infinity when determining result
        prodInf2R       <= aInfR | bInfR;
        prodNaN2R       <= aNaNR | bNaNR | (aInfR & bZeroR) | (bInfR & aZeroR);

        // Compute exponent
        prodExp2R       <= prodExpR - (BIAS - 1);

        // Compute partial products
        prodA2R         <= aOperandR * bOperandR[PRODA_IN_HI:PRODA_IN_LO];
        prodB2R         <= aOperandR * bOperandR[PRODB_IN_HI:PRODB_IN_LO];

        /* Pipeline #3 */
        prodSign3R      <= prodSign2R;
        prodInf3R       <= prodInf2R;
        prodNaN3R       <= prodNaN2R;

        // Ensure exponents are zero when either of the inputs is nonzero
        if (aZero2R || bZero2R) begin
            prodExp3R   <= 0;
        end else begin
            prodExp3R   <= prodExp2R;
        end

        prod3R          <= prodA2R + {prodB2R, {PRODA_IN_WIDTH{1'b0}}};

        /* Pipeline #4 */
        prodSign4R      <= prodSign3R;
        prodInf4R       <= prodInf3R;
        prodNaN4R       <= prodNaN3R;
        prod4R          <= prod3R;
        prodExp4R       <= prodExp3R;

        // Determine highest active bit of product
        prodShift4R     <= 0;
        for (i = 0; i < PROD_WIDTH; i = i + 1) begin
            if (prod3R[i]) begin
                prodShift4R     <= ((PROD_WIDTH - 1) - i);
            end
        end

        /* Pipeline #5 */
        prodSign5R      <= prodSign4R;
        prodInf5R       <= prodInf4R;
        prodNaN5R       <= prodNaN4R;
        prod5R          <= prod4R << prodShift4R;
        prodExp5R       <= prodExp4R - $signed(prodShift4R);

        /* Pipeline #6 */
        prodSign6R      <= prodSign5R;
        prodInf6R       <= prodInf5R;
        prodNaN6R       <= prodNaN5R;
        prod6R          <= prod5R;

        // Determine shift required to make exponent positive
        // Maximum shift of output ensures that no significant bits result from round
        if (prodExp5R > 0) begin
            prodExp6R   <= prodExp5R;
            prodShift6R <= 0;
        end else begin
            prodExp6R   <= 0;
            if (prodExp5R < -FRAC_WIDTH) begin
                prodShift6R <= MAX_R_SHIFT;
            end else begin
                prodShift6R <= -prodExp5R + 1;
            end
        end

        /* Pipeline #7 */
        prodSign7R      <= prodSign6R;
        prodInf7R       <= prodInf6R;
        prodNaN7R       <= prodNaN6R;
        prod7R          <= {prod6R, {MAX_R_SHIFT{1'b0}}} >> prodShift6R;

        // Clamp exponent
        if (prodExp6R > MAX_EXP) begin
            prodExp7R   <= MAX_EXP;
        end else begin
            prodExp7R   <= $unsigned(prodExp6R);
        end

        // Determine if float is normal
        if (prodShift6R == 0) begin
            prodNorm7R  <= 1;
        end else begin
            prodNorm7R  <= 0;
        end

        /* Pipeline #8 */
        prodSign8R      <= prodSign7R;
        prodNaN8R       <= prodNaN7R;
        prodExp8R       <= prodExp7R;

        // Extract mantissa and truncated bits
        prodMantissaVar = prod7R[PROD_WIDTH+FRAC_WIDTH:PROD_WIDTH+1];
        prodTruncVar    = prod7R[PROD_WIDTH:0];

        prodMantissa8R  <= prodMantissaVar[MANTISSA_WIDTH-1:0];

        // Determine bit used for implicit round
        roundBit8R      <= 0;
        if (prodTruncVar[PROD_WIDTH]) begin
            if ((prodMantissaVar[0]) || (prodTruncVar[PROD_WIDTH-1:0])) begin
                roundBit8R  <= 1;
            end
        end

        // Determine if mantissa is at its maximum value
        maxMantissa8R   <= 0;
        if (prodMantissaVar[MANTISSA_WIDTH-1:0] == {MANTISSA_WIDTH{1'b1}}) begin
            if (prodNorm7R) begin
                if (prodMantissaVar[FRAC_WIDTH-1]) begin
                    maxMantissa8R   <= 1;
                end
            end else begin
                maxMantissa8R       <= 1;
            end
        end

        // Determine if product is infinity
        prodInf8R           <= prodInf7R;
        prodSoftInf8R       <= 0;
        if (prodExp7R == (MAX_EXP - 1)) begin
            prodSoftInf8R   <= 1;
        end else if (prodExp7R == MAX_EXP) begin
            prodInf8R       <= 1;
        end

        /* Pipeline #9 */
        prodSign9R          <= prodSign8R;
        prodNaN9R           <= prodNaN8R;

        // Increment mantissa when rounding up
        prodMantissa9R      <= prodMantissa8R + roundBit8R;

        // Increment exponent when mantissa wraps
        // Update infinity flag if needed
        prodInf9R           <= prodInf8R;
        if (maxMantissa8R && roundBit8R) begin
            prodExp9R       <= prodExp8R + 1;
            if (prodSoftInf8R) begin
                prodInf9R   <= 1;
            end
        end else begin
            prodExp9R       <= prodExp8R;
        end

        /* Pipeline #10 */
        prodSign10R         <= prodSign9R;
        if (prodNaN9R) begin
            prodExp10R      <= MAX_EXP;
            prodMantissa10R <= {1'b1, {(MANTISSA_WIDTH-1){1'b0}}};
        end else if (prodInf9R) begin
            prodExp10R      <= MAX_EXP;
            prodMantissa10R <= 0;
        end else begin
            prodExp10R      <= prodExp9R;
            prodMantissa10R <= prodMantissa9R;
        end
    end

    // Valid Process
    always @(posedge clkIn or posedge rstIn) begin
        if (rstIn) begin
            validR <= 0;
        end else begin
            validR <= {validR[LATENCY-2:0], validIn};
        end
    end

    assign dataOut  = {prodSign10R, prodExp10R, prodMantissa10R};
    assign validOut = validR[LATENCY-1];

endmodule
