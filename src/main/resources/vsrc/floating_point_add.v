module floating_point_add (
    clkIn,
    rstIn,
    dataAIn,
    dataBIn,
    validIn,
    dataOut,
    validOut);
    
    // Parameters to define floating-point type
    parameter FRAC_WIDTH      = 24;
    parameter EXP_WIDTH       =  8;
    
    // Derived parameters for floating-point type
    localparam DATA_WIDTH     = FRAC_WIDTH + EXP_WIDTH;
    localparam MANTISSA_WIDTH = FRAC_WIDTH - 1;
    
    // Parameters to select sub-regions of float
    localparam MANTISSA_LO    = 0;
    localparam MANTISSA_HI    = MANTISSA_LO + MANTISSA_WIDTH - 1;
    localparam EXP_LO         = MANTISSA_HI + 1;
    localparam EXP_HI         = EXP_LO + EXP_WIDTH - 1;
    localparam SIGN_IDX       = EXP_HI + 1;
    
    // Maximum exponent value
    localparam MAX_EXP        = 2**EXP_WIDTH - 1;
    
    // NaN and Inf
    localparam NAN            = {1'b0, {EXP_WIDTH{1'b1}}, 1'b1, {(MANTISSA_WIDTH-1){1'b0}}};
    localparam INF            = {1'b0, {EXP_WIDTH{1'b1}}, {MANTISSA_WIDTH{1'b0}}};
    
    // Width of Padded type +1 bit for sign, +1 implicit bit in mantissa
    localparam PAD_WIDTH      = MANTISSA_WIDTH + 2;
    localparam PAD_WIDTH_LOG2 = $clog2(PAD_WIDTH);
    
    // Latency of module
    localparam LATENCY        = 13;
    
    // Inputs
    input clkIn, rstIn;
    input [DATA_WIDTH-1:0] dataAIn;
    input [DATA_WIDTH-1:0] dataBIn;
    input validIn;
    
    // Outputs
    output [DATA_WIDTH-1:0] dataOut;
    output validOut;
    
    // Sub-regions of floating-point types
    wire aSign, bSign;
    wire [EXP_WIDTH-1:0] aExp, bExp;
    wire [MANTISSA_WIDTH-1:0] aMantissa, bMantissa;

    // Valid Pipeline
    reg [LATENCY-1:0] validR;
    
    // Pipeline #1
    reg aSignR, bSignR;
    reg aInfR, bInfR;
    reg aNaNR, bNaNR;
    reg [MANTISSA_WIDTH:0] aOperandR;
    reg [MANTISSA_WIDTH:0] bOperandR;
    reg [EXP_WIDTH-1:0] maxExpR;
    reg [EXP_WIDTH-1:0] minExpR;
    reg maxSelR;
    
    // Pipeline #2
    reg maxSel2R;
    reg sumSign2R, sumInf2R, sumNaN2R;
    reg [EXP_WIDTH-1:0] maxExp2R;
    reg [EXP_WIDTH-1:0] expShift2R;
    reg signed [PAD_WIDTH-1:0] aOperand2R;
    reg signed [PAD_WIDTH-1:0] bOperand2R; 

    // Pipeline #3
    reg sumInf3R, sumSign3R, sumNaN3R;
    reg [EXP_WIDTH-1:0] maxExp3R;
    reg [PAD_WIDTH_LOG2-1:0] expShift3R;
    reg signed [PAD_WIDTH-1:0] maxOperand3R;
    reg signed [PAD_WIDTH-1:0] minOperand3R;

    // Pipeline #4
    reg sumInf4R, sumSign4R, sumNaN4R;
    reg [EXP_WIDTH-1:0] maxExp4R;
    reg signed [PAD_WIDTH-1:0] maxOperand4R;
    reg signed [2*PAD_WIDTH-1:0] minOperand4R;     
    
    // Pipeline #5
    reg sumInf5R, sumSign5R, sumNaN5R;
    reg [EXP_WIDTH-1:0] maxExp5R;
    reg signed [PAD_WIDTH:0] sumOperandMsbVar;
    reg signed [PAD_WIDTH-1:0] sumOperandLsbVar;
    reg signed [2*PAD_WIDTH:0] sumOperand5R;
    
    // Pipeline #6
    reg sumInf6R, sumSign6R, sumNaN6R;
    reg [EXP_WIDTH-1:0] maxExp6R;
    reg [2*PAD_WIDTH-1:0] sumOperand6R;
    
    // Pipeline #7
    reg sumInf7R, sumSign7R, sumNaN7R, sumZero7R;
    reg [2*PAD_WIDTH-1:0] sumOperand7R;
    reg [EXP_WIDTH-1:0] maxShift7R;
    reg [PAD_WIDTH_LOG2:0] sumShift7R;
    
    // Pipeline #8
    reg sumInf8R, sumSign8R, sumNaN8R, sumZero8R;
    reg [2*PAD_WIDTH-1:0] sumOperand8R;
    reg [EXP_WIDTH-1:0] maxShift8R;
    reg [PAD_WIDTH_LOG2:0] sumShift8R;
    
    // Pipeline #9
    reg sumInf9R, sumNaN9R, sumSign9R;
    reg [2*PAD_WIDTH-1:0] sumOperand9R;
    reg [EXP_WIDTH-1:0] sumExp9R;
    
    // Pipeline 10
    reg sumInf10R, sumNaN10R, sumSign10R, roundBit10R;
    reg [MANTISSA_WIDTH:0] sumOperand10R;
    reg [EXP_WIDTH-1:0] sumExp10R;
    
    // Pipeline 11
    reg sumInf11R, sumNaN11R, sumSign11R;
    reg [MANTISSA_WIDTH+1:0] sumOperand11R;
    reg [EXP_WIDTH-1:0] sumExp11R;
    
    // Pipeline 12
    reg sumInf12R, sumNaN12R, sumSign12R;
    reg [MANTISSA_WIDTH+1:0] sumOperand12R;
    reg [EXP_WIDTH-1:0] sumExp12R;
    
    // Pipeline 13
    reg [DATA_WIDTH-1:0] sum13R;
    
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
        aSignR <= aSign;
        bSignR <= bSign;
        
        // Inf/NaN Checks
        aInfR <= 0;
        aNaNR <= 0;
        if (aExp == MAX_EXP) begin
            if (aMantissa == 0) begin
                aInfR <= 1;
            end else begin
                aNaNR <= 1;
            end
        end
        
        bInfR <= 0;
        bNaNR <= 0;
        if (bExp == MAX_EXP) begin
            if (bMantissa == 0) begin
                bInfR <= 1;
            end else begin
                bNaNR <= 1;
            end
        end
        
        // Determine implicit bits in mantissa
        // and handle subnormal floating point numbers
        // subnormal => 2^(-126) * (0.fraction)
        // normal    => 2^(exp - 127) * (1.fraction)
        if (aExp == 0) begin
            aOperandR <= {aMantissa, 1'b0};
        end else begin
            aOperandR <= {1'b1, aMantissa};
        end
        
        if (bExp == 0) begin
            bOperandR <= {bMantissa, 1'b0};
        end else begin
            bOperandR <= {1'b1, bMantissa};
        end
        
        // Determine the minimum and maximum exponent
        maxExpR     <= aExp;
        minExpR     <= bExp;
        maxSelR     <= 0;
        if (bExp > aExp) begin
            maxExpR <= bExp;
            minExpR <= aExp;
            maxSelR <= 1;
        end      
        
        /* Pipeline #2 */
        maxSel2R    <= maxSelR;
        maxExp2R    <= maxExpR;
        
        // Handle Inf
        sumSign2R   <= 0;
        sumInf2R    <= 0;
        if (aInfR == 1) begin
            sumSign2R <= aSignR;
            sumInf2R  <= 1;
        end else if (bInfR == 1) begin
            sumSign2R <= bSignR;
            sumInf2R  <= 1;
        end
        
        // Handle NaN
        sumNaN2R    <= 0;
        if ((aNaNR == 1) || (bNaNR == 1)) begin
            sumNaN2R <= 1;
        end else if ((aInfR == 1) && (bInfR == 1)) begin
            sumNaN2R <= aSignR ^ bSignR;
        end
        
        // Determine how far to shift minimum operand
        // to align the operands prior to addition
        expShift2R  <= maxExpR - minExpR;
        
        // Create 2's complement numbers
        if (aSignR) begin
            aOperand2R <= -$signed({2'b0, aOperandR});
        end else begin
            aOperand2R <= $signed({2'b0, aOperandR});
        end
        
        if (bSignR) begin
            bOperand2R <= -$signed({1'b0, bOperandR});
        end else begin
            bOperand2R <= $signed({1'b0, bOperandR});
        end
        
        /* Pipeline #3 */
        sumInf3R   <= sumInf2R;
        sumSign3R  <= sumSign2R;
        sumNaN3R   <= sumNaN2R;
        maxExp3R   <= maxExp2R;
        
        // Limit the operand shift
        expShift3R <= expShift2R;
        if (expShift2R > PAD_WIDTH) begin
            expShift3R <= PAD_WIDTH;
        end
        
        // Select maximum operand
        if (maxSel2R == 0) begin
            maxOperand3R <= aOperand2R;
            minOperand3R <= bOperand2R;
        end else begin
            maxOperand3R <= bOperand2R;
            minOperand3R <= aOperand2R;
        end
        
        /* Pipeline 4 */        
        sumInf4R     <= sumInf3R;
        sumSign4R    <= sumSign3R;
        sumNaN4R     <= sumNaN3R;
        maxExp4R     <= maxExp3R;
        
        // Align both operands
        maxOperand4R <= maxOperand3R;
        minOperand4R <= $signed({minOperand3R, {PAD_WIDTH{1'b0}}}) >>> expShift3R;
        
        /* Pipeline 5 */
        sumInf5R     <= sumInf4R;
        sumSign5R    <= sumSign4R;
        sumNaN5R     <= sumNaN4R;
        maxExp5R     <= maxExp4R;
        
        // Sum both operands
        sumOperandMsbVar = {maxOperand4R[PAD_WIDTH-1], maxOperand4R}
            + {minOperand4R[2*PAD_WIDTH-1], minOperand4R[(2*PAD_WIDTH-1):PAD_WIDTH]};
        sumOperandLsbVar = minOperand4R[PAD_WIDTH-1:0];
        sumOperand5R <= {sumOperandMsbVar, sumOperandLsbVar};
        
        /* Pipeline 6 */
        sumInf6R     <= sumInf5R;
        sumNaN6R     <= sumNaN5R;
        maxExp6R     <= maxExp5R;
        
        // Determine the absolute value and sign of the result
        sumSign6R    <= 0;
        sumOperand6R <= $unsigned(sumOperand5R[2*PAD_WIDTH-1:0]);
        if (sumOperand5R[2*PAD_WIDTH] == 1) begin
            sumSign6R    <= 1;
            sumOperand6R <= $unsigned(-sumOperand5R[2*PAD_WIDTH-1:0]);
        end
        
        // Override sign for Inf
        if (sumInf5R == 1) begin
            sumSign6R    <= sumSign5R;
        end            
        
        /* Pipeline 7 */
        sumInf7R     <= sumInf6R;
        sumNaN7R     <= sumNaN6R;
        sumSign7R    <= sumSign6R;
        sumOperand7R <= sumOperand6R;
        
        // Limit shift of result
        maxShift7R   <= maxExp6R + 1;
        
        // Determine how far to shift the sum to the left
        // Want first significant bit in MSB of output
        sumZero7R  <= 1;
        sumShift7R <= 0;
        for (i = 0; i < (2*PAD_WIDTH); i=i+1) begin
            if (sumOperand6R[i] == 1) begin
                sumZero7R  <= 0;
                sumShift7R <= (2*PAD_WIDTH - 1) - i;
            end
        end
        
        /* Pipeline 8 */
        sumInf8R     <= sumInf7R;
        sumNaN8R     <= sumNaN7R;
        sumSign8R    <= sumSign7R;
        sumOperand8R <= sumOperand7R;
        sumZero8R    <= sumZero7R;
        maxShift8R   <= maxShift7R;
        
        // Limit shift of result
        sumShift8R <= sumShift7R;
        if (sumShift7R > maxShift7R) begin
            sumShift8R <= maxShift7R[PAD_WIDTH_LOG2:0];
        end
        
        /* Pipeline 9 */
        sumInf9R     <= sumInf8R;
        sumNaN9R     <= sumNaN8R;
        sumSign9R    <= sumSign8R;
        
        // Determine resulting exponent
        if (sumZero8R == 1) begin
            sumExp9R <= 0;
        end else begin
            sumExp9R <= maxShift8R - sumShift8R;
        end
        
        // Shift result so significant bits are in MSB
        sumOperand9R <= sumOperand8R << sumShift8R;
        
        /* Pipeline 10 */
        sumInf10R     <= sumInf9R;
        sumNaN10R     <= sumNaN9R;
        sumSign10R    <= sumSign9R;
        sumExp10R     <= sumExp9R;
        
        // Select significant bits from result
        sumOperand10R <= sumOperand9R[(2*PAD_WIDTH-1):(PAD_WIDTH+1)];
        
        // Determine round from convergent round
        roundBit10R  <= 0;
        if (sumOperand9R[PAD_WIDTH] == 1) begin
            if ((sumOperand9R[PAD_WIDTH-1:0] != 0) || (sumOperand9R[PAD_WIDTH+1] == 1)) begin
                roundBit10R <= 1;
            end
        end
        
        /* Pipeline #11 */    
        sumInf11R     <= sumInf10R;
        sumNaN11R     <= sumNaN10R;
        sumSign11R    <= sumSign10R;    
        sumExp11R     <= sumExp10R;
        
        // Round result
        sumOperand11R <= sumOperand10R + roundBit10R;
        
        /* Pipeline #12 */
        sumInf12R     <= sumInf11R;
        sumNaN12R     <= sumNaN11R;
        sumSign12R    <= sumSign11R;
        
        // Handle overflow in round
        sumExp12R     <= sumExp11R;
        sumOperand12R <= sumOperand11R;
        if (sumOperand11R[PAD_WIDTH-1] == 1) begin
            sumExp12R     <= sumExp11R + 1;
            sumOperand12R <= sumOperand11R >> 1;
        end
        
        /* Pipeline #13 */
        if (sumNaN12R == 1) begin
            sum13R <= NAN;
        end else if ((sumInf12R == 1) || (sumExp12R == MAX_EXP)) begin
            sum13R <= {sumSign12R, INF[(DATA_WIDTH-2):0]};
        end else begin
            sum13R <= {sumSign12R, sumExp12R, sumOperand12R[MANTISSA_WIDTH-1:0]};
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
    
    assign dataOut  = sum13R;
    assign validOut = validR[LATENCY-1];
    
endmodule
