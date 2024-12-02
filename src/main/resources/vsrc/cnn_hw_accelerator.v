module cnn_hw_accelerator (
    clkIn,
    rstIn,
    startIn,
    filtRowsIn,
    filtColsIn,
    dataRowsIn,
    dataColsIn,
    addrIn,
    wrEnIn,
    wrDataIn,
    readyIn,
    validOut,
    dataOut);

    // Configuration of RISCV bus interface
    parameter BUS_ADDR_WIDTH    = 32;
    parameter BUS_DATA_WIDTH    = 64;
    
    // Floating-point hardware accelerator configuration
    // Standard single precision
    parameter FRAC_WIDTH        = 24;
    parameter EXP_WIDTH         = 8;
    
    // Multiply and accumulate input width
    parameter VECTOR_SIZE       = 8;
    
    // Maximum size of input matrices
    // < Max Rows > * < Max Cols >
    parameter MAX_SIZE          = 4096;

    // Derived RISCV bus parameters
    localparam BUS_WE_WIDTH     = BUS_DATA_WIDTH/8;
    
    // Derived counter parameters
    localparam CNT_WIDTH        = $clog2(MAX_SIZE);

    // Derived vector size parameters
    localparam VECTOR_SIZE_LOG2 = $clog2(VECTOR_SIZE);
    
    // Derived RAM parameters
    localparam RAM_DEPTH        = MAX_SIZE/VECTOR_SIZE;
    localparam RAM_ADDR_WIDTH   = $clog2(RAM_DEPTH);
    localparam RAM_DATA_WIDTH   = FRAC_WIDTH + EXP_WIDTH;
    localparam RAM_WE_WIDTH     = RAM_DATA_WIDTH/8;
      
    // Input/Output Ports
    input clkIn;
    input rstIn;
    
    input startIn;
    input [CNT_WIDTH:0] filtRowsIn;
    input [CNT_WIDTH:0] filtColsIn;
    input [CNT_WIDTH:0] dataRowsIn;
    input [CNT_WIDTH:0] dataColsIn;
    
    input [BUS_ADDR_WIDTH-1:0] addrIn;
    input [  BUS_WE_WIDTH-1:0] wrEnIn;
    input [BUS_DATA_WIDTH-1:0] wrDataIn;
    
    input  readyIn;
    output validOut;
    output [RAM_DATA_WIDTH-1:0] dataOut;
    
    // Bus write registers
    reg [RAM_ADDR_WIDTH-1:0] busAddrR;
    reg [RAM_DATA_WIDTH-1:0] busWrDataR [0:VECTOR_SIZE-1];
    reg [  RAM_WE_WIDTH-1:0] busWrEnR [0:1][0:VECTOR_SIZE-1];
    
    // Map Bus Writes to Correct RAM Banks
    genvar i;
    integer j;
    generate
    
        // Constants for selecting relevant bits of address
        localparam NUM_BYTES    = RAM_WE_WIDTH*VECTOR_SIZE;
        localparam ADDR_LO      = $clog2(NUM_BYTES);
        localparam ADDR_HI      = RAM_ADDR_WIDTH + ADDR_LO;
        
        // Constants for mapping write enable bits
        localparam GROUP_SIZE   = BUS_WE_WIDTH/RAM_WE_WIDTH;
        localparam NUM_GROUPS   = NUM_BYTES/BUS_WE_WIDTH;

        // Constant for selecting write select bits
        localparam WR_SEL_LO    = $clog2(RAM_WE_WIDTH) + $clog2(GROUP_SIZE);
        localparam WR_SEL_HI    = ADDR_LO - 1;
    
        // Extract address and write select bits
        wire [RAM_ADDR_WIDTH:0] busAddr;
        wire [VECTOR_SIZE_LOG2-1:0] busWrSel;
        
        // Get address and write select bits (applies to all groups)
        assign busAddr  = addrIn[  ADDR_HI:ADDR_LO  ];
        assign busWrSel = addrIn[WR_SEL_HI:WR_SEL_LO];
        
        for (i = 0; i < VECTOR_SIZE; i = i + 1) begin
        
            // Determine indices of write enable bits
            localparam WE_LO   = (i % GROUP_SIZE) * RAM_WE_WIDTH;
            localparam WE_HI   = WE_LO + RAM_WE_WIDTH - 1;
            
            // Determine indices of data bits
            localparam DATA_LO = (i % GROUP_SIZE) * RAM_DATA_WIDTH;
            localparam DATA_HI = DATA_LO + RAM_DATA_WIDTH - 1;
            
            // Determine corresponding write select
            localparam WR_SEL  = i / GROUP_SIZE;

            // Extracted data and write enable bits
            wire [  RAM_DATA_WIDTH-1:0] busWrData;
            wire [    RAM_WE_WIDTH-1:0] busWrEn;
            
            // Get write data and write enable bits for group
            assign busWrData = wrDataIn[DATA_HI:DATA_LO];
            assign busWrEn   = wrEnIn  [  WE_HI:WE_LO  ];

            // Map writes to correct set of write enable bits
            always @(posedge clkIn) begin
                busWrDataR[i] <= busWrData;
                for (j = 0; j < 2; j = j + 1) begin
                    busWrEnR[j][i] <= 0;
                    if ((busAddr[RAM_ADDR_WIDTH] == j) && (busWrSel == WR_SEL)) begin
                        busWrEnR[j][i] <= busWrEn;
                    end
                end
            end
        end
        
        // Select only required bits of address
        always @(posedge clkIn) begin
            busAddrR <= busAddr[RAM_ADDR_WIDTH-1:0];
        end        
    endgenerate
    
    // State Definitions
    localparam IDLE = 0;
    localparam CALC = 1;
    
    // Parameters for selecting subregions of column counts
    localparam FILT_COL_CNT_WIDTH = CNT_WIDTH - VECTOR_SIZE_LOG2;
    localparam FILT_COL_CNT_LO    = VECTOR_SIZE_LOG2;
    localparam FILT_COL_CNT_HI    = CNT_WIDTH - 1;
    
    // FSM registers
    reg stateR;
    reg validR;
    
    reg [VECTOR_SIZE_LOG2-1:0] lastRdCntR;
    
    reg [CNT_WIDTH-1:0] maxFiltColCntVar;
    reg [FILT_COL_CNT_WIDTH-1:0] maxFiltColCntR;
    reg [CNT_WIDTH-1:0] maxFiltRowCntR;
    reg [CNT_WIDTH-1:0] maxDataColCntR;
    reg [CNT_WIDTH-1:0] maxDataRowCntR;
    
    reg [CNT_WIDTH:0] filtColsR;
    reg [CNT_WIDTH:0] dataColsR;
    
    // Filter Column Counter
    wire filtColAdv;
    wire filtColClr;
    wire filtColDoneR;
    wire [FILT_COL_CNT_WIDTH-1:0] filtColCntR;
    
    // Filter Row Counter
    wire filtRowAdv;
    wire filtRowClr;
    wire filtRowDoneR;
    wire [CNT_WIDTH-1:0] filtRowCntR;
    
    // Data Column Counter
    wire dataColAdv;
    wire dataColClr;
    wire dataColDoneR;
    wire [CNT_WIDTH-1:0] dataColCntR;
    
    // Data Row Counter
    wire dataRowAdv;
    wire dataRowClr;
    wire dataRowDoneR;
    wire [CNT_WIDTH-1:0] dataRowCntR;
    
    // Done signal
    wire done;
    
    // Read state machine
    always @(posedge clkIn) begin
        if (rstIn) begin
            stateR          <= IDLE;
            validR          <= 0;
            lastRdCntR      <= 0;
            maxFiltColCntR  <= 0;
            maxFiltRowCntR  <= 0;
            maxDataColCntR  <= 0;
            maxDataRowCntR  <= 0;
            filtColsR       <= 0;
            dataColsR       <= 0;
        end else begin
            case (stateR)
                IDLE : begin
                    maxFiltColCntVar = filtColsIn - 1;
                    lastRdCntR      <= maxFiltColCntVar[FILT_COL_CNT_LO-1:0];
                    maxFiltColCntR  <= maxFiltColCntVar[FILT_COL_CNT_HI:FILT_COL_CNT_LO];
                    maxFiltRowCntR  <= filtRowsIn - 1;
                    maxDataColCntR  <= dataColsIn - filtColsIn;
                    maxDataRowCntR  <= dataRowsIn - filtRowsIn;
                    filtColsR       <= filtColsIn;
                    dataColsR       <= dataColsIn;
                    if (startIn) begin
                        validR      <= 1;
                        stateR      <= CALC;
                    end
                end
                CALC : begin
                    if (done) begin
                        validR      <= 0;
                        stateR      <= IDLE;
                    end
                end
            endcase
        end
    end
    
    // Counter control signals
    assign dataRowAdv = filtColDoneR & filtRowDoneR & dataColDoneR & fifoWrReady;
    assign dataRowClr = !validR & fifoWrReady;
    
    assign dataColAdv = filtColDoneR & filtRowDoneR & fifoWrReady;
    assign dataColClr = dataRowClr | dataRowAdv;
    
    assign filtRowAdv = filtColDoneR;
    assign filtRowClr = dataColClr | dataColAdv;
    
    assign filtColAdv = 1;
    assign filtColClr = filtRowClr | (filtRowAdv & !filtRowClr);
       
    // Filter Column Index Counter
    counter #(
        .CNT_WIDTH(FILT_COL_CNT_WIDTH)) filt_col_cnt (
        .clkIn(clkIn),
        .rstIn(1'b0),
        .clrIn(filtColClr),
        .advIn(filtColAdv),
        .endValIn(maxFiltColCntR),
        .cntOut(filtColCntR),
        .doneOut(filtColDoneR));
      
    // Filter Row Index Counter
    counter #(
        .CNT_WIDTH(CNT_WIDTH)) filt_row_cnt (
        .clkIn(clkIn),
        .rstIn(1'b0),
        .clrIn(filtRowClr),
        .advIn(filtRowAdv),
        .endValIn(maxFiltRowCntR),
        .cntOut(filtRowCntR),
        .doneOut(filtRowDoneR));
        
    // Data Column Index Counter
    counter #(
        .CNT_WIDTH(CNT_WIDTH)) data_col_cnt (
        .clkIn(clkIn),
        .rstIn(1'b0),
        .clrIn(dataColClr),
        .advIn(dataColAdv),
        .endValIn(maxDataColCntR),
        .cntOut(dataColCntR),
        .doneOut(dataColDoneR));
    
    // Data Row Index Counter
    counter #(
        .CNT_WIDTH(CNT_WIDTH)) data_row_cnt (
        .clkIn(clkIn),
        .rstIn(1'b0),
        .clrIn(dataRowClr),
        .advIn(dataRowAdv),
        .endValIn(maxDataRowCntR),
        .cntOut(dataRowCntR),
        .doneOut(dataRowDoneR));
        
    // Done signal for 2D Convolution 
    assign done = filtColDoneR & filtRowDoneR & dataColDoneR & dataRowDoneR;
    
    // Pipeline #2
    reg last2R;
    reg [CNT_WIDTH:0] dataCols2R;
    reg [CNT_WIDTH-1:0] dataRowCnt2R;
    reg [CNT_WIDTH-1:0] dataColCnt2R;
    reg [CNT_WIDTH-1:0] filtRowAddr2R;
    reg [CNT_WIDTH-1:0] filtColAddr2R;
    
    // Pipeline #3
    reg last3R;
    reg [CNT_WIDTH-1:0] dataRowAddr3R;
    reg [CNT_WIDTH-1:0] dataColAddr3R;
    reg [CNT_WIDTH-1:0] filtAddr3R;
    
    // Pipeline #4
    reg last4R;
    reg [CNT_WIDTH-1:0] dataAddr4R;
    reg [CNT_WIDTH-1:0] filtAddr4R;
    
    // Pipeline #5
    reg last5R;
    reg [VECTOR_SIZE_LOG2-1:0] dataShift5R;
    reg [VECTOR_SIZE_LOG2-1:0] filtShift5R;
    reg [CNT_WIDTH-1:0] dataAddrVar;
    reg [CNT_WIDTH-1:0] filtAddrVar;
    reg [RAM_ADDR_WIDTH*VECTOR_SIZE-1:0] dataAddr5R;
    reg [RAM_ADDR_WIDTH*VECTOR_SIZE-1:0] filtAddr5R;
    
    // Pipeline #6
    reg last6R;
    reg [VECTOR_SIZE_LOG2-1:0] dataShift6R;
    reg [VECTOR_SIZE_LOG2-1:0] filtShift6R;
    reg [RAM_ADDR_WIDTH*VECTOR_SIZE-1:0] dataAddr6R;
    reg [RAM_ADDR_WIDTH*VECTOR_SIZE-1:0] filtAddr6R;
    
    // Data Process
    always @(posedge clkIn) begin
    
        // Pipeline #2
        last2R        <= filtColDoneR & filtRowDoneR;
        dataCols2R    <= dataColsR;
        dataRowCnt2R  <= dataRowCntR + filtRowCntR;
        dataColCnt2R  <= dataColCntR + {filtColCntR, {FILT_COL_CNT_LO{1'b0}}};
        filtRowAddr2R <= filtRowCntR * filtColsR;
        filtColAddr2R <= {filtColCntR, {FILT_COL_CNT_LO{1'b0}}};
        
        // Pipeline #3
        last3R        <= last2R;
        dataRowAddr3R <= dataRowCnt2R * dataCols2R;
        dataColAddr3R <= dataColCnt2R;
        filtAddr3R    <= filtRowAddr2R + filtColAddr2R;
        
        // Pipeline #4
        last4R        <= last3R;
        dataAddr4R    <= dataRowAddr3R + dataColAddr3R;
        filtAddr4R    <= filtAddr3R;
        
        // Pipeline #5
        last5R        <= last4R;

        // Determine shift required to access correct RAM bank
        dataShift5R   <= dataAddr4R[(VECTOR_SIZE_LOG2-1):0];
        filtShift5R   <= filtAddr4R[(VECTOR_SIZE_LOG2-1):0];
        
        // Determine addresses for each RAM bank
        for (j = 0; j < VECTOR_SIZE; j = j + 1) begin
            dataAddrVar  = dataAddr4R + j;
            filtAddrVar  = filtAddr4R + j;
            
            dataAddr5R[(j*RAM_ADDR_WIDTH)+:RAM_ADDR_WIDTH] <= dataAddrVar[VECTOR_SIZE_LOG2+:RAM_ADDR_WIDTH];            
            filtAddr5R[(j*RAM_ADDR_WIDTH)+:RAM_ADDR_WIDTH] <= filtAddrVar[VECTOR_SIZE_LOG2+:RAM_ADDR_WIDTH];
        end
        
        // Pipeline #6
        last6R      <= last5R;
        dataShift6R <= dataShift5R;
        filtShift6R <= filtShift5R;
        
        // Circular shift
        dataAddr6R  <= (dataAddr5R << (dataShift5R*RAM_ADDR_WIDTH)) | (dataAddr5R >> ((VECTOR_SIZE - dataShift5R)*RAM_ADDR_WIDTH));
        filtAddr6R  <= (filtAddr5R << (filtShift5R*RAM_ADDR_WIDTH)) | (filtAddr5R >> ((VECTOR_SIZE - filtShift5R)*RAM_ADDR_WIDTH));
    end
    
    // Pipeline #2
    reg [VECTOR_SIZE-1:0] rdEn2R;
    reg valid2R;
    
    // Pipeline #3
    reg [VECTOR_SIZE-1:0] rdEn3R;
    
    // Pipeline #4
    reg [VECTOR_SIZE-1:0] rdEn4R;
    
    // Pipeline #5
    reg [VECTOR_SIZE-1:0] rdEn5R;
    
    // Pipeline #6
    reg [VECTOR_SIZE-1:0] dataRdEn6R;
    reg [VECTOR_SIZE-1:0] filtRdEn6R;
    
    // Read Enable Process
    always @(posedge clkIn) begin
        if (rstIn) begin
            valid2R     <= 0;
            rdEn2R      <= 0;
            rdEn3R      <= 0;
            rdEn4R      <= 0;
            rdEn5R      <= 0;
            dataRdEn6R  <= 0;
            filtRdEn6R  <= 0;
        end else begin
        
            // Pipeline #2
            valid2R     <= validR & !(filtColDoneR & filtRowDoneR & !fifoWrReady);
            
            // Determine which bits of read enable are high
            for (j = 0; j < VECTOR_SIZE; j = j + 1) begin
                if (filtColDoneR) begin
                    if (j <= lastRdCntR) begin
                        rdEn2R[j] <= 1;
                    end else begin
                        rdEn2R[j] <= 0;
                    end
                end else begin
                    rdEn2R[j] <= 1;
                end
            end
            
            // Pipeline #3
            if (valid2R) begin
                rdEn3R  <= rdEn2R;
            end else begin
                rdEn3R  <= 0;
            end
            
            // Pipeline #4
            rdEn4R      <= rdEn3R;
            
            // Pipeline #5
            rdEn5R      <= rdEn4R;
            
            // Pipeline #6
            // Circular shift
            dataRdEn6R  <= (rdEn5R << dataShift5R) | (rdEn5R >> (VECTOR_SIZE - dataShift5R));
            filtRdEn6R  <= (rdEn5R << filtShift5R) | (rdEn5R >> (VECTOR_SIZE - filtShift5R));
        end
    end
    
    // RAM constants
    localparam WREN_ZERO  = {  RAM_WE_WIDTH{1'b0}};
    localparam DATA_ZERO  = {RAM_DATA_WIDTH{1'b0}};
    localparam RD_LATENCY = 1;
       
    // RAM Bank A
    wire [RAM_DATA_WIDTH*VECTOR_SIZE-1:0] dataA;
    wire [VECTOR_SIZE_LOG2-1:0] dataAShift;
    
    // RAM Bank B
    wire [RAM_DATA_WIDTH*VECTOR_SIZE-1:0] dataB;
    wire [VECTOR_SIZE_LOG2-1:0] dataBShift;
    
    // Common RAM signals
    wire [VECTOR_SIZE-1:0] ramValid;
    wire ramLast;
    
    // Generate Data RAM for each vector element
    generate
        for (i = 0; i < VECTOR_SIZE; i = i + 1) begin
        
            wire [RAM_ADDR_WIDTH-1:0] rdAddr;
            wire [RAM_DATA_WIDTH-1:0] rdData;
            
            assign rdAddr = dataAddr6R[i*RAM_ADDR_WIDTH+:RAM_ADDR_WIDTH];
            
            dp_ram #(
                .DATA_WIDTH(RAM_DATA_WIDTH),
                .RAM_DEPTH(RAM_DEPTH)) data_ram (
                .clkIn(clkIn),
                .rstIn(rstIn),
                .addrAIn(busAddrR),
                .wrEnAIn(busWrEnR[0][i]),
                .wrDataAIn(busWrDataR[i]),
                .rdEnAIn(1'b0),
                .addrBIn(rdAddr),
                .wrEnBIn(WREN_ZERO),
                .wrDataBIn(DATA_ZERO),
                .rdEnBIn(dataRdEn6R[i]),
                .rdDataBOut(rdData),
                .rdAckBOut(ramValid[i]));
                
            assign dataA[RAM_DATA_WIDTH*i+:RAM_DATA_WIDTH] = rdData;
            
        end
            
        delay #(
            .LATENCY(RD_LATENCY),
            .DATA_WIDTH(VECTOR_SIZE_LOG2)) data_delay (
            .clkIn(clkIn),
            .rstIn(1'b0),
            .dataIn(dataShift6R),
            .dataOut(dataAShift)); 
    endgenerate
        
    // Generate Filter RAM for each vector element
    generate
        for (i = 0; i < VECTOR_SIZE; i = i + 1) begin
        
            wire [RAM_ADDR_WIDTH-1:0] rdAddr;
            wire [RAM_DATA_WIDTH-1:0] rdData;
            
            assign rdAddr = filtAddr6R[i*RAM_ADDR_WIDTH+:RAM_ADDR_WIDTH];
            
            dp_ram #(
                .DATA_WIDTH(RAM_DATA_WIDTH),
                .RAM_DEPTH(RAM_DEPTH)) filt_ram (
                .clkIn(clkIn),
                .rstIn(rstIn),
                .addrAIn(busAddrR),
                .wrEnAIn(busWrEnR[1][i]),
                .wrDataAIn(busWrDataR[i]),
                .rdEnAIn(1'b0),
                .addrBIn(rdAddr),
                .wrEnBIn(WREN_ZERO),
                .wrDataBIn(DATA_ZERO),
                .rdEnBIn(dataRdEn6R[i]),
                .rdDataBOut(rdData));
                
            assign dataB[RAM_DATA_WIDTH*i+:RAM_DATA_WIDTH] = rdData;
            
        end
            
        delay #(
            .LATENCY(RD_LATENCY),
            .DATA_WIDTH(VECTOR_SIZE_LOG2)) filt_delay (
            .clkIn(clkIn),
            .rstIn(1'b0),
            .dataIn(filtShift6R),
            .dataOut(dataBShift)); 
    endgenerate
    
    // Delay last signal to match reads from RAM
    delay #(
        .LATENCY(RD_LATENCY),
        .DATA_WIDTH(1)) last_delay (
        .clkIn(clkIn),
        .rstIn(1'b0),
        .dataIn(last6R),
        .dataOut(ramLast));
    
    // RAM Output Pipeline Stage
    reg ramLastR;
    reg [VECTOR_SIZE-1:0] ramValidR;
    reg [RAM_DATA_WIDTH*VECTOR_SIZE-1:0] dataAR;
    reg [RAM_DATA_WIDTH*VECTOR_SIZE-1:0] dataBR;
    
    // Valid Process
    always @(posedge clkIn) begin
        if (rstIn) begin
            ramValidR  <= 0;
        end else begin
            // Circular shift
            ramValidR  <= (ramValid >> dataAShift) | (ramValid << (VECTOR_SIZE - dataAShift));            
        end
    end
    
    // Data Process
    always @(posedge clkIn) begin
        ramLastR    <= ramLast;
        // Circular shift
        dataAR      <= (dataA >> (dataAShift*RAM_DATA_WIDTH)) | (dataA << ((VECTOR_SIZE - dataAShift)*RAM_DATA_WIDTH));
        dataBR      <= (dataB >> (dataBShift*RAM_DATA_WIDTH)) | (dataB << ((VECTOR_SIZE - dataBShift)*RAM_DATA_WIDTH));
    end
    
    // Multiply and accumulate results
    wire [RAM_DATA_WIDTH-1:0] macData;
    wire macValid;
    
    // Multiply and Accumulate
    multiply_and_accumulate #(
        .FRAC_WIDTH(FRAC_WIDTH),
        .EXP_WIDTH(EXP_WIDTH)) mac(
        .clkIn(clkIn),
        .rstIn(rstIn),
        .dataAIn(dataAR),
        .dataBIn(dataBR),
        .validIn(ramValidR),
        .lastIn(ramLastR),
        .dataOut(macData),
        .validOut(macValid));
       
    // Output FIFO
    fifo #(
        .DATA_WIDTH(RAM_DATA_WIDTH),
        .FIFO_SKID(128)) fifo_i(
        .clkIn(clkIn),
        .rstIn(rstIn),
        .wrDataIn(macData),
        .wrValidIn(macValid),
        .wrReadyOut(fifoWrReady),
        .rdDataOut(dataOut),
        .rdValidOut(validOut),
        .rdReadyIn(readyIn));
    
endmodule