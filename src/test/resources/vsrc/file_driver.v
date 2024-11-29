module file_driver (
    clkIn,
    rstIn,
    readyIn,
    dataOut,
    validOut,
    lastOut);
    
    parameter DATA_WIDTH  = 32;
    parameter VECTOR_SIZE = 1;
    parameter FILE_NAME   = "input.txt";
    
    input clkIn, rstIn;
    input readyIn;
    output [DATA_WIDTH*VECTOR_SIZE-1:0] dataOut;
    output [VECTOR_SIZE-1:0] validOut;
    output lastOut;
    
    reg [VECTOR_SIZE-1:0] validR;
    reg [DATA_WIDTH*VECTOR_SIZE-1:0] dataR;
    
    reg [VECTOR_SIZE-1:0] valid2R;
    reg [DATA_WIDTH*VECTOR_SIZE-1:0] data2R;
    
    reg [DATA_WIDTH-1:0] fileData;
    
    integer inputFile, i;
    
    initial begin
        inputFile = $fopen(FILE_NAME, "r");
        if (inputFile == 0) begin
            $display("Could not open \"%s\"", FILE_NAME);
            $finish;
        end 
    end
    
    always @(posedge clkIn or posedge rstIn) begin
        if (rstIn) begin
            dataR   <= 0;
            validR  <= 0;
            data2R  <= 0;
            valid2R <= 0;
        end else begin
            if (!valid2R || readyIn) begin
                dataR  <= 0;
                validR <= 0;
                for (i = 0; i < VECTOR_SIZE; i = i + 1) begin
                    if (!$feof(inputFile)) begin
                        $fscanf(inputFile, "%h\n", fileData);
                        dataR[DATA_WIDTH*i +: DATA_WIDTH]   <= fileData;
                        validR[i]                           <= 1;
                    end
                end
                data2R  <= dataR;
                valid2R <= validR;
            end
        end
    end
    
    assign lastOut  = (valid2R != 0) && (validR == 0);
    assign validOut = valid2R;
    assign dataOut  = data2R;
    
endmodule