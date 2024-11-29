`timescale 1ns/1ns

module rst_gen(rstOut);

    parameter RESET_TIME = 100;
    
    output reg rstOut;
    
    initial begin
        rstOut <= 1;
        #(RESET_TIME) rstOut <= 0;
    end
    
endmodule