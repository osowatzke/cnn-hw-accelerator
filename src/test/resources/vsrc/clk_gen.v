`timescale 1ns/1ns

module clk_gen(clkOut);

    parameter CLK_PERIOD = 10;
    
    output reg clkOut;
    
    initial begin
        clkOut <= 1;
    end
    
    always #(CLK_PERIOD/2) clkOut <= ~clkOut;

endmodule