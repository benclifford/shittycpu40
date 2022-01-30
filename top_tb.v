module top_tv;

reg clk;

wire led;
wire pin1;
wire pin2;
wire pin3;
wire usbpu;

localparam period = 1;

top foo (clk, led, pin1, pin2, pin3, usbpu);

integer steps;

initial begin
    $dumpfile("top_tb.vcd");
    $dumpvars(0, foo);
    $dumpvars(0,clk);
    $dumpvars(0,led);
    $dumpvars(0,pin2);
    $dumpvars(0,usbpu);
end


initial begin;
  for(steps=0; steps < 20971520; steps = steps + 1)
  begin
     clk = 0;
     # period
     clk = 1;
     # period;
  end
  $finish;
end

endmodule
