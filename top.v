// look in pins.pcf for all the pin names on the TinyFPGA BX board
module top (
    input CLK,    // 16MHz clock
    output LED,   // User/boot LED next to power LED
    output USBPU  // USB pull-up resistor
);
    // drive USB pull-up resistor to '0' to disable USB
    assign USBPU = 0;

    // assign LED = 1; // light the LED just to show somethings happened
    reg [0:0] led = 1;
    assign LED = led; // can't make LED a register directly...

    // this reset delay is a known hardware niggle with ice40 loading RAM at initial time - need to wait a few clock cycles after startup. see https://github.com/YosysHQ/icestorm/issues/76
    // with 16000000 delay, works but that is about a second startup time. the github issue talks about 36 clock cycles.
    // so hopefully 16000 clock cycles is plenty for the hardware but not very noticeable to a human - 1ms?
    reg [31:0] rst_delay = 16000;

    reg [31:0] pc;  // program counter

    reg [8:0] instr_phase; // phase through instruction execution

    reg [31:0] instr;

    reg [31:0] delay_countdown; // for 1xxxxxxx DELAY instruction


    reg [31:0] scratch; // simple register file of 1 register... elaborate later

    // reg [31:0] scratchstack[128]; // scratch stack aka data stack

    // TODO: this is only 16 bits - a stack cell is 32 bits so I need 2
    // also its "only" 256 cells... maybe i want access to the whole ram, not
    // a distinct stack RAM? Although before I was only expecting a stack of
    // 128 cells...

    cellram_scratch scratchstackram (
      .CLK (CLK),
      .WDATA (scratchstack_wdata),
      .ADDR (scratchstack_addr),
      .RDATA (scratchstack_rdata),
      .WEN (scratchstack_wen)

    );

    reg [31:0] scratchstack_wdata;
    reg [31:0] scratchstack_rdata;
    reg [7:0] scratchstack_addr;
    reg scratchstack_wen;

    reg [7:0] scratchsp = 0;

    always @(posedge CLK) begin
      if(rst_delay == 0) begin


        if(instr_phase == 0) begin
            // load an instruction from the PC address
            instr <= ram[pc];
            instr_phase <= 1;
        end
        if(instr_phase == 1) begin
          // when we hit here we should have the instruction to execute in instr
          if(instr == 32'h20000001) begin   // LED on
            led <= 1;
            pc <= pc + 1;
            instr_phase <= 0;
          end
          if(instr == 32'h20000000) begin   // LED off
            led <= 0;
            pc <= pc + 1;
            instr_phase <= 0;
          end
          // any instruction with top nibble 1 means "sleep immediate"
          // with a maximum of around 16 seconds delay possible with 16 MHz clock
          if( (instr & 32'hF0000000) == 32'h10000000) begin
            delay_countdown <= instr & 32'h0FFFFFFF;
            pc <= pc + 1;
            instr_phase <= 2; // go into wait-before-phase 0 state
          end
          if(instr == 32'h30000000) begin
            pc <= 0;
            instr_phase <= 0;
          end
          if( (instr & 32'hF0000000) == 32'h40000000) begin // subtract scratch immediate
            scratch <= scratch - (instr & 32'h0FFFFFFF);
            pc <= pc + 1;
            instr_phase <= 0;
          end
           if( (instr & 32'hF0000000) == 32'h50000000) begin // jump back if non-zero, relative immediate 5
            if (scratch != 0) begin
              pc <= pc - (instr & 32'h0FFFFFFF);
            end else begin
              pc <= pc + 1;
            end
            instr_phase <= 0;
          end
          if( (instr & 32'hF0000000) == 32'h60000000) begin // load scratch immediate
            scratch <= instr & 32'h0FFFFFFF;
            pc <= pc + 1;
            instr_phase <= 0;
          end
          if( (instr & 32'hF0000000) == 32'h70000000) begin // DROP stack head
            scratchstack_addr <= scratchstack - 1;
            scratchsp <= scratchsp - 1;

            pc <= pc + 1;
            instr_phase <= 4; // read scratch ram data into scratch
          end
          if( (instr & 32'hF0000000) == 32'h80000000) begin // PUSH head down, leaving scratch free for new value to assign in another instruction
            scratchstack_wdata <= scratch;
            scratchstack_addr <= scratchsp;
            scratchsp <= scratchsp + 1;
            pc <= pc + 1;
            instr_phase <= 3; // pulse 
          end
 
        end
        if(instr_phase == 2) begin // someones requested a delay before going back to phase 0
            if(delay_countdown == 0) begin
                instr_phase <= 0;
            end else begin
                delay_countdown <= delay_countdown - 1;
            end
        end
        if(instr_phase == 3) begin // someones requested stack write
            scratchstack_wen <= 1;
            instr_phase <= 5; // end-write
        end
        if(instr_phase == 4) begin // someones requested stack read
            scratch <= scratchstack_rdata;
            instr_phase <= 0;
        end
        if(instr_phase == 5) begin // end write
            scratchstack_wen <= 1;
            instr_phase <= 0; // end-write
        end

      end else begin
        rst_delay <= rst_delay - 1;
      end
    end

    // attempt to allocate half the RAM on the ice40
    // 2048 32-bit words, or 8192 kilobytes
    reg [31:0] ram[0:2048] ;

    initial begin

      scratchstack_wen = 0;

      pc = 0;
      instr_phase = 0;

      `include "ram.vh" 

    end;

endmodule

module cellram_scratch (
      input CLK,
      input [31:0] WDATA,
      input WEN,
      input [7:0] ADDR,
      output [31:0] RDATA,
    );

 // I'm assuming i'll be fiddling with RAM stuff later so this
 // block is a passthrough to give me scope for fiddling with
 // the interface. maybe unnecessary.

 ram ramblock (
   .clk (CLK),
   .addr (ADDR),
   .din (WDATA),
   .dout (RDATA),
   .write_en (WEN)
  );

endmodule

// from ice40 RAM block technote, modified by me
module ram (din, addr, write_en, clk, dout);// 512x8
 parameter addr_width = 8;
 parameter data_width = 32;
 input [addr_width-1:0] addr;
 input [data_width-1:0] din;
 input write_en, clk;
 output [data_width-1:0] dout;
 reg [data_width-1:0] dout; // Register for output.
 reg [data_width-1:0] mem [(1<<addr_width)-1:0];
 always @(posedge clk)
 begin
 if (write_en)
 mem[(addr)] <= din;
 dout = mem[addr]; // Output register controlled by clock.
 end
endmodule
