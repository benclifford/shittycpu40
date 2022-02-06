// look in pins.pcf for all the pin names on the TinyFPGA BX board
module top (
    input CLK,    // 16MHz clock
    input PIN_2,  // console serial in
    output LED,   // User/boot LED next to power LED
    output PIN_1, // console serial out
    output PIN_3, // copy of internal LED
    output PIN_4, // speaker
    output USBPU  // USB pull-up resistor
);
    // drive USB pull-up resistor to '0' to disable USB
    assign USBPU = 0;

    // assign LED = 1; // light the LED just to show somethings happened
    reg [0:0] led = 1;
    assign LED = led; // can't make LED a register directly...
    assign PIN_3 = led;

    // this reset delay is a known hardware niggle with ice40 loading RAM at initial time - need to wait a few clock cycles after startup. see https://github.com/YosysHQ/icestorm/issues/76
    // with 16000000 delay, works but that is about a second startup time. the github issue talks about 36 clock cycles.
    // so hopefully 16000 clock cycles is plenty for the hardware but not very noticeable to a human - 1ms?
    reg [31:0] rst_delay = 16000;

    reg [31:0] pc;  // program counter

    reg [8:0] instr_phase; // phase through instruction execution

    reg [31:0] instr;

    reg [31:0] delay_countdown; // for 1xxxxxxx DELAY instruction


    reg [31:0] scratch; // simple register file of 1 register... elaborate later

    // this wdata register can go to all of the peripherals, as it is only read
    // by a peripheral when the relevant enable signal is used.

    reg [31:0] general_wdata;

    // reg [31:0] scratchstack[128]; // scratch stack aka data stack

    // TODO: this is only 16 bits - a stack cell is 32 bits so I need 2
    // also its "only" 256 cells... maybe i want access to the whole ram, not
    // a distinct stack RAM? Although before I was only expecting a stack of
    // 128 cells...

    cellram_scratch scratchstackram (
      .CLK (CLK),
      .WDATA (general_wdata),
      .ADDR (scratchstack_addr),
      .RDATA (scratchstack_rdata),
      .WEN (scratchstack_wen)

    );

    wire [31:0] scratchstack_rdata;
    reg [7:0] scratchstack_addr;
    reg scratchstack_wen;

    reg [7:0] scratchsp;


    // the uart

    wire console_resetn = rst_delay == 0;

    reg [3:0] console_div_we = 0;
    wire [31:0] console_div_do;

    reg console_dat_we = 0;
    reg console_dat_re = 0;
    wire [31:0] console_dat_do;
    wire console_dat_wait;

    simpleuart console (
      .clk (CLK),
      .resetn (console_resetn),
      .ser_tx (PIN_1),
      .ser_rx (PIN_2),
      .reg_div_we (console_div_we),
      .reg_div_di (general_wdata),
      .reg_div_do (console_div_do),
      .reg_dat_we (console_dat_we),
      .reg_dat_re (console_dat_re),
      .reg_dat_di (general_wdata),
      .reg_dat_do (console_dat_do),
      .reg_dat_wait (console_dat_wait)
    );


    reg tonegen_we = 0;

    tonegen speaker (
        .clk (CLK),
        .speaker (PIN_4),
        .cfg_divider (general_wdata),
        .cfg_we (tonegen_we)
    );


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
          // Instruction prefix h30000000 is unused: this used to be JUMPZERO, which is placed by LOAD 0, RET
          // and more flexibly, LOAD <somewhere else>, RET
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
            general_wdata <= scratch;
            scratchstack_addr <= scratchsp;
            scratchsp <= scratchsp + 1;
            pc <= pc + 1;
            instr_phase <= 3;
          end
          if( (instr & 32'hF0000000) == 32'h70000000) begin // DROP stack head
            scratchstack_addr <= scratchsp - 1;
            scratchsp <= scratchsp - 1;

            pc <= pc + 1;
            instr_phase <= 4; // read scratch ram data into scratch
          end
          // Instruction prefix h80000000 is unused: this used to be PUSH
          if( (instr & 32'hF0000000) == 32'h90000000) begin // push stack head down, and put the next-step PC in the top (aka GOSUB) in the new scratch space
            // compare with impl of 0x8.. instruction which leaves the space empty -- should be identical apart from messing with PC and the empty scratch cell.
            general_wdata <= scratch;
            scratchstack_addr <= scratchsp;
            scratchsp <= scratchsp + 1;
            instr_phase <= 3;

            pc <= instr & 32'h0FFFFFFF;
            scratch <= pc + 1;
          end
          if( (instr & 32'hF0000000) == 32'hA0000000) begin // RET to on-stack new PC (aka GOTO)... like DROP but doing PC stuff with the otherwise-discarded value
            pc <= scratch;

            scratchstack_addr <= scratchsp - 1;
            scratchsp <= scratchsp - 1;
            instr_phase <= 4;
          end
          if( (instr & 32'hFF000000) == 32'hB1000000) begin   // B1  == console uart init - write to UART cfg divider register.
            general_wdata <= 53333; // I would like this to be under program control, eg pop the value from scratch
                                     // it's computed as 16MHz / baudrate = divisor.
                                     // 16Mhz / 53333 should give 300 baud
            pc <= pc + 1;
            instr_phase <= 6; // pulse register write.
          end
          if( (instr & 32'hFF000000) == 32'hB2000000) begin   // B2 = write to console
            // this is multistep:
            // put value on write register
            // take write high
            // wait until uart says its done
            general_wdata <= instr & 32'h000000FF;
            pc <= pc + 1;
            instr_phase <= 62; // pulse uart data write and wait for response
          end
          if( (instr & 32'hF0000000) == 32'hC0000000) begin   // set tonegen divider
            general_wdata <= instr & 32'h0FFFFFFF;
            pc <= pc + 1;
            instr_phase <= 70;
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
            // fluffy wait
            instr_phase <= 41;
        end
        if(instr_phase == 41) begin // someones requested stack read
            // fluffy wait
            instr_phase <= 42;
        end
        if(instr_phase == 42) begin // someones requested stack read
            scratch <= scratchstack_rdata;
            instr_phase <= 0;
        end
        if(instr_phase == 5) begin // end write
            scratchstack_wen <= 0;
            instr_phase <= 0;
        end
        if(instr_phase == 6) begin // someones requested console uart clock divisor write
            console_div_we <= 4'b1111;
            instr_phase <= 61;
        end
        if(instr_phase == 61) begin // unpulse clock divisor write
            console_div_we <= 0;
            instr_phase <= 0;
        end
        if(instr_phase == 62) begin // uart write
            console_dat_we <= 1;
            instr_phase <= 63;
        end
        if(instr_phase == 63 && !console_dat_wait) begin // wait for UART to respond...
            console_dat_we <= 0;
            instr_phase <= 0;
        end
        if(instr_phase == 70) begin // pulse write for tonegen
            tonegen_we <= 1;
            instr_phase <= 71;
        end
        if(instr_phase == 71) begin // pulse write for tonegen
            tonegen_we <= 0;
            instr_phase <= 0;
        end

      end else begin
        rst_delay <= rst_delay - 1;
      end
    end

    // attempt to allocate half the RAM on the ice40
    // 2048 32-bit words, or 8192 kilobytes
    reg [31:0] ram[0:2048] ;

    initial begin

      scratchstack_wen <= 0;
      scratchsp <= 0;

      scratchsp <= 0;
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
module ram (din, addr, write_en, clk, dout);
 parameter addr_width = 8;
 parameter data_width = 32;
 input [addr_width-1:0] addr;
 input [data_width-1:0] din;
 input write_en, clk;
 output reg [data_width-1:0] dout;
 // reg [data_width-1:0] dout; // Register for output.
 reg [data_width-1:0] mem [(1<<addr_width)-1:0];
 always @(posedge clk)
   begin
   if (write_en) begin
     mem[(addr)] <= din;
   end
   dout <= mem[addr]; // Output register controlled by clock.
 end
endmodule


module tonegen (
  input clk,
  input [31:0] cfg_divider, // trying to get 1khz-ish
  input cfg_we,
  output reg speaker = 0
);

    reg [31:0] phase;
    reg [31:0] reg_cfg_divider = 0;

    always @(posedge clk) begin
        if(cfg_we) begin
            reg_cfg_divider <= cfg_divider;
        end
    end

    always @(posedge clk) begin
        if(reg_cfg_divider == 0) begin
            speaker <= 0;
        end else if(phase > reg_cfg_divider) begin
            speaker <= ~speaker;
            phase <= 0;
        end else begin
            phase <= phase + 1;
        end
    end

endmodule
