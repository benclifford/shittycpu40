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
    // assign LED = led; // can't make LED a register directly...
    assign LED = led;
    assign PIN_3 = led;

    // this reset delay is a known hardware niggle with ice40 loading RAM at initial time - need to wait a few clock cycles after startup. see https://github.com/YosysHQ/icestorm/issues/76
    // with 16000000 delay, works but that is about a second startup time. the github issue talks about 36 clock cycles.
    // so hopefully 16000 clock cycles is plenty for the hardware but not very noticeable to a human - 1ms?
    reg [31:0] rst_delay = 16000;

    reg [31:0] pc;  // program counter

    reg [6:0] instr_phase; // phase through instruction execution

    reg [31:0] instr;

    reg [1:0] pop_phase = 0; // phase through stack pop
    reg pop_skip_top = 0; // when popping, don't pop over the top of the stack
                          // used when an instruction wants to pop, but also has
                          // replaced the top value with a new value.
                          // eg    ... a b => ... new


    reg [31:0] delay_countdown; // for 1xxxxxxx DELAY instruction


    reg [31:0] scratch; // Head of the scratch stack
    reg [31:0] scratch_next; // 2nd element on the scratch stack

    // this wdata register can go to all of the peripherals, as it is only read
    // by a peripheral when the relevant enable signal is used.

    reg [31:0] general_wdata;

    // reg [31:0] scratchstack[128]; // scratch stack aka data stack

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


    // the auxiliary stack

    cellram_scratch auxstackram (
      .CLK (CLK),
      .WDATA (general_wdata),
      .ADDR (auxstack_addr),
      .RDATA (auxstack_rdata),
      .WEN (auxstack_wen)

    );

    wire [31:0] auxstack_rdata;
    reg [7:0] auxstack_addr;
    reg auxstack_wen;
    reg [7:0] auxsp;

    // Does auxsp need to be different from auxstack_addr?
    // I guess it allows auxsp to be incremented separately
    // from preserving the previous value in _addr?
    // Do I care about that?


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
          // Instruction prefix h30000000 is unused: this used to be JUMPZERO, which is placed by LOAD 0, RET
          // and more flexibly, LOAD <somewhere else>, RET
          if( (instr & 32'hF0000000) == 32'h40000000) begin // subtract scratch immediate
            scratch <= scratch - (instr & 32'h0FFFFFFF);
            pc <= pc + 1;
            instr_phase <= 32;
          end
           if( (instr & 32'hF0000000) == 32'h50000000) begin // jump back if non-zero, relative immediate 5
            if (scratch != 0) begin
              pc <= pc - (instr & 32'h0FFFFFFF);
            end else begin
              pc <= pc + 1;
            end
            instr_phase <= 32;
          end
          if( (instr & 32'hF0000000) == 32'h60000000) begin // load scratch immediate
            scratch <= instr & 32'h0FFFFFFF;
            scratch_next <= scratch;
            general_wdata <= scratch_next;
            scratchstack_addr <= scratchsp;
            scratchsp <= scratchsp + 1;
            pc <= pc + 1;
            instr_phase <= 3;
          end
          if( (instr & 32'hFF000000) == 32'h70000000) begin // DROP stack head
            pc <= pc + 1;

            // read scratch ram data into scratch
            instr_phase <= 32;
            pop_phase <= 1;
          end
          if( (instr & 32'hFF000000) == 32'h71000000) begin // DUP stack head
            pc <= pc + 1;
            // scratch -- remains unchanged
            scratch_next <= scratch;
            general_wdata <= scratch_next;
            scratchstack_addr <= scratchsp;
            scratchsp <= scratchsp + 1;
            instr_phase <= 3;
          end
          if( (instr & 32'hFF000000) == 32'h72000000) begin // OVER
            pc <= pc + 1;
            scratch <= scratch_next;
            scratch_next <= scratch;
            general_wdata <= scratch_next;
            scratchstack_addr <= scratchsp;
            scratchsp <= scratchsp + 1;
            instr_phase <= 3;
          end
          if( (instr & 32'hFF000000) == 32'h78000000) begin // PUSH
            // transfer top of scratch stack to aux stack
            pc <= pc + 1;
           
            general_wdata <= scratch;
 
            pop_phase <= 1;

            auxstack_addr <= auxsp;
            auxsp <= auxsp + 1;

            instr_phase <= 18; // pulse off wen.

          end
          if( (instr & 32'hFF000000) == 32'h79000000) begin // POP

            // phase 1:
            // set the address... this phase could go away perhaps if
            // auxsp and auxstack_addr are the same?

            auxstack_addr <= auxsp - 1;
            auxsp <= auxsp - 1;

            pc <= pc + 1;

            // scratch will be set in next phase
            scratch_next <= scratch;
            general_wdata <= scratch_next;
            scratchstack_addr <= scratchsp;
            scratchsp <= scratchsp + 1;

            instr_phase <= 19;

          end
          if( (instr & 32'hFF000000) == 32'h80000000) begin // ADD: a b => (a+b)
            scratch <= scratch + scratch_next;
            pc <= pc + 1;
            instr_phase <= 32;
            pop_phase <= 1;
            pop_skip_top <= 1;
          end
          if( (instr & 32'hFF000000) == 32'h90000000) begin // push stack head down, and put the next-step PC in the top (aka GOSUB) in the new scratch space
            scratch <= pc + 1;
            scratch_next <= scratch;
            general_wdata <= scratch_next;
            scratchstack_addr <= scratchsp;
            scratchsp <= scratchsp + 1;
            instr_phase <= 3;

            pc <= instr & 32'h0FFFFFFF;
          end
          if( (instr & 32'hFF000000) == 32'hA0000000) begin // RET to on-stack new PC (aka GOTO)... like DROP but doing PC stuff with the otherwise-discarded value
            pc <= scratch;

            instr_phase <= 32;
            pop_phase <= 1;
          end
          if( (instr & 32'hFF000000) == 32'hB1000000) begin   // B1  == console uart init - write to UART cfg divider register.
            general_wdata <= 53333; // I would like this to be under program control, eg pop the value from scratch
                                     // it's computed as 16MHz / baudrate = divisor.
                                     // 16Mhz / 53333 should give 300 baud
            pc <= pc + 1;
            instr_phase <= 10; // pulse register write.
          end
          if( (instr & 32'hFF000000) == 32'hB3000000) begin   // B3 = read char from console
            // will read from the console without blocking, push that byte onto the stack
            // If no valid data, the UART gives a 0 byte.
            scratch <= console_dat_do;
            scratch_next <= scratch;
            general_wdata <= scratch_next;
            scratchstack_addr <= scratchsp;
            scratchsp <= scratchsp + 1;

            // This is assuming that the UART is always supplying a data value on console_data_do
            // and doesn't need the pulse first to read; instead just to clear the value for the
            // next character to arrive.
            console_dat_re <= 1;
            pc <= pc + 1;
            instr_phase <= 14; // this should just put console_dat_re back to 0
          end
          if( (instr & 32'hFF000000) == 32'hB4000000) begin   // B4 = write to console - from stack
            // put high uart write, wait till its done, uart low
            // i wonder if i can put the UART high here too?
            // or if i have to wait one cycle? my understanding of
            // when things are allowed to change is a bit fuzzy.
            general_wdata <= scratch & 32'h000000FF;
            instr_phase <= 16;
            pop_phase <= 1;
            pc <= pc + 1;
          end
          if( (instr & 32'hFF000000) == 32'hC0000000) begin   // set tonegen divider
            general_wdata <= scratch;
            pc <= pc + 1;
            instr_phase <= 12;
            pop_phase <= 1;
          end
          if((instr & 32'hFF000000) == 32'hC2000000) begin   // Set LED to LSB of stack
            led <= scratch[0];
            pc <= pc + 1;
            instr_phase <= 32;
            pop_phase <= 1;
          end
          if( (instr & 32'hFF000000) == 32'hC1000000) begin  // Sleep n clock cycles
            // A maximum of around 12 seconds delay possible with 12 MHz clock
            delay_countdown <= scratch;
            pc <= pc + 1;
            instr_phase <= 2; // go into wait-before-phase 0 state
            pop_phase <= 1;
          end

        end  // end of phase 1 decoding
        if(instr_phase == 2) begin // someones requested a delay before going back to phase 0
            if(delay_countdown == 0) begin
                instr_phase <= 32;
            end else begin
                delay_countdown <= delay_countdown - 1;
            end
        end
        if(instr_phase == 3) begin // someones requested stack write
            scratchstack_wen <= 1;
            instr_phase <= 4; // end-write
        end
        if(instr_phase == 4) begin // end write
            scratchstack_wen <= 0;
            instr_phase <= 32;
        end
        if(instr_phase == 10) begin // someones requested console uart clock divisor write
            console_div_we <= 4'b1111;
            instr_phase <= 11;
        end
        if(instr_phase == 11) begin // unpulse clock divisor write
            console_div_we <= 0;
            instr_phase <= 32;
        end
        if(instr_phase == 12) begin // pulse write for tonegen
            tonegen_we <= 1;
            instr_phase <= 13;
        end
        if(instr_phase == 13) begin // pulse write for tonegen
            tonegen_we <= 0;
            instr_phase <= 32;
        end
        if(instr_phase == 14) begin // end pulse write for uart read, start write pulse for stack write
            console_dat_re <= 0;
            scratchstack_wen <= 1;
            instr_phase <= 15; // end-write
        end
        if(instr_phase == 15) begin // end pulse for stack write
            scratchstack_wen <= 0;
            instr_phase <= 32; // this should just put console_dat_re back to 0
        end
        if(instr_phase == 16) begin // wait states for stack read in UARTWRITESTACK
            console_dat_we <= 1;  // enable console write
            instr_phase <= 17;
        end
        if(instr_phase == 17 && !console_dat_wait) begin // wait states for stack read in UARTWRITESTACK
            instr_phase <= 32;
            console_dat_we <= 0; // maybe this has to happen as soon as console_dat_wait goes low?
        end
        if(instr_phase == 18) begin // pulse aux stack write - PUSH
            auxstack_wen <= 1; // unsure if I should delay this pulse on by 1 clockcycle?
            instr_phase <= 20;
        end
        if(instr_phase == 20) begin // unpulse aux stack write - PUSH
            auxstack_wen <= 0;
            instr_phase <= 32;
        end
        if(instr_phase == 19) begin // next part of POP
            instr_phase <= 21; // wait state on auxstack read
        end
        if(instr_phase == 21) begin // next part of POP
            scratch <= auxstack_rdata;
            instr_phase <= 3;
        end
        if(instr_phase == 32 && pop_phase == 0) begin
            // waits for both main instruction to end and for pop_phase to end

            instr_phase <= 0;  // for now, straight back to the start
            // but expect to put post/join handling in here.
        end

        if(pop_phase == 1) begin // someones requested stack read
            scratchstack_addr <= scratchsp - 1;
            scratchsp <= scratchsp - 1;
            pop_phase <= 2;
        end
        if(pop_phase == 2) begin
            // fluffy wait
            pop_phase <= 3;
        end
        if(pop_phase == 3) begin // someones requested stack read
            if (pop_skip_top == 0) scratch <= scratch_next;
            scratch_next <= scratchstack_rdata;
            pop_phase <= 0;
            pop_skip_top <= 0;
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

      auxstack_wen <= 0;
      auxsp <= 0;

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
