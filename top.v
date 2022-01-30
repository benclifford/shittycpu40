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

    reg [31:0] delay_countdown; // for 0020 DELAY instruction

    always @(posedge CLK) begin
      if(rst_delay == 0) begin


        if(instr_phase == 0) begin

            // this should load an instruction from mem[pc]
            instr <= ram[pc];
            instr_phase <= 1;
        end
        if(instr_phase == 1) begin
          // when we hit here we should have the instruction to execute in instr
          if(instr == 32'h0011) begin   // LED on
            led <= 1;
            pc <= pc + 1;
            instr_phase <= 0;
          end
          if(instr == 32'h0010) begin   // LED off
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
          if(instr == 32'h0030) begin
            pc <= 0;
            instr_phase <= 0;
          end
        end
        if(instr_phase == 2) begin // someones requested a delay before going back to phase 0
            if(delay_countdown == 0) begin
                instr_phase <= 0;
            end else begin
                delay_countdown <= delay_countdown - 1;
            end
        end
      end else begin
        rst_delay <= rst_delay - 1;
      end
    end

    // attempt to allocate half the RAM on the ice40
    reg [31:0] ram[0:2048] ;

    initial begin

      pc = 0;
      instr_phase = 0;

      ram[0] = 32'h00000011; // LED on
      ram[1] = 32'h11000000; // sleep 1s
      ram[2] = 32'h00000010; // LED off
      ram[3] = 32'h11000000; // sleep 1s
      ram[4] = 32'h00000011; // LED on
      ram[5] = 32'h10800000; // sleep half a second
      ram[6] = 32'h00000010; // LED off
      ram[7] = 32'h10800000; // sleep half a second
      ram[8] = 32'h00000030; // reset PC to start
    end;

endmodule
