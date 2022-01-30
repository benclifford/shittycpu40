// look in pins.pcf for all the pin names on the TinyFPGA BX board
module top (
    input CLK,    // 16MHz clock
    output LED,   // User/boot LED next to power LED
    output PIN_1,
    output PIN_2,
    output PIN_3,
    output USBPU  // USB pull-up resistor
);
    // drive USB pull-up resistor to '0' to disable USB
    assign USBPU = 0;

    // keep track of time and location in blink_pattern
    reg [26:0] blink_counter = 0;

    // pattern that will be flashed over the LED over time
    wire [31:0] blink_pattern = 32'b10101010101010101010101011111111;

    // increment the blink_counter every clock
    always @(posedge CLK) begin
        blink_counter <= blink_counter + 1;
    end

    reg [1:0] num = 0;

    // assign num[1:0] = blink_counter[24:23];
    assign LED = num[1:0] == 0;
    assign PIN_1 = num[1:0] == 1;
    assign PIN_2 = num[1:0] == 2;
    assign PIN_3 = num[1:0] == 3;

    always @(posedge blink_counter[21]) begin
      num <= num + 1;
    end

/*
    // light up the LED according to the pattern
    assign LED = (blink_counter[23] && blink_counter[26])
              || ((!blink_counter[26]) && blink_counter[10]
                 && blink_counter[11] && blink_counter[12]
                 && blink_counter[21] && blink_counter[9]);

    // also output this on pin 1
    assign PIN_1 = blink_counter[25] && blink_counter[23];
    assign PIN_2 = (~PIN_1) && blink_counter[20];
    assign PIN_3 = blink_counter[20];
*/
endmodule
