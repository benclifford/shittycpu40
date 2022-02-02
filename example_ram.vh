      ram[0] = 32'h60000003; // load scratch immediate <- 3

      ram[1] = 32'h80000000; // allocate loop variable in scratch
      ram[2] = 32'h60000008; // load scratch immediate <- 8

      // this block eight times
      ram[3] = 32'h20000001; // LED on
      ram[4] = 32'h10080000; // sleep a blip
      ram[5] = 32'h20000000; // LED off
      ram[6] = 32'h10080000; // sleep a blip

      ram[7] = 32'h40000001; // some kind of decrement top of stack by one?

      ram[8] = 32'h50000005; // some kind of jump-back if non-zero / jump if zero - jump back relative, 5

      ram[9] = 32'h70000000; // drop the loop count for this inner loop
      ram[10] = 32'h10800000; // sleep 0.5s

      ram[11] = 32'h40000001; // some kind of decrement top of stack by one?
      ram[12] = 32'h5000000B; // some kind of jump-back if non-zero / jump if zero - jump back relative, 11

      // NO-OP as no stack register file now  ram[6] = 32'h70000000; // some kind of drop from stack, if using stack style registers
 
      ram[13] = 32'h15000000; // sleep 5s
      ram[14] = 32'h30000000; // reset PC to start - jump-unconditional to start

