`timescale 1ps/1ps

`include "huntest.vh"

module testbench;
    logic clock = 1'b0;
    logic reset = 1'b1;

    initial forever #(10ns/2) clock = ~clock;

    logic [3:0] cntr;

    always_ff @(posedge clock)
      cntr <= reset ? '0 : cntr + 1'b1;

    initial begin
        reset = 1'b1;
        repeat(2) @(posedge clock);
        reset = 1'b0;
        repeat(2) @(posedge clock);

        repeat(10) begin
            `log_info(("C = %d", cntr));
            #(22ns);
        end

        repeat(2) @(posedge clock);

        // `log_fail(("Example failure"));
        `log_success(("Testbench completed"));
        $finish;
    end

    initial begin
`ifdef HUNTEST_TESTBENCH
        `log_info(("BASE_DIR='%s'", `HUNTEST_BASE_DIR));
        `log_info(("TB_DIR='%s'", `HUNTEST_TB_DIR));
        `log_info(("HUNTEST_TESTBENCH defined"));
`else
        `log_info(("HUNTEST_TESTBENCH undefined"));
`endif
    end

    initial begin
        if ($test$plusargs ("dump")) begin
            $dumpfile("testbench.fst");
            $dumpvars(0, testbench);
        end
    end

endmodule // testbench
