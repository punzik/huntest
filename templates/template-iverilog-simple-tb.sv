`timescale 1ps/1ps

`include "huntest.vh"

module testbench;

    initial begin
`ifdef HUNTEST_TESTBENCH
        `log_info(("BASE_DIR='%s'", `HUNTEST_BASE_DIR));
        `log_info(("TB_DIR='%s'", `HUNTEST_TB_DIR));
        `log_info(("HUNTEST_TESTBENCH defined"));
`else
        `log_info(("TESTBENCH undefined"));
`endif
    end

    initial begin
        if ($test$plusargs ("dump")) begin
            $dumpfile("testbench.fst");
            $dumpvars(0, testbench);
        end
    end

endmodule // testbench
