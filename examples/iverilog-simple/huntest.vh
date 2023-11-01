`ifndef HUNTEST_VERILOG_DEFINES
 `define HUNTEST_VERILOG_DEFINES

// Log level string prefixes for use with $display function.
// Example usage: $display("%sInfo message", `LOG_INFO);
 `define LOG_INFO "INFO#"
 `define LOG_WARN "WARN#"
 `define LOG_FAIL "FAIL#"

// Dirty hacked redefine of $display function. Must be used with two parentheses.
// Example usage: `log_info(("Info message"));
 `define log_info(msg)  begin $display({`LOG_INFO, $sformatf msg}); end
 `define log_warn(msg)  begin $display({`LOG_WARN, $sformatf msg}); end
 `define log_fail(msg)  begin $display({`LOG_FAIL, $sformatf msg}); end
`endif
