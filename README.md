# Huntest

Huntest is a launcher and small Guile library for HDL testbenches.  A `.hut`
file defines one or more testbenches; Huntest discovers them, creates isolated
work directories, runs their tests in parallel by default, formats tagged
output, and returns a CI-friendly exit status.

The project includes adapters for:

- custom Guile test bodies;
- Icarus Verilog (`iverilog` + `vvp`);
- Verilator (`verilator --binary`);
- SymbiYosys (`sby`).

> **Status:** work in progress.  Huntest runs on Unix-like systems and the
> flake exposes `x86_64-linux` and `aarch64-linux` outputs.

Framework users should start with the [public API reference](docs/api.md).

## Quick start with Nix

Run Huntest directly from GitHub:

```console
nix run github:punzik/huntest -- --help
```

Arguments for Huntest go after `--`:

```console
nix run github:punzik/huntest -- -r tests +seed=17
```

The default development shell contains Huntest and Guile:

```console
nix develop github:punzik/huntest
```

A Verilator-enabled shell is also provided:

```console
nix develop github:punzik/huntest#verilator
```

Huntest deliberately does not bundle every simulator.  Icarus Verilog,
Verilator, or SymbiYosys must be available on `PATH` for the corresponding
adapter.  Enter the development environment supplied by the HDL project, or
provide the selected backend through Nix.

Useful local commands:

```console
nix run . -- --version
nix run . -- --list-all templates
nix develop .#verilator
```

## Command-line usage

```text
huntest [OPTION]... [PATH ...] [PLUSARGS]
```

With no `PATH`, Huntest searches the current directory for `.hut` files.  The
search is non-recursive unless `-r` is supplied.  A path may name a `.hut` file
or a directory.

```console
# Inventory testbenches without running them.
huntest --list-all -r tests

# Run one test sequentially, keep its work directory, and print all output.
huntest -n -v -k \
  -Q '^alu simulation$::^smoke$' \
  tests/alu.hut +seed=17

# Run every .hut file under tests/.
huntest -r tests

# Remove Huntest work directories selected by the supplied scripts.
huntest -C -r tests
```

### Frequently used options

| Option | Meaning |
| --- | --- |
| `-Q`, `--query QUERY` | Select testbenches and tests with a regular-expression query. May be repeated. |
| `-r`, `--recursive` | Search directories recursively. |
| `-l`, `--list` | List selected testbenches without running them. |
| `-a`, `--list-all` | List all testbenches, including deferred ones. |
| `-n`, `--nopar` | Run sequentially. Default execution is parallel. |
| `-v`, `--verbose` | Print ordinary, untagged output too. |
| `-q`, `--quiet` | Suppress successful testbench details. |
| `-k`, `--keep` | Keep work directories after successful tests. |
| `-s`, `--static` | Use a stable work directory; also enables `--keep`. |
| `-i`, `--incremental` | Reuse a static work directory instead of deleting it first. |
| `-w`, `--work PATH` | Put work directories below `PATH`. |
| `-C`, `--clean` | Delete selected work directories. Use with care. |
| `-c`, `--color` | Colorize output. |
| `-x`, `--regex REGEX` | File-name regex used during discovery; default is `\.hut$`. |
| `-f`, `--defines` | Print the `huntest.vh` logging header to stdout. |
| `-V`, `--version` | Print the Huntest version. |
| `-h`, `--help` | Print command-line help. |

A plusarg starts with `+` and is passed to the selected test body.  For
example, `+dump` can enable waveform dumping and `+seed=17` can select a
random seed.

### Color scheme

Colors are emitted only with `-c` / `--color`.  In auto mode, Huntest reads
the final numeric background field of the commonly exported `COLORFGBG`
variable.  A light background selects darker, high-contrast colors; a dark
background retains the bright palette.  When the terminal does not expose
`COLORFGBG`, the dark palette is the safe fallback.

Override auto-detection when necessary:

```console
HUNTEST_COLOR_SCHEME=light huntest -c -r tests
HUNTEST_COLOR_SCHEME=dark  huntest -c -r tests
HUNTEST_COLOR_SCHEME=auto  huntest -c -r tests
```

### Queries

A query has the form:

```text
testbench-regexp::test-regexp
```

Either part may be empty.  If `::` is absent, the expression selects
testbench names only.

| Query | Selection |
| --- | --- |
| `::` | All non-deferred tests in all testbenches. |
| `^alu$::` | Every test in the testbench named `alu`. |
| `::reset [13]` | Tests whose name contains `reset 1` or `reset 3`. |
| `alu::smoke` | Matching tests in matching testbenches. |

Quote queries in the shell so that spaces and regular-expression metacharacters
arrive unchanged.

## Test result protocol

Huntest uses line prefixes to classify output:

| Prefix | Meaning |
| --- | --- |
| `INFO#` | Informational output. |
| `WARN#` | Warning output. |
| `SUCCESS#` | Explicit confirmation that a normal testbench completed successfully. |
| `FAIL#` | Test failure. |

A normal test is `PASS` only when all of the following hold:

1. its body returns a truthy value;
2. its captured output contains no line beginning with `FAIL#`;
3. its captured output contains a line beginning with `SUCCESS#`.

If the body otherwise succeeds but does not emit `SUCCESS#`, Huntest reports
`UNKNOWN`, returns a non-zero status, and retains evidence in the work
directory.  A false callback return, a failed backend process, or a `FAIL#`
line is `FAIL`.

The testbench as a whole additionally requires successful `#:init` and
`#:finish` callbacks.  `#:expect-fail #t` inverts only the tag expectation: it
passes when a truthy body emits `FAIL#`; it does **not** require `SUCCESS#` and
does not make a non-zero backend exit successful.

### Tagged output from Guile

```scheme
(hut::println 'info "seed: ~a" seed)
(hut::println 'warn "using fallback implementation")
(hut::println 'success "Testbench completed")
(hut::println 'fail "expected 12, got ~a" actual)
```

### Tagged output from Verilog/SystemVerilog

Generate a version-matched header:

```console
huntest --defines > huntest.vh
```

Include it in the testbench and emit success before `$finish`:

```systemverilog
`include "huntest.vh"

initial begin
  if (actual !== expected)
    `log_fail(("expected=%0h actual=%0h", expected, actual));
  else
    `log_success(("check completed"));
  $finish;
end
```

Macro calls use doubled parentheses.  A direct
`$display("SUCCESS#message")` or `$display("FAIL#message")` also works.

## Writing `.hut` files

A `.hut` file is Guile Scheme.  It evaluates to testbench records through
`hut::run`.  See the complete [public API reference](docs/api.md) for every
exported binding and backend keyword.

```scheme
(import (prefix (huntest) hut::))

(hut::run
 (hut::make-testbench
  #:name "alu"
  #:help "Basic ALU checks"

  #:init
  (lambda (plusargs base-path tb-path)
    ;; Shared setup.
    #t)

  #:tests
  (list
   (hut::make-test
    #:name "smoke"
    #:body
    (lambda (plusargs base-path tb-path test-path)
      ;; Per-test work belongs under (test-path).
      (hut::println 'success "ALU smoke test completed")
      #t))

   (hut::make-test
    #:name "known diagnostic"
    #:expect-fail #t
    #:body
    (lambda args
      (hut::println 'fail "intentional expected failure")
      #t)))

  #:finish
  (lambda (plusargs base-path tb-path)
    ;; Shared cleanup.
    #t)))
```

### Constructors

`hut::make-testbench` accepts:

| Keyword | Default | Meaning |
| --- | --- | --- |
| `#:name` | `"unnamed"` | Name used in output and the left side of a query. |
| `#:help` | `#f` | String or list of strings shown by list commands. |
| `#:defer` | `#f` | Skip by default; run only when explicitly selected. |
| `#:init` | truthy no-op | Callback before the tests. |
| `#:tests` | empty list | One test or a list of tests. |
| `#:finish` | truthy no-op | Callback after the test run is reached. |

`hut::make-test` accepts `#:name`, `#:body`, and `#:expect-fail`.

### Callback arguments and paths

```text
init:   (lambda (plusargs base-path tb-path) ...)
test:   (lambda (plusargs base-path tb-path test-path) ...)
finish: (lambda (plusargs base-path tb-path) ...)
```

The path arguments are procedures, not strings:

```scheme
(base-path)                 ; directory containing the .hut file
(base-path "rtl/alu.sv")    ; path relative to the .hut file
(tb-path)                   ; testbench work directory
(test-path)                 ; private work directory for this test
(test-path "result.json")
```

Tests in one testbench can run concurrently.  Do not write shared mutable
artifacts outside `test-path` without synchronization.

`hut::system%` runs an arbitrary shell command and returns its process status;
use it only with trusted command strings.  The Icarus and Verilator adapters
pass their backend arguments as argv instead, so paths, HDL literals, and
parameter strings are not reparsed by a shell.

## Work directories and artifacts

Each testbench receives a work directory and each test receives a private
subdirectory.  Backend artifacts such as `.vvp`, Verilator `obj_dir`,
waveforms, generated `.sby` files, and `log.txt` are placed there.

Successful work is removed by default.  Use `-k` to keep it, or `-s` for a
predictable directory name during debugging.  Failed and `UNKNOWN` tests keep
their useful artifacts; successful test directories inside an otherwise failed
testbench may be pruned.

For a focused investigation, a practical command is:

```console
huntest -n -v -k -s -Q '^alu$::^smoke$' tests/alu.hut +dump
```

## Icarus Verilog adapter

Import `(huntest iverilog)` and use `iverilog::test-body-simple`:

```scheme
(import (prefix (huntest) hut::)
        (prefix (huntest iverilog) iverilog::))

(hut::run
 (hut::make-testbench
  #:name "Icarus ALU"
  #:tests
  (hut::make-test
   #:name "smoke"
   #:body
   (iverilog::test-body-simple
    #:sources '("rtl/alu.sv" "tb/alu_tb.sv")
    #:top "alu_tb"
    #:compile-flags '("-g2012" "-Wall")
    #:runtime-flags '("-fst")
    #:include-paths '("rtl/include")
    #:defines '(("WIDTH" 8) "HUNTEST_SIM")
    #:parameters '(("CASES" "32"))
    #:fail-on-warnings? #t))))
```

The adapter compiles with `iverilog`, then runs the generated `.vvp` file with
`vvp`.  It automatically adds the `.hut` directory as an include directory and
defines `HUNTEST_TESTBENCH`, `HUNTEST_BASE_DIR`, and `HUNTEST_TB_DIR`.

### Icarus keywords

| Keyword | Default | Meaning |
| --- | --- | --- |
| `#:sources` | required | Source list or a callback returning a source list. |
| `#:top` | required | Top module name. |
| `#:compile-flags` | `()` | Individual `iverilog` argv entries. |
| `#:runtime-flags` | `()` | Individual `vvp` argv entries. |
| `#:include-paths` | `()` | Paths relative to the `.hut` file. |
| `#:defines` | `()` | Bare define strings or `(name value)` pairs. |
| `#:parameters` | `()` | `(name value)` pairs for root-module `-Ptop.name=value` overrides. |
| `#:fail-on-warnings?` | `#f` | Fail on captured case-insensitive `warning:` diagnostics. |
| `#:init`, `#:finish` | truthy no-op | Per-test callbacks. |

Each list entry is one process argument.  Do not combine several shell
arguments into one string.

Parameter values are Verilog expressions.  Numeric literals need no shell
escaping:

```scheme
#:parameters '(("VALUE" "8'h12"))
```

For a Verilog string literal, keep the double quotes in the Scheme value:

```scheme
#:parameters '(("MESSAGE" "\"hello world\""))
```

Icarus and VVP warnings do not fail tests by default.  Set
`#:fail-on-warnings? #t` when diagnostics must be treated as verification
failures.

See `examples/iverilog-simple/` and `templates/template-iverilog-simple.hut`.

## Verilator adapter

`verilator::test-body-simple` targets self-contained SystemVerilog testbenches
that can run using Verilator's generated main program.  It builds each test as:

```text
verilator --binary --timing --top-module <top> \
  --Mdir <test-dir>/obj_dir -o simulation ...
```

and executes `<test-dir>/obj_dir/simulation` with runtime flags and plusargs.

```scheme
(import (prefix (huntest) hut::)
        (prefix (huntest verilator) verilator::))

(hut::run
 (hut::make-testbench
  #:name "Verilator ALU"
  #:tests
  (hut::make-test
   #:name "smoke"
   #:body
   (verilator::test-body-simple
    #:sources '("rtl/alu.sv" "tb/alu_tb.sv")
    #:top "alu_tb"
    #:compile-flags '("--trace-fst")
    #:include-paths '("rtl/include")
    #:defines '(("WIDTH" 8))
    #:parameters '(("CASES" "32"))
    #:timing? #t
    #:build-jobs 1
    #:fail-on-warnings? #t))))
```

### Verilator keywords

The common keywords (`#:sources`, `#:compile-flags`, `#:runtime-flags`,
`#:include-paths`, `#:defines`, `#:init`, `#:finish`) have the same meaning as
for Icarus.  Differences are:

| Keyword | Default | Meaning |
| --- | --- | --- |
| `#:parameters` | `()` | `(name value)` pairs emitted as `-Gname=value`. |
| `#:timing?` | `#t` | Use `--timing`; set `#f` to use `--no-timing`. |
| `#:build-jobs` | `1` | Verilator build parallelism for this test. |
| `#:fail-on-warnings?` | `#f` | Fail on `%Warning-...` or `warning:` output. |

Verilator often exits non-zero on warnings itself.  The explicit warning option
also covers configurations using `-Wno-fatal` and C++ compiler diagnostics.
Each test has a private `obj_dir`, making default parallel Huntest execution
safe; the conservative `#:build-jobs 1` avoids multiplying compiler jobs.

See `examples/verilator-simple/` and
`templates/template-verilator-simple.hut`.

## SymbiYosys adapter

The `(huntest sby)` adapter writes an `.sby` file into the test directory and
runs `sby -f` there.

```scheme
(import (prefix (huntest) hut::)
        (prefix (huntest sby) sby::))

(hut::run
 (hut::make-testbench
  #:name "counter formal"
  #:tests
  (hut::make-test
   #:name "bounded proof"
   #:body
   (sby::test-body-simple
    #:sources '("rtl/counter.sv")
    #:top "counter"
    #:mode 'prove
    #:depth 32
    #:engine 'yices
    #:frontend 'yosys-systemverilog
    #:finish
    (lambda args
      (hut::println 'success "SymbiYosys completed")
      #t)))))
```

| Keyword | Default | Meaning |
| --- | --- | --- |
| `#:mode` | `'bmc` | One of `'bmc`, `'prove`, or `'cover`. |
| `#:depth` | `20` | Formal depth. |
| `#:append` | `0` | Extra depth appended to the run. |
| `#:engine` | `'yices` | `'yices` or `'boolector`. |
| `#:frontend` | `'yosys-verilog` | `'yosys-verilog`, `'yosys-systemverilog`, `'verific`, or `'synlig`. |
| `#:strip-output` | `#f` | Remove the `.hut` base path from printed SBY output. |
| `#:parameters`, `#:defines` | `()` | Values used to generate the SBY script. |

The adapter uses the normal test protocol, so the `#:finish` callback in the
example emits `SUCCESS#` after the backend has been invoked.  A non-zero `sby`
exit still fails the test.

See `templates/template-sby-simple.hut`.

## Templates and examples

| Path | Purpose |
| --- | --- |
| `templates/template.hut` | Minimal custom Guile test. |
| `templates/template-iverilog-simple.hut` | Icarus adapter skeleton. |
| `templates/template-verilator-simple.hut` | Verilator adapter skeleton. |
| `templates/template-sby-simple.hut` | SymbiYosys adapter skeleton. |
| `examples/iverilog-simple/` | Runnable Icarus example. |
| `examples/verilator-simple/` | Runnable Verilator example. |

The Verilog templates include `huntest.vh`.  Generate that header with
`huntest --defines` before compiling a copied template.

## License

Huntest is distributed under the [MIT License](LICENSE).
