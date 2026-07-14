Huntest - HDL Testbench Launcher
================================

Work in progress.

Running with Nix
----------------

Run Huntest directly from GitHub (with Nix flakes enabled):

```console
nix run github:punzik/huntest
```

Arguments can be passed after `--`, for example:

```console
nix run github:punzik/huntest -- --help
```

To enter a development shell with Huntest and Guile:

```console
nix develop github:punzik/huntest
```

Test result protocol
--------------------

A normal test is `PASS` only when its callback succeeds, it emits no `FAIL#`
line, and its testbench prints a line beginning with `SUCCESS#`. A callback
that otherwise succeeds without that marker is reported as `UNKNOWN`, retains
its work directory, and makes Huntest exit with a non-zero status.

In a `.hut` callback, emit the marker with:

```scheme
(hut::println 'success "Testbench completed")
```

For Verilog, generate `huntest.vh` with `huntest --defines` and emit the
marker before `$finish`:

```systemverilog
`log_success(("Testbench completed"));
$finish;
```

`#:expect-fail #t` tests remain successful when they emit the expected
`FAIL#` marker; they do not require `SUCCESS#`.

Icarus module parameters
------------------------

`iverilog::test-body-simple` passes `#:parameters` to Icarus as root-module
`-P` overrides. Values are Verilog expressions; Huntest handles shell quoting.
Do not manually escape a single quote in a numeric HDL literal:

```scheme
#:parameters '(("VALUE" "8'h12"))
```

For a Verilog string literal, include its double quotes in the Scheme string:

```scheme
#:parameters '(("MESSAGE" "\"hello world\""))
```

```
Usage: huntest [OPTION]... [PLUSARGS]
Run testbenches

Options:
  -Q, --query <QUERY>  Regexp query string.
  -k, --keep           Do not delete work directory if test is pass.
  -i, --incremental    Do not delete existing work directory in static mode
  -s, --static         Use static work dir for initial debug purposes.
                       This option also enable keep option.
  -w, --work <PATH>    Work path. Default: base path of the script.
  -c, --color          Colorize output.
  -n, --nopar          Sequential execution.
  -C, --clean          Delete work folders.
  -f, --defines        Print useful Verilog defines.
  -r, --recursive      Recursive search for script files.
  -x, --regex <REGEX>  Regular expression for searching script files. Default: '\.hut$'
  -l, --list           List testbenches. Nothing is executed.
  -a, --list-all       List testbenches. Ignore query and defer.
  -v, --verbose        Verbose output.
  -q, --quiet          Quiet output.
  -V, --version        Print version.
  -h, --help           Print this message and exit.

PLUSARGS:
  A plussarg is any argument beginning with +.
  For example: +arg, +var=val, +define+MACRO=123

QUERY:
  QUERY is a string like a 'testbench-regexp::test-regexp'.
  Query used to filter testbenches and tests by their names.
  Each part of the query is a regular expression or an empty string.

  For example:
    '::'                   -> select all tests in all testbenches.
    '^tb0$::'              -> select testbenches with name exactly 'tb0'.
    'testbench::test [13]' -> select testbenches with name contains 'testbench'
                              and tests with name contains 'test 1' or 'test 3'.
    '::test 1'             -> setect tests with name contains 'test 1'.
    'testbench 1'          -> select testbenches witn name contains 'testbench 1'.

Source code and issue tracker: <https://github.com/punzik/huntest>
```
