# Huntest public API

This document describes every exported binding in Huntest **0.3.0**.  Import
modules with a prefix to keep names from colliding with application code:

```scheme
(import (prefix (huntest) hut::)
        (prefix (huntest iverilog) iverilog::)
        (prefix (huntest verilator) verilator::)
        (prefix (huntest sby) sby::))
```

The command-line workflow, result protocol, Nix usage, and runnable examples
are documented in the [README](../README.md).  Internal record constructors,
status accessors, and scheduling functions are intentionally not part of this
API.

## Conventions

### Truth values and process status

Callbacks must return a truthy Scheme value to succeed.  Process helpers return
the status from Guile's `close-pipe`; compare it with zero:

```scheme
(zero? (hut::system% "tool --check"))
```

### Paths

`base-path`, `tb-path`, and `test-path` callback arguments are path procedures.
Calling one without arguments returns its absolute directory; calling it with a
relative string resolves that string below the directory.

```scheme
(base-path)                ; directory containing the .hut file
(base-path "rtl/dut.sv")   ; absolute source path
(test-path "wave.fst")     ; private artifact path for one test
```

### Tagged output

The supported symbols for `hut::println` are `info`, `warn`, `success`, and
`fail`, producing `INFO#`, `WARN#`, `SUCCESS#`, and `FAIL#` lines respectively.
A normal test requires `SUCCESS#`; see [result protocol](../README.md#test-result-protocol).

## Module `(huntest)`

### Defining and running testbenches

#### `hut::make-test`

```scheme
(hut::make-test
 #:name name
 #:expect-fail expect-fail?
 #:body body)
```

Creates an opaque test record.

| Keyword | Default | Description |
| --- | --- | --- |
| `#:name` | `""` | Display and query name. Empty or whitespace-only names become `"unnamed"`. |
| `#:expect-fail` | `#f` | Require a truthy body and at least one `FAIL#` line. |
| `#:body` | `(lambda args #f)` | Callback with signature described below. |

Test callback signature:

```scheme
(lambda (plusargs base-path tb-path test-path) ...)
```

`plusargs` is a list of command-line strings that retain their leading `+`.
The body should use `test-path` for generated artifacts.

#### `hut::make-testbench`

```scheme
(hut::make-testbench
 #:name name
 #:help help
 #:defer defer?
 #:init init
 #:tests tests
 #:finish finish)
```

Creates an opaque testbench record.  One test or a list of tests is accepted by
`#:tests`; duplicate test names are made unique by appending a decimal suffix.

| Keyword | Default | Description |
| --- | --- | --- |
| `#:name` | `""` | Display and query name. Empty names become `"unnamed"`. |
| `#:help` | `#f` | String or list of strings shown by list commands. |
| `#:defer` | `#f` | Exclude from an unqualified run; select it with `-Q` or list it with `-a`. |
| `#:init` | truthy no-op | Callback before this testbench's tests. |
| `#:tests` | `()` | A test record or a list of test records. |
| `#:finish` | truthy no-op | Callback after reached test execution. |

Init and finish callback signatures:

```scheme
(lambda (plusargs base-path tb-path) ...)
```

A testbench passes only if init, finish, and every selected test pass.

#### `hut::run`

```scheme
(hut::run testbench-or-list ...)
```

Accepts testbench records and arbitrarily nested lists of testbench records.
Use it as the final expression of a `.hut` script.

When a `.hut` file is loaded by the `huntest` command, `run` returns its
flattened testbench records to the launcher.  When the `.hut` file itself is
executed as a Guile script, `run` parses the script command line, runs the
selected tests, prints a summary, and exits the process.

#### `hut::main`

```scheme
(hut::main command-line)
```

Command-line entry point used by the installed `huntest` executable.
`command-line` has the same shape as Guile's `(command-line)`: program name
followed by options, paths, and plusargs.  It discovers `.hut` files, loads
them, runs selected tests, and exits with the aggregate status.  Application
code normally calls `hut::run`, not `hut::main`.

### Output and command helpers

#### `hut::println`

```scheme
(hut::println tag-or-format arg ...)
```

If the first argument is one of `info`, `warn`, `success`, or `fail`, formats
`arg ...` and prefixes every output line with the corresponding tag.  Otherwise
the first argument is used as a `format` string and a newline is appended.

```scheme
(hut::println 'info "seed: ~a" 17)
(hut::println 'success "completed")
(hut::println "ordinary message: ~a" value)
```

An unknown tag raises an exception.

#### `hut::system%`

```scheme
(hut::system% command #:base base)
```

Runs `command` through a shell, prints a `RUN:` line followed by combined
stdout/stderr, and returns the process status.  If `base` is a string, the
command is executed after changing to that directory.

This helper is for trusted, intentionally shell-oriented commands.  Do not
interpolate untrusted paths, plusargs, or values into `command`.

#### `hut::system%-capture`

```scheme
(hut::system%-capture command #:base base #:stderr stderr?)
```

Shell variant of `system%` that returns two values:

```scheme
(let-values (((status output)
              (hut::system%-capture "tool --check" #:base (test-path))))
  ...)
```

It still prints the `RUN:` line.  `stderr?` defaults to `#t` and combines
stderr into `output`; set it to `#f` to leave stderr separate.

#### `hut::system%-capture-argv`

```scheme
(hut::system%-capture-argv command args #:base base)
```

Runs one program with an argv list and returns `(values status output)`.  Every
item in `args`, along with `command` and `base` when supplied, must be a string;
otherwise an exception is raised.  Arguments are passed positionally and are
not parsed as shell code.  Stdout and stderr are combined in `output`.

```scheme
(let-values (((status output)
              (hut::system%-capture-argv
               "iverilog"
               (list "-g2012" "-Ptop.VALUE=8'h12" "tb.sv")
               #:base (test-path))))
  (and (zero? status) output))
```

The implementation uses a fixed shell trampoline only to set the child working
directory and redirect stderr; dynamic arguments remain positional.  Prefer
this procedure over `system%-capture` when invoking a fixed executable.

### Paths, files, and strings

#### `hut::append-path`

```scheme
(hut::append-path path item-or-items)
```

Appends `path`, a path separator, and either one string or every string in a
list.  It performs simple concatenation; it does not preserve absolute paths or
normalize `..` components.

```scheme
(hut::append-path "/work" "result.json")
; => "/work/result.json"
(hut::append-path "/work" '("a" "b"))
; => '("/work/a" "/work/b")
```

#### `hut::list-dir`

```scheme
(hut::list-dir path)
```

Returns full paths of immediate directory entries, excluding `.` and `..`.
Returns `()` when `path` does not exist.

#### `hut::find-files`

```scheme
(hut::find-files predicate base #:recursive recursive?)
```

Visits entries below `base` and returns paths for which `predicate` is truthy.
The predicate receives `(path type)`, where `type` is the symbol returned by
Guile's `stat:type`, such as `regular` or `directory`.  With `#:recursive #t`,
subdirectories are traversed too.

```scheme
(import (srfi srfi-13))

(hut::find-files
 (lambda (path type)
   (and (eq? type 'regular) (string-suffix? ".hut" path)))
 "."
 #:recursive #t)
```

#### `hut::delete-recursive`

```scheme
(hut::delete-recursive path)
```

Canonicalizes `path` and recursively deletes a file or directory tree.  This is
destructive and raises filesystem errors to the caller.

#### `hut::project-root`

```scheme
(hut::project-root path)
```

Returns the nearest ancestor of `path` that contains a `.git` entry.  Returns
`"/"` if none is found.

#### `hut::string->filename`

```scheme
(hut::string->filename string #:optional max-len)
```

Converts arbitrary text to a lowercase filename-safe fragment.  Non-alphanumeric
characters become `_` (with a few readable substitutions such as `<` to `lt`),
and the result is limited to `max-len`, which defaults to 32.

#### `hut::verilog-string-literal`

```scheme
(hut::verilog-string-literal string)
```

Returns a double-quoted Verilog string literal after escaping `\` and `"`.
This is useful when building a `-DNAME=value` argument:

```scheme
(string-append "-DROOT=" (hut::verilog-string-literal (base-path)))
```

### Lists, combinations, and plusargs

#### `hut::find-plusarg`

```scheme
(hut::find-plusarg plusargs prefix)
```

Searches a leading-`+` plusarg list for the first item whose text after `+`
starts with `prefix`.  An empty prefix matches only the bare `"+"` argument.
Returns the original plusarg string or `#f`.

```scheme
(hut::find-plusarg '("+seed=17" "+dump") "seed=")
; => "+seed=17"
```

#### `hut::list-add-separator`

```scheme
(hut::list-add-separator separator list)
```

Returns `list` with `separator` inserted between adjacent elements.

#### `hut::list-flat`

```scheme
(hut::list-flat item ...)
```

Recursively flattens nested pairs, drops empty lists, and returns a list of
leaves.

#### `hut::transpose`

```scheme
(hut::transpose matrix)
```

Transposes a rectangular list of lists.  The rows must have compatible lengths.

#### `hut::combinations`

```scheme
(hut::combinations list ...)
```

Returns the Cartesian product of its input lists.  Each result is a list when
more than one input list is supplied.

```scheme
(hut::combinations '(8 16) '("fast" "slow"))
; => '((8 "fast") (16 "fast") (8 "slow") (16 "slow"))
```

#### `hut::map-combinations`

```scheme
(hut::map-combinations procedure list ...)
```

Applies `procedure` to every Cartesian combination of the input lists.  This is
convenient for generating testbench matrices.

```scheme
(hut::map-combinations
 (lambda (width mode)
   (string-append "w" (number->string width) "-" mode))
 '(8 16)
 '("fast" "slow"))
```

#### `hut::sort-uniq`

```scheme
(hut::sort-uniq items less?)
```

Sorts `items` with strict ordering predicate `less?` and removes equivalent
adjacent values according to that ordering.

#### `hut::string-append*`

```scheme
(hut::string-append* string-or-list ...)
```

Flattens nested string lists and concatenates all leaves without a separator.

#### `hut::string-append-sep*`

```scheme
(hut::string-append-sep* separator string-or-list ...)
```

Flattens nested string lists and concatenates leaves with `separator` between
them.

## Module `(huntest iverilog)`

### `iverilog::test-body-simple`

```scheme
(iverilog::test-body-simple
 #:sources sources
 #:top top
 #:compile-flags compile-flags
 #:runtime-flags runtime-flags
 #:include-paths include-paths
 #:parameters parameters
 #:defines defines
 #:init init
 #:finish finish
 #:fail-on-warnings? fail-on-warnings?)
```

Returns a normal Huntest test body.  The body runs Icarus compilation and VVP
simulation in the test's private directory.  It automatically adds the `.hut`
directory to include paths and defines `HUNTEST_TESTBENCH`,
`HUNTEST_BASE_DIR`, and `HUNTEST_TB_DIR`.

Conceptually, it runs:

```text
iverilog -o <top>.vvp -s <top> ...
vvp ... <test-path>/<top>.vvp ... <plusargs>
```

| Keyword | Default | Description |
| --- | --- | --- |
| `#:sources` | required | List of source paths, or a test callback returning such a list. Relative paths resolve below the `.hut` file. |
| `#:top` | required | Top module and generated `.vvp` basename. |
| `#:compile-flags` | `()` | Individual argv entries for `iverilog`. |
| `#:runtime-flags` | `()` | Individual argv entries for `vvp`. Flags such as `-fst` are placed after the `.vvp` argument as required by VVP. |
| `#:include-paths` | `()` | Include paths relative to the `.hut` file. |
| `#:parameters` | `()` | `(name value)` pairs converted to `-Ptop.name=value`. Values are Verilog expressions. |
| `#:defines` | `()` | Bare define strings or `(name value)` pairs converted to `-D` arguments. |
| `#:init`, `#:finish` | truthy no-op | Per-test callbacks with the normal test signature. |
| `#:fail-on-warnings?` | `#f` | Fail if Icarus or VVP output contains case-insensitive `warning:`. |

Every flag, source, include path, define, and parameter becomes one argv item;
do not combine several command-line arguments in one string.  Numeric literals
such as `"8'h12"` need no shell escaping.  A Verilog string parameter must
include its own double quotes, for example `"\"hello\""`.

## Module `(huntest verilator)`

### `verilator::test-body-simple`

```scheme
(verilator::test-body-simple
 #:sources sources
 #:top top
 #:compile-flags compile-flags
 #:runtime-flags runtime-flags
 #:include-paths include-paths
 #:parameters parameters
 #:defines defines
 #:timing? timing?
 #:build-jobs build-jobs
 #:init init
 #:finish finish
 #:fail-on-warnings? fail-on-warnings?)
```

Returns a normal Huntest test body for a self-contained SystemVerilog
testbench.  It uses Verilator's generated main program; C++ harness workflows
are outside the scope of this simple builder.

Each test receives a private object directory and a deterministic executable:

```text
verilator --binary --timing --top-module <top> \
  --Mdir <test-path>/obj_dir -o simulation ...
<test-path>/obj_dir/simulation <runtime-flags> <plusargs>
```

| Keyword | Default | Description |
| --- | --- | --- |
| `#:sources` | required | List of paths or a test callback returning one; relative paths resolve below the `.hut` file. |
| `#:top` | required | Verilator top module. |
| `#:compile-flags` | `()` | Individual argv entries for Verilator. |
| `#:runtime-flags` | `()` | Individual argv entries for the generated executable. |
| `#:include-paths` | `()` | Include paths relative to the `.hut` file. |
| `#:parameters` | `()` | `(name value)` pairs converted to top-level `-Gname=value`. |
| `#:defines` | `()` | Bare define strings or `(name value)` pairs converted to `-D` arguments. |
| `#:timing?` | `#t` | Use `--timing`; `#f` uses `--no-timing`. |
| `#:build-jobs` | `1` | Value passed to `--build-jobs`; keep it low when Huntest runs tests in parallel. |
| `#:init`, `#:finish` | truthy no-op | Per-test callbacks with the normal test signature. |
| `#:fail-on-warnings?` | `#f` | Fail on `%Warning-...` or `warning:` diagnostics from Verilator, its build, or the executable. |

The adapter automatically defines the same `HUNTEST_*` macros as the Icarus
adapter.  Verilator often makes warnings fatal itself; the explicit policy is
useful with `-Wno-fatal`.

## Module `(huntest sby)`

### `sby::test-body-simple`

```scheme
(sby::test-body-simple
 #:sources sources
 #:top top
 #:parameters parameters
 #:defines defines
 #:mode mode
 #:depth depth
 #:append append
 #:engine engine
 #:frontend frontend
 #:init init
 #:finish finish
 #:strip-output strip-output?)
```

Returns a normal Huntest test body.  It writes `<top>.sby` into the private
test directory, invokes `sby -f <top>.sby`, and returns truthy only when SBY
exits zero and the finish callback is truthy.

| Keyword | Default | Description |
| --- | --- | --- |
| `#:sources` | required | List of source paths or a test callback returning one. |
| `#:top` | required | Formal top module. |
| `#:parameters`, `#:defines` | `()` | Values used to generate the SBY/Yosys script. |
| `#:mode` | `'bmc` | `'bmc`, `'prove`, or `'cover`. |
| `#:depth` | `20` | Formal depth. |
| `#:append` | `0` | Extra depth appended by this builder. |
| `#:engine` | `'yices` | `'yices` or `'boolector`. |
| `#:frontend` | `'yosys-verilog` | `'yosys-verilog`, `'yosys-systemverilog`, `'verific`, or `'synlig`. |
| `#:init`, `#:finish` | truthy no-op | Per-test callbacks with the normal test signature. |
| `#:strip-output` | `#f` | Remove the `.hut` base directory prefix when printing SBY output. |

The normal Huntest result protocol still applies.  A typical finish callback
emits `SUCCESS#` after the SBY invocation; a non-zero SBY exit remains a test
failure.

### `sby::make-sby-config`

```scheme
(sby::make-sby-config
 #:sources sources
 #:top top
 #:parameters parameters
 #:defines defines
 #:mode mode
 #:depth depth
 #:append append
 #:engine engine
 #:frontend frontend)
```

Returns a list of strings containing an SBY configuration.  It does not write
a file or execute SBY.  This is useful for applications that need to inspect or
customize a generated configuration before writing it.

| Keyword | Default | Description |
| --- | --- | --- |
| `#:sources` | required | Source paths for `[files]` and the generated read commands. |
| `#:top` | required | Top module passed to `prep -top`. |
| `#:parameters`, `#:defines` | `()` | Parameter and define values for the generated script. |
| `#:mode` | `'bmc` | `'bmc`, `'prove`, or `'cover`. Invalid values raise an exception. |
| `#:depth` | `20` | Generated `depth` value. |
| `#:append` | `20` | Generated `append` value; zero omits the line. |
| `#:engine` | `'yices` | `'yices` or `'boolector`; invalid values raise an exception. |
| `#:frontend` | `'yosys-verilog` | Selects the corresponding Yosys, Verific, or Synlig read commands. |

## API compatibility

The documented procedures and keywords above are the public API for 0.3.0.
Keep `.hut` scripts on these exported bindings; names not listed here are
internal implementation details and may change without notice.
