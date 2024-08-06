Huntest - HDL Testbench Launcher
================================

Work in progress.

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
