Below is an overview of the source code for the final project of the *Advanced Programming Languages for A.I.* course [H02A8A]. Every procedure is preceded by documentation.

To run benchmarks for any problem, the benchmark procedure can be used (call 'benchmark(true)', or 'benchmark(false)' if no intermediate results should be printed out).

Any files in folders named 'misc' are not discussed in the report. They contain experimental code and results of experiments.

Authors : Michaël Dooreman & Bruno Vandekerkhove

## Sudoku

ECLiPSe source code in /src/sudoku/eclipse/.
CHR source code in /src/sudoku/chr/.

All the ECLiPSe models are in /src/sudoku/eclipse/model/.
The channeling constraints are in combined.pl.
The model with nothing but channeling constraints is in channeling.pl.
The other filenames should be obvious.

All the CHR models are in /src/sudoku/chr/model/.
Not all of the models mentioned in the report were implemented in this language.

The benchmarks and automated benchmarking code is in /src/sudoku/benchmarks.

## Hashiwokakero

ECLiPSe solver in /src/hashiwokakero/eclipse/solver.pl.
CHR solver in /src/hashiwokakero/chr/solver.pl.
CHR solver + experiments with active constraints and bound consistency in /src/hashiwokakero/chr/solver_active.pl.

## Scheduling Meetings

All the code lies in /src/scheduling/scheduling.pl.
The first procedure is the entry point.
The next procedures generate constraints (including implied constraints).
The last procedure automates benchmarking.
