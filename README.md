# Overview

Repository for constraint programming assignments for the *Advanced Programming Languages for A.I.* course [H02A8A].

[The assignment can be found here.](/docs/Assignment.pdf)

## Sources

**Books**

- [Constraint Logic Programming using Eclipse (Wallace)](/books/Constraint%20Logic%20Programming%20using%20Eclipse%20(Wallace).pdf).
- [Constraint Handling Rules (Thom Fruhwirth)](/books/Constraint%20Handling%20Rules%20(Thom%20Fruhwirth).pdf).
- [Handbook of Constraint Programming (book chapter)](/books/Handbook%20of%20Constraint%20Programming.pdf).

**Slides**

- [Introduction](/slides/Introduction.pdf).
- [Passive Constraints](/slides/Passive.pdf).
- [Active Constraints](/slides/Active.pdf).
- [Optimization with Active Constraints](/slides/Optimization.pdf).
- [Other CP Systems](/slides/Other.pdf).
- [Constraints on Reals](/slides/Reals.pdf).
- [Constraint Handling Rules](/slides/CHR.pdf).

**Research**

 - [LibGen](http://libgen.io)
 - [LIMO](http://limo.libis.be)
 - [Google Scholar](http://scholar.google.com)

## ECLiPSe

 - [Download](http://eclipseclp.org/download.html)
 - [Tutorial](http://eclipseclp.org/doc/tutorial/tutorial.html)

## Constraint Handling Rules

**CHR rules**

 - Simplification (removes constraints, adds new ones to constraint store)
 - Propagation (similar to simplification, doesn't remove constraints)
 - Simpagation (combines simplification and propagation)
 
 [Consult WikiPedia](https://en.wikipedia.org/wiki/Constraint_Handling_Rules)
 
 # Report
 
 ## Sudoku

Sudoku is a well-known puzzle game which needs no introduction. It is
typically modelled as a constraint satisfaction problem through the use
of `all_different` constraints on rows, columns and blocks. Such global
inequalities tend to improve upon the use of binary inequalities. The
constraint generating code[^1] is fairly trivial and needn't be detailed
here.

There are several other ways one could model Sudoku. The widely cited
study by Helmut Simonis and subsequent studies
provide some ideas. Four *'dual'* models, two approaches based on a
boolean characterisation, a combination of models provided by Laburthe,
a model enforcing the singular occurrence of every value in every row,
column and block, as well as a model with nothing but channeling
constraints were considered[^2]. Tests were run on the provided
puzzles[^3] as well as some minimum puzzles provided by Gordon
Royle[^4]. These are puzzles with a minimal amount of pre-filled cells
(17 to be precise), which does not mean that they are
harder to solve.

The dual models hold a $N x N$ array with all the decision
variables. Whereas in the classic viewpoint the rows, columns and values
of this array correspond to those of the input puzzle, every one of the
four dual models changes their roles. The first two switch the role of
rows or columns with those of values. In the third model every row and
column of the array corresponds to a block and a position. In the fourth
dual model every row represents a block, every column a value and every
value a position within a block. For each of them it was harder to
implement the necessary constraints, usually necessitating the use of
auxiliary variables together with appropriate channeling constraints.\

In one of his works Laburthe discusses various rules that can be used to
resolve Sudoku puzzles, after which he details three models that he
associates with the rules. He ends up proposing a
model for every level of difficulty of the input puzzle. An attempt was
made at implementing his recommendation for 'difficult' puzzles. It
decreased the average number of backtracks but increased the runtime.\

The boolean models include the natural combined model
and a more intuitive characterisation resembling an integer programming or a SAT model (using [`occurrences/3`](http://eclipseclp.org/doc/bips/lib/ic_global/occurrences-3.html) instead of sums, disjunctions and conjunctions). Both of them have $N x N x N$ boolean variables $b_{rcv}$ which are true if the cell at row $r$ and column $c$ holds the value $v$. The natural combined model was cumbersome to implement and performed badly. It was introduced together with an algorithm which was tailored after it, and a constraint for unequality of lists isn't really supported by `ECLiPSe`[^5].

Note that it is usually not recommended to use a boolean model when
integers can be used instead (as pointed out by Rossi).

The last two models were found to be the most performant. The first
makes use of the previously mentioned [`occurrences/3`](http://eclipseclp.org/doc/bips/lib/ic_global/occurrences-3.html) constraint to make every value occur just once in every row, column and block.

The second one generates nothing but channeling constraints. It has been
demonstrated that this can provide good results despite such constraints
being less 'tight' than `all_different` constraints[^6]. When Dotú
discussed it he was considering QuasiGroups. This was
extended[^7] to Sudoku puzzles by making use of three instead of two
dual models (since blocks need to be considered as well). The variant in
which channeling constraints between all models (one primal, three dual)
are generated performed better than the one in which only channeling
constraints between the primal and every dual model are applied. These
variants are analogous to what Dotú referred to as *'trichanneling'* and
*'bichanneling'*.

**Experiments**

Number of backtracks and running time for most of the models are
displayed in table 1. Removal of *'big'* (`all_different`) constraints
in the classic model[^8] led to an increase in runtime which
corroborates Demoen's experiences.

<p align="center">
<img src="https://github.com/BrunoVandekerkhove/constraint-programming/blob/master/img/missing6.png?raw=true">
</p>

An interesting observation is that the two most performant models also
have the same number of backtracks. One of them is the model with
nothing but channeling constraints, the other uses the
[`occurrences/3`](http://eclipseclp.org/doc/bips/lib/ic_global/occurrences-3.html)
constraint which enforces arc-consistency. The first will detect when
for a given *row-column*, *row-value*, *column-value* or *block-value*
combination only one possible *value*, *column*, *row* or *position*
remains. It will remove this value, column, \... from the domains of the
other primal or dual variables[^9]. The second can do the same ; it can
propagate unequalities when the domain of a variable is reduced to a
singleton but also knows when a value can be put in only one particular
cell of a row, column or block. It is probably slower because the
constraint itself is more generic than reification constraints.

A model combining the classic viewpoint with the fourth dual model was
set up. Number of backtracks and runtimes are displayed in table 3.

<p align="center">
<img src="https://github.com/BrunoVandekerkhove/constraint-programming/blob/master/img/snippet1.png?raw=true">
</p>

The `first_fail` heuristic generally outperforms `input_order`. It
considers variables with the smallest domain first, rather than
considering variables in the order they were given. This increases
pruning power as removing a value from the domain will remove a bigger
part of the search tree because the branching count increases as you go
down the tree (which is not necessarily the case when using
`input_order`). This tends to cause the number of backtracks to
decrease. Unless the solution lies at the left side of the search tree
generated by the `input_order` heuristic performance will increase in
comparison.

As for the `all_different` constraints, more constraint propagation can
be done when making use of `ic_global` since it enforces bound
consistency rather than enforcing arc-consistency on
the corresponding inequalities. This decreases the number of backtracks
for every puzzle but may increase runtime for some of them (such as
*sudowiki\_nb49*) because the propagation takes time. The global
constraints do tend to perform a little better on average (about 11.2
versus 11.5 seconds total runtime).

Combining two viewpoints adds redundancy, leading to more propagation.
The number of backtracks ends up decreasing because of this while the
runtime may not necessarily decrease due to the larger number of
constraints that have to be dealt with. As seen in table 3 the
runtimes of the combined model generally laid somewhere in between those
of the two original viewpoints.

**CHR**

Some of the viewpoints considered previously were implemented in `CHR`.
The runtimes are shown in table 4.
Initially the `first_fail` variable heuristic and the `indomain_min`
value heuristic were used. These are easy to implement and generally
perform quite nicely. Based on previous experiments with `ECLiPSe` it
was concluded that (at least for the given benchmarks) `indomain_max`
would be a better choice for the classic model[^10]. It cut runtime by
about half, yet increased the runtime of the `dual4` model.

Because the channeling-only viewpoint performed best in the `ECLiPSe`
experiments an attempt was made at implementing it in `CHR`. The
approach that was used led to complicated code and - ironically - the
slowest runtimes seen yet. After a related failed experiment[^11] the
focus was laid on keeping the code more simple and adding redundant
constraints. A few of the rules discussed by Laburthe were tested out. 
The *x-wing* rule was of no use, but adding a *single-position* rule 
decreased the runtime.

After this experiment a second attempt was made at implementing some
sort of channeling-only model, or at least a model which would detect
when values can only be put in one cell in a given row, column or block.
By applying all the *single-position* rules, basically. While technically 
speaking no channeling is done in the implementation of this experimental model, 
the number of backtracks decreased starkly[^12] and the total runtime decreased 
in comparison with the classic model.

In example code Gonzalez & Christiansen use a 4-coordinate approach
($a\times b\times c\times d$ with $a\times b$ designating a block, $c$ a row - and $d$ a column within that block). This looked like an elegant trick to try out as it makes it unnecessary to convert from *row-column* combinations to block numbers. A form of pre-processing which did decrease runtime (for the classic model) by a few seconds. Applying it in the experimental model reduced total runtime to about 4 seconds.

As in the previous section, two of the `CHR` viewpoints were combined in
one single model. For every puzzle it generally performed at least as
well as the worst of the two other models - with a few exceptions.
Sometimes it performed better than both, because the propagation
achieved by the separate models and reduction in backtracks quickened
the search enough to compensate for the increased complexity of the
model. Total runtime clearly is highest in the dual model, lower in the
classic model, even lower in the combined model and lowest in the
experimental model discussed previously.

<p align="center">
<img src="https://github.com/BrunoVandekerkhove/constraint-programming/blob/master/img/sudokutables.png?raw=true">
</p>

<p align="center">
<img src="https://github.com/BrunoVandekerkhove/constraint-programming/blob/master/img/table4.png?raw=true">
</p>

## Hashiwokakero

Hashiwokakero is another Japanese logic puzzle published by the same
company, in which islands have to be connected by bridges. Six
constraints are to be respected, the last one being the connectedness
constraint, i.e. that all islands have to be connected. What follows is
a discussion of an implementation of two solvers of Hashiwokakero
puzzles. One written in ECLiPSe, the other in `CHR`.

**ECLiPSe implementation**

A partial solution by Joachim Schimpf was provided. It did not enforce
the connectedness constraint. Joachim defines four variables for each of
the input puzzle's cells. They represent the number of bridges for each
of the cell's directions (north, east, south, west). Then he enforces
the five first constraints :

1.  Bridges run in one straight line, horizontally or vertically. This
    is enforced with equality constraints, making sure that the number
    of bridges for a given direction of a given cell equals the number
    of bridges in the opposite direction of a neighbouring cell. A total
    of four equality constraints for every cell except those on the
    border, which may only have two or three neighbours[^13].

2.  Bridges cannot cross other bridges or islands. This is enforced by
    making sure that any cell that does not represent an island either
    has no horizontal or no vertical bridges.

3.  At most two bridges connect a pair of islands. Joachim imposes this
    constraint by declaring the domains of the variables to be
    $[0\dots 2]$.

4.  The number of bridges connected to an island must match the number
    $X$ on that island. A simple sum constraint ($N+E+S+W$ \#= $X$)
    suffices to enforce this one.

The connectedness constraint was enforced through the use of an
analogous set of four variables ($FN,FE,FS,FW$) per cell, denoting the
*flow* for each of the cell's directions. Say the island at the upper
left is said to be the sink, then if a flow can be assigned to all
islands such that the sink's incoming flow equals the total number of
islands minus one, the islands are sure to be connected. The net flow
for each non-sink island needs to be one, for each cell it should be
zero, and empty cells should have no flow. Most of these constraints can
be implemented with equality constraints (the `ic` library enforces
bound consistency for these), some of the others were implemented with
the use of the [$\Rightarrow$](http://eclipseclp.org/doc/bips/lib/ic/EG-2.html) (*'implication'*) constraint.

<p align="center">
<img src="https://github.com/BrunoVandekerkhove/constraint-programming/blob/master/img/snippet2.png?raw=true">
</p>

All of these constraints are active, meaning that when variables are, in
a sense, 'woken up', the domain of associated variables is updated
accordingly.

The provided benchmarks are solved rather quickly by the solver. It
generally takes a few milliseconds (and zero backtracks), even for the
biggest board. If one makes use of the `most_constrained` or the
`occurrence` variable heuristic, runtimes increase. The `largest` or
`smallest` heuristics perform even worse. This is due to the fact that
these heuristics are more likely to label flow variables. Flow variables
have larger domains and most of the values in their domain cannot
partake in a solution. As a result, backtracks increase and runtimes do
too.

Since the solver does not need any backtracks to solve the benchmark
problems, implied constraint could only improve the constraint
propagation. Two implied constraints were considered, the first one
being 'a non-sink island with only one bridge, has a flow of only one in
the direction of that bridge' and the second one similar to it, but in
the case of a non-sink island with two bridges in the same direction.
When both constraints are used, the runtime increases. If only one is
used, the runtime decreases with a significant amount. The first
constraint performs best, decreasing the benchmark runtime by 20% on
average.

**CHR implementation**

A `CHR` solver was also created. Because of the results of the previous
experiments no special heuristic (such as `occurrence` or
`most_constrained`) was made use of. The solver generates `island/7` and
`cell/4` constraints which associates islands and cells with their
variables (representing the number of bridges in a given direction) and
their number (in the case of islands). Every island has a corresponding
`sum/3` constraint which gets updated every time any of the variables in
its list is assigned. This corresponds to forward checking. The
variables represent the number of bridges for a given direction and a
given cell (or island). Instead of defining all these variables
separately and enforcing $V_1=V_2$ equality constraints whenever they're
supposed to be equal, shared variables are defined instead. This is done
in the pre-processing step when the board is read. Because of this
decision all but the second and the fifth constraint have to be
enforced. The sum constraint was just described. The way the second
constraint is dealt with is shown in code snippet [\[hashi2\]](#hashi2){reference-type="ref" reference="hashi2"}. As can be seen, the `in/2` constraint (and operator) is used to update domains.

<p align="center">
<img src="https://github.com/BrunoVandekerkhove/constraint-programming/blob/master/img/snippet3.png?raw=true">
</p>

Two additional constraints were added to speed up the solver. The first
is a generalised version of the *'4 in the corner, 6 on the side and 8
in the middle'* technique. If an island's number equals twice its number
of neighbours then it should be connected with each of these neighbours
by a pair of bridges. This covers some of the special cases as
*'neighbour'* is defined more broadly as *'any island to which the
island can still be connected with at a certain point'*[^14]. As
expected, it sped up the solver for all puzzles, almost cutting the
total runtime by half.

The second constraint prevents isolation of some islands by stating that
any two neighbouring islands, both with the number one or two, cannot be
connected by that same number of bridges[^15]. Only in the sixth puzzle
did this speed up the search by about half a second due to the reduction
in number of backtracks. Interestingly the constraint slows down puzzle
two, as enforcing it changes the variable ordering leading to a stark
increase in number of backtracks[^16].

Some redundant constraints were already part of the basic solver. For
example, if an island only has one neighbour, its sum constraint only
contains one variable which can readily be assigned.

As for the connectedness constraints, both passive - and active versions
were implemented. Because any initial solution generated without
enforcing connectedness still tends to be connected anyways[^17], any
constraint propagation relating to flow tends to slow down the search
procedure. Because of this the passive versions of the connectedness
constraints outperform the active versions (having a total runtime of
about 6 seconds for all six puzzles).

The passive checks can intuitively be understood as follows ; all paths
from the sink to all corners of the board are tracked until all
connected islands have been visited. If at that point any islands remain
that haven't been visited, then the board is not connected.

As far as the implementation goes, a `connects/5` constraint is added
for any two islands that *can* be but aren't necessarily connected. This
is done in the pre-processing step, where all the variables are defined.
When at any point during the search procedure the number of bridges
between islands is determined, then the corresponding constraint is
replaced by a `connected/4` constraint which says that the islands *are*
connected. At the end of the search procedure the `connected/4`
constraints are used to traverse all paths starting from the sink, and
if any such connection remains that couldn't be reached from the sink,
failure is reported.

In the case of puzzle six no solution could be found in a reasonable
amount of time when the initial versions of the active connectedness
constraints were used. The domains of the flow variables are quite large
because the number of islands equals 140. While for most of these
islands the flow can easily and uniquely be determined, 35 flow sum
constraints are less trivial. Generally speaking, when a bunch of
islands are connected in a circle (meaning that there is no *'tail'*,
i.e. every island is connected to at least 2 other islands) the solution
isn't unique. It's what happens in puzzle six, where after cutting all
the *'tails'* a few circles remain.

```
              5 = = 4 = = 4 - - 3 = = 3
              |           |           |
              3 = 3 - 3 = 5           |
                                      3
                                      X
                                      X
          5 - - 2                     3
          X     |                     |
          5 = = 6 - - 6               |
                      X               2
                      X               |
                      6               |
                      X               3
            3 - 4     X               X
            X   X     X               X
            6 = 6 = = 7               4
                      |               X
4 - 4                 |               4
X   |                 |               X
4 = 5                 4 = 3 - 5 = 6 = 4
```

If the sum constraints relating to flow enforce bound consistency (as is
the case in ECLiPSe), wrong values can more quickly be filtered out.
Realizing this, a limited form of bound consistency was implemented. A
correct solution was generated much more quickly as a result as
non-connectedness of a given solution could be detected earlier on.
Eventually the search procedure still got stuck on puzzle six, looking
for a valid way to label flow variables. After taking it up a notch (by
improving the bound consistency logic) the solver managed to find a
solution to that puzzle in 8 seconds. Enforcing bound consistency for
the other sum constraints as well reduced total runtime to about 5
seconds (all six puzzles).

## Scheduling Meetings

The last challenge is the scheduling of some meetings, taking into
account the preferences of the various persons involved. A constraint
optimization problem where the cost is a function of the end time of the
last meeting and the number of '*violations*' (people of lower rank
having their meeting after that of people of higher rank). This number
of violations is of secondary importance.

Weekend constraints are generated first. If a person doesn't want to
meet on weekends then his or her meeting is not allowed to overlap with
the first weekend that follows :
$$((S + \textit{StartingDay})\ \texttt{mod}\ 7) + D <  5$$ In the above constraint $S$ and $D$ represent the start and duration of the person's meeting. Making direct use of [`mod/3`](https://www.eclipseclp.org/doc/bips/kernel/arithmetic/mod-3.html) leads to an instantiation error, necessitating the use of an auxiliary variable representing the result of the modulo operation.

<p align="center">
<img src="https://github.com/BrunoVandekerkhove/constraint-programming/blob/master/img/snippet4.png?raw=true">
</p>

Precedence constraints and constraints assuring that no meetings overlap
are generated last. The corresponding code is fairly trivial[^18]. The
fact that the meeting with the minister should come last is equivalent
to adding $N-1$ precedence constraints with $N$ the total number of
persons.

The cost function is defined as $(V_{max}\times E)+V$ where $V_{max}$ is
the maximum number of rank violations, $E$ is the end time of the
meeting with the minister and $V$ is the actual number of rank
violations for a given solution. This ensures that whenever two
solutions have a different $E$, the solution with the smallest $E$ will
have the lowest cost (whatever the number of violations $V$). Yet if two
solutions have the same end time $E$, then it's the number of violations
$V$ that will determine what solution is best.

<p align="center">
<img src="https://github.com/BrunoVandekerkhove/constraint-programming/blob/master/img/snippet5.png?raw=true">
</p>

An additional constraint was used for the cost function, stating that it
cannot be smaller than $V_{max}\times D_{tot}$ with $D_{tot}$ the sum of
all meeting durations. This makes a difference[^19].

Some implied constraints were added to increase performance. In case two
persons have a different rank but the same meeting duration and weekend
preferences, a corresponding order on their start times can safely be
imposed. This mustn't override the precedence constraints.

Table 5 shows the runtime for each benchmark. Two
versions are considered ; one ensures that no two meetings overlap by
imposing a ($S_1$+$D_1$ $\leq$ $S_2$ or $S_2$+$D_2$ $\leq$ $S_1$)
constraint for every such pair, the other version uses a global version
of these same constraints provided by the [`ic_edge_finder`](http://eclipseclp.org/doc/bips/lib/ic_edge_finder/index.html) library. It's clear that the global version outperforms the other one. The time it takes to propagate the constraints is usually compensated for by the reduction in nodes having to be considered due to the pruning of the search tree.

Instead of making use of implied constraints one can also tinker with
the various heuristics provided by the [`search/5`](http://eclipseclp.org/doc/bips/lib/ic/search-6.html) procedure. Some of those lend themselves to some benchmarks but not to others.

The `indomain_min` heuristic performed better than `indomain_max` as
solutions with smaller end times are prioritised during backtracking. A
first solution necessarily has the smallest end time. The corresponding
maximum value imposed on the cost function proceeds to prune a large
part of the search tree and only the number of violations remains to be
optimised.

<p align="center">
<img src="https://github.com/BrunoVandekerkhove/constraint-programming/blob/master/img/table5.png?raw=true">
</p>

## Overview of the Code

 `/src/sudoku/`                      `utils.pl`               *Utility functions for Sudoku (`CHR` & `ECLiPSe`)*
  `/src/sudoku/benchmarks/`           `benchmarks.pl`                    *Automatic benchmarking code*
  `/src/sudoku/benchmarks/puzzles/`   `*`                                     *Sudoku benchmarks*
  `/src/sudoku/chr/`                  `solver.pl`                           *Sudoku solver (`CHR`)*
  `/src/sudoku/chr/model/`            `*`                                 *Sudoku viewpoints (`CHR`)*
  `/src/sudoku/eclipse/`              `solver.pl`                         *Sudoku solver (`ECLiPSe`)*
  `/src/sudoku/eclipse/model/`        `*`                               *Sudoku viewpoints (`ECLiPSe`)*
  `/src/hashiwokakero/eclipse/`       `solver.pl`                      *Hashiwokakero solver (`ECLiPSe`)*
  `/src/hashiwokakero/chr/`           `solver.pl`                        *Hashiwokakero solver (`CHR`)*
  `/src/hashiwokakero/benchmarks/`    `hashi_benchmarks.pl`                *Hashiwokakero benchmarks*
  `/src/scheduling/`                  `scheduling.pl`                    *Scheduling meetings solution*


[^1]: `ECLiPSe` and `CHR` implementations are available in
    `/sudoku/model/classic.pl` and in `/sudoku/chr/model/classic.pl`.

[^2]: These are implemented with `ECLiPSe` in the
    `/sudoku/eclipse/model/` directory, and some `CHR` versions are in
    `/sudoku/chr/model/`.

[^3]: `/sudoku/benchmarks/benchmarks.pl` provides automatic benchmarking
    code.

[^4]: [These are available online.](http://rotor.di.unipi.it/cisterni/Shared%20Documents/minsudoku.html)

[^5]: A custom-made implementation as well as the
    [`\sim=/2`](https://eclipseclp.org/doc/bips/kernel/termcomp/TE-2.html)
    constraint which checks if two terms can be unified were tried.
    Channeling back to integers with
    [`ic_global:bool_channeling/3`](http://eclipseclp.org/doc/bips/lib/ic_global/bool_channeling-3.html)
    worked better (ironically).

[^6]: *"The reason for this difference is that the primal not-equals
    constraints detect singleton variables (i.e. those variables with a
    single value), the channelling constraints detect singleton
    variables and singleton values (i.e. those values which occur in the
    domain of a single variable), whilst the primal all-different
    constraint detects global consistency (which includes singleton
    variables, singleton values and many other
    situations)\"

[^7]: The code lies in `sudoku/model/channeling.pl` in which a flag
    called `extended` can be used to opt for one of two variants.

[^8]: In his study Demoen gives several *Missing(6)* examples, models in
    which 6 of the `all_different` constraints are removed. *Missing(7)*
    models aren't Sudoku, and because of the stark rise in number of
    backtracks no further experimentation with the removal of '*small*'
    constraints was done. The `eliminate_redundancy` flag can be used to
    toggle redundancy elimination on and off.

[^9]: The difference between unequality constraints and channeling is
    explained in more detail in. Adding
    unequalities to the implementation slows down the search procedure
    because the channeling constraints already do this propagation and
    more.

[^10]: For solving the provided puzzles, not in general.

[^11]: At some point a constraint `pos/4` was used to convert from
    *row-column* combinations to the corresponding blocks and positions
    (e.g. `pos(2,3,1,6)`). This slowed down the algorithm a lot because
    it increased the size of the constraint store. Encoding positions
    with 4 coordinates worked better.

[^12]: It's a little harder to count the number of backtracks in `CHR`.
    Because the channeling-only model solves the *extra2* puzzle with
    zero backtracks the `CHR` model was tested on that one, and it also
    did zero backtracking.

[^13]: It can be noted that Joachim's code enforces both $A$ \#= $B$ and
    $B$ \#= $A$ in several cases. It has no effect on the runtimes.

[^14]: Let's give a concrete example. Say, an island has the number four
    and three neighbors. Nothing could be concluded before doing any
    searching. If at any point during search it becomes clear that one
    of the three neighbors can't be connected to the island, then two
    pairs of bridges need to be added to form connections with the
    remaining neighbours.

[^15]: Note that in the case of trivial boards with nothing else than
    two such islands enforcing this constraint would prevent the
    solution from being found.

[^16]: Experiments with value heuristics (`indomain_min`,
    `indomain_max`) showed that changing the heuristic can have a large
    impact on the number of backtracks. The same holds true for variable
    heuristics. Part of the reason why ECLiPSe is a whole lot faster is
    that it enforces bound consistency for the sum constraints. In the
    sum constraints of the basic `CHR` solver only forward checking is
    done.

[^17]: At least in the case of the provided benchmarks. In the case of
    puzzle six and a two smaller boards that were added, the first
    solution isn't connected.

[^18]: All code for this third challenge can be found in
    `/src/scheduling/scheduling.pl`.

[^19]: The total runtime was reduced by a factor of 4 (not when making
    use of the `ic_edge_finder` version).
