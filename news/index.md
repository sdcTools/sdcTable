# Changelog

## sdcTable 0.34.0

- New parallel processing support:
  - [`attack()`](https://sdctools.github.io/sdcTable/reference/attack.md)
    now features an `n_workers` argument, enabling parallel execution of
    attacker problems via the `future.apply` package (if available).
- [`protectTable()`](https://sdctools.github.io/sdcTable/reference/protectTable.md)
  enhancements:
  - The function now exposes `n_workers` when using
    `method = "SIMPLEHEURISTIC"`, allowing parallelized internal
    attacker calls (passed through to
    [`attack()`](https://sdctools.github.io/sdcTable/reference/attack.md)).
  - Added argument `attack_threshold` for `method = "SIMPLEHEURISTIC"`.
    This allows users to define the safety tolerance (as
    `abs(upper - lower) > attack_threshold`) directly when protecting
    tables.
- Documentation updates: Added detailed explanations and performance
  warnings regarding RAM usage for parallel processing in the package
  vignette and function man-pages.
- Added a `threshold` argument to
  [`attack()`](https://sdctools.github.io/sdcTable/reference/attack.md)
  for more flexible safety checks (defaults to `1e-8`).
- Optimized internal `create_m_matrix()` for faster matrix construction
  and lower memory overhead.
- Refactored/Simplified internal parameter-generation
- Added a centralized **Cell Status Codes** section to `sdcProblem`
  documentation, explaining states like `"z"` (empty cells) and `"w"`.
  This section is now reused across all functions that modify cell
  statuses
  ([UserIssue](https://github.com/sdcTools/UserSupport/issues/319)
  [\#319](https://github.com/sdcTools/userSupport/issues/319))
- Significantly optimized
  [`contributing_indices()`](https://sdctools.github.io/sdcTable/reference/contributing_indices.md)
  regarding running-time and memory usage

## sdcTable 0.33.0

CRAN release: 2025-06-26

- Use `highs` instead of `Rglpk` and `glpkAPI` for LP-Solving as package
  was removed from CRAN

## sdcTable 0.32.7

CRAN release: 2025-03-11

- Additional check for
  [`createArgusInput()`](https://sdctools.github.io/sdcTable/reference/createArgusInput.md)
  to make sure that the holding variable is integer (if specified)
- Additional check for colons in labels in
  [`createArgusInput()`](https://sdctools.github.io/sdcTable/reference/createArgusInput.md)
- Fix non-default Totals whe using
  [`createArgusInput()`](https://sdctools.github.io/sdcTable/reference/createArgusInput.md)

## sdcTable 0.32.6

CRAN release: 2023-08-11

- Improvements in `SIMPLEHEURISTIC` when adding additional suppressions
- Add `RegSDC` as suggested package in order to avoid a CRAN note
- Skip Unit-Tests for CRAN only

## sdcTable 0.32.5

CRAN release: 2023-05-19

- Fix singleton-detection procedure in case of existing “dummy-cells”
  (code `"w"`)

## sdcTable 0.32.4

CRAN release: 2022-09-22

- Update dependency on `Matrix` package

## sdcTable 0.32.3

CRAN release: 2022-07-16

- Make
  [`SSBtools::GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)
  as `method = "GAUSS"` available in
  [`protectTable()`](https://sdctools.github.io/sdcTable/reference/protectTable.md)
- Update of package vignette
- Small overall fixes
- Fix creation of tau-Argus batchfiles (`typ = "microdata"`) in case
  aggregate codes are available in raw input data

## sdcTable 0.32.2

CRAN release: 2021-12-03

- when applying dominance rules, empty cells (with frequency `0`) are
  never marked primary sensitive
- do not overwrite pre-existing sensitive cells when applying a
  threshold rule after a dominance rule with `allowZeros = FALSE`
- when computing dominance rules using sampling weights it is ensured
  that weights are consistently rounded

## sdcTable 0.32.1

CRAN release: 2021-09-30

- allow invocation of exact previous implementation of
  `"SIMPLEHEURISTIC` using `method = "SIMPLEHEURISTIC_OLD"` in
  [`protectTable()`](https://sdctools.github.io/sdcTable/reference/protectTable.md)
- fix for edge-cases in `"SIMPLEHEURISTIC"`: weights are temporarily
  changed for `"z"`-cells if no additional suppression can be detected
- combined existing functionality to compute constraint-matrix of a
  problem instance into (much faster) `create_m_matrix()`
  - replaced internal methods/functions `c_gen_mat_m()` and
    `.gen_constraint_matrix()` and `genMatMFull()`
- re-parametrized internal method `c_make_att_prob`

## sdcTable 0.32.0

CRAN release: 2021-08-06

- rewrite of the `"SIMPLEHEURISTIC` approach
  - is based on (full) constraint matrix written using `rcpp`
  - rewritten the singleton-detection procedure with `rcpp`
  - by default attacker-problems are checked (in a loop) for primary
    sensitive cells and additional supps are added until all required
    cells are secure (fixes also issue
    [\#136](https://github.com/sdcTools/userSupport/issues/136), thx
    Øyvind Langsrud for reporting)
  - the previous (possible unsafe but faster) implemented method can be
    toggled using parameter `solve_attackerprobs` in `protectTable` and
    `protectLinkedTable`
- bugfix in internal method `c_gen_mat_m` with problems that have a
  single dimension
- dominance rules:
  - bugfix when computing rules on weighted data
  - allow nk-dominance rules with n=1 (thx
    [@MaximeBeaute](https://github.com/MaximeBeaute) for reporting)
  - increased performance as contributing-units to cells are computed
    only once
- improved function
  [`attack()`](https://sdctools.github.io/sdcTable/reference/attack.md)
  - problem-formulation and solution using the `glpkAPI`-package
  - can be used to attack all (suppressed) or specific cells
  - can also be used after computing a solution for the cell suppression
    problem
- performance improvements
  - `c_quick_suppression()` and
    [`attack()`](https://sdctools.github.io/sdcTable/reference/attack.md)
    compute linear deps (`.gen_contraint_matrix`) only once
  - improvements in computation of contributing units to a cell
    (`contributing_indices`)
- new/updated functions/methods:
  - new function
    [`protect_linked_tables()`](https://sdctools.github.io/sdcTable/reference/protect_linked_tables.md)
    - will replace
      [`protectLinkedTables()`](https://sdctools.github.io/sdcTable/reference/protect_linked_tables.md)
      in the future (currently calls new function internally)
    - only allows the `SIMPLEHEURISTIC` algorithm
  - [`cell_info()`](https://sdctools.github.io/sdcTable/reference/cell_info.md)
    replaces
    [`cellInfo()`](https://sdctools.github.io/sdcTable/reference/defunct-sdcTable.md)
    (defunct)
  - [`change_cellstatus()`](https://sdctools.github.io/sdcTable/reference/change_cellstatus.md)
    replaces
    [`changeCellStatus()`](https://sdctools.github.io/sdcTable/reference/defunct-sdcTable.md)
    (defunct)
  - new function
    [`createRegSDCInput()`](https://sdctools.github.io/sdcTable/reference/createRegSDCInput.md)
    allows to compute input for e.g
    [`RegSDC::SuppressDec`](https://olangsrud.github.io/RegSDC/reference/SuppressDec.html)
  - removed S4-class definition `safeObj` and related methods
    - results are stored within `sdcProblem`-objects (in slot `results`)
    - rewritten
      [`getInfo()`](https://sdctools.github.io/sdcTable/reference/getInfo.md)
      using an internal helper-function
      [`get_safeobj()`](https://sdctools.github.io/sdcTable/reference/get_safeobj.md)
  - new function
    [`contributing_indices()`](https://sdctools.github.io/sdcTable/reference/contributing_indices.md)
    - returns contributing units (from raw input data) to a cell
    - removed internal helper-function `c_contributing_indices()`
  - replaced internal S4-method `c_cellID` with utility-function
    `cell_id()`
- improved examples, documentation, test-data and unit-tests
  - improved and updated documentation
    [`getInfo()`](https://sdctools.github.io/sdcTable/reference/getInfo.md)
    and
    [`setInfo()`](https://sdctools.github.io/sdcTable/reference/setInfo.md)
  - replaced `microData1.RData` and `microData2.RData`
    - generation is reproducible in `data-raw`
    - data can be loaded using `data(microdata1)` and `data(microdata2)`
  - replaced `problem.RData` and `problemWithSupps.RData` with
    [`sdc_testproblem()`](https://sdctools.github.io/sdcTable/reference/sdc_testproblem.md)
  - better document sdc-code `z` in
    [`primarySuppression()`](https://sdctools.github.io/sdcTable/reference/primarySuppression.md)
  - improved and updated vignette (thx to
    [@Krisselack](https://github.com/Krisselack) for reporting)

## sdcTable 0.31

CRAN release: 2020-03-10

- remove debugging output
- fixing typos in vignettes
- bugfix in
  [`protectLinkedTables()`](https://sdctools.github.io/sdcTable/reference/protect_linked_tables.md)
- remove dependency on package `lpSolveAPI`
- update singleton-detection procedure by allowing to input a threshold
  value that must be respected for all simple table rows
- check dominance rules (unweighted variables are used)

## sdcTable 0.30

CRAN release: 2019-09-19

- various fixes and improvements in
  [`createJJFormat()`](https://sdctools.github.io/sdcTable/reference/createJJFormat.md)
- ignore sampling weights in case the input in
  [`makeProblem()`](https://sdctools.github.io/sdcTable/reference/makeProblem.md)
  is a complete table and not microdata
- `sdcProb2df():` if numeric variables are shown; display their weighted
  values (in case) sampling weights have been specified in `makeProblem`

## sdcTable 0.29

CRAN release: 2019-08-31

- change in
  [`primarySuppression()`](https://sdctools.github.io/sdcTable/reference/primarySuppression.md):
  for dominance rule it is now only possible to specify underlying
  variables by name (using argument `numVarName`) and no longer by index
  to avoid errors
- fixes in primary suppression for dominance rules
- create files from examples in tempdir()

## sdcTable 0.28

CRAN release: 2019-06-04

- new function
  [`contributing_indices()`](https://sdctools.github.io/sdcTable/reference/contributing_indices.md)
- call `.Defunct` for old exported functions
- make default bounds (`ub`, `lb`) depend on costs
- allow to specify numVarInd also by name via `numVarName` in
  primarySupression() for dominance rules (`p`, `nk` and `pq`)
- make use of sampling weights (by replicating) values for dominance
  rules.
- don’t export unused functionality
- [`createJJFormat()`](https://sdctools.github.io/sdcTable/reference/createJJFormat.md)
  and
  [`writeJJFormat()`](https://sdctools.github.io/sdcTable/reference/writeJJFormat.md)
  can be used to create text files with sdcProblems in “JJs Format”
- allow to specify variable names too in `makeProblem`

## sdcTable 0.27

CRAN release: 2019-02-28

- use functionality from
  [`sdcHierarchies`](https://cran.r-project.org/package=sdcHierarchies)
  to build hierarchies. - removed
  [`create_node()`](https://sdctools.github.io/sdcTable/reference/defunct-sdcTable.md);
  please use
  [`hier_create()`](https://bernhard-da.github.io/sdcHierarchies/reference/hier_create.html)
  instead
  - removed
    [`add_nodes()`](https://sdctools.github.io/sdcTable/reference/defunct-sdcTable.md);
    please use
    [`hier_add()`](https://bernhard-da.github.io/sdcHierarchies/reference/hier_add.html)
    instead
  - removed
    [`delete_nodes()`](https://sdctools.github.io/sdcTable/reference/defunct-sdcTable.md);
    please use
    [`hier_delete()`](https://bernhard-da.github.io/sdcHierarchies/reference/hier_delete.html)
    instead
  - removed `rename_nodes()`; please use
    [`hier_rename()`](https://bernhard-da.github.io/sdcHierarchies/reference/hier_rename.html)
    instead

## sdcTable 0.26

CRAN release: 2019-01-18

- bugfix when computing indices for contributing units used in some
  primary suppression methods

## sdcTable 0.25

CRAN release: 2018-11-14

- better error messages in case invalid hierarchies have been specified

## sdcTable 0.24

CRAN release: 2018-07-24

- update in SIMPLEHEURISTIC-algorithm that now really respects cells
  (status `"z"`) that must not be suppressed

## sdcTable 0.23

CRAN release: 2018-06-22

- bugfix in SIMPLEHEURISTIC-algorithm
- code cleanup
- updated package vignette
- new functions
  [`create_node()`](https://sdctools.github.io/sdcTable/reference/defunct-sdcTable.md),
  [`add_nodes()`](https://sdctools.github.io/sdcTable/reference/defunct-sdcTable.md)
  and
  [`delete_nodes()`](https://sdctools.github.io/sdcTable/reference/defunct-sdcTable.md)
  that allow a dynamic generation of hierarchies. For an example have a
  look at
  [`?makeProblem`](https://sdctools.github.io/sdcTable/reference/makeProblem.md)

## sdcTable 0.22.x

- Feature to create BATCH files suitable for
  [**tau-argus**](https://github.com/sdcTools/tauargus)
