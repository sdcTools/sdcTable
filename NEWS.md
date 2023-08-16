# sdcTable 0.32.6
- Improvements in `SIMPLEHEURISTIC` when adding additional suppressions
- Add `RegSDC` as suggested package in order to avoid a CRAN note
- Skip Unit-Tests for CRAN only

# sdcTable 0.32.5
- Fix singleton-detection procedure in case of existing "dummy-cells" (code `"w"`)

# sdcTable 0.32.4
- Update dependency on `Matrix` package

# sdcTable 0.32.3
- Make `SSBtools::GaussSuppression` as `method = "GAUSS"` available in `protectTable()`
- Update of package vignette
- Small overall fixes
- Fix creation of tau-Argus batchfiles (`typ = "microdata"`) in case aggregate codes are available in raw input data

# sdcTable 0.32.2
- when applying dominance rules, empty cells (with frequency `0`) are never marked primary sensitive
- do not overwrite pre-existing sensitive cells when applying a threshold rule after a dominance rule with `allowZeros = FALSE`
- when computing dominance rules using sampling weights it is ensured that weights are consistently rounded

# sdcTable 0.32.1
- allow invocation of exact previous implementation of `"SIMPLEHEURISTIC` using `method = "SIMPLEHEURISTIC_OLD"` in `protectTable()`
- fix for edge-cases in `"SIMPLEHEURISTIC"`: weights are temporarily changed for `"z"`-cells if no additional suppression
can be detected
- combined existing functionality to compute constraint-matrix of a problem instance into (much faster) `create_m_matrix()`
  * replaced internal methods/functions `c_gen_mat_m()` and `.gen_constraint_matrix()` and `genMatMFull()`
- re-parametrized internal method `c_make_att_prob`

# sdcTable 0.32.0
- rewrite of the `"SIMPLEHEURISTIC` approach
  * is based on (full) constraint matrix written using `rcpp`
  * rewritten the singleton-detection procedure with `rcpp`
  * by default attacker-problems are checked (in a loop) for primary sensitive cells and additional supps are added until all required cells are secure (fixes also issue #136, thx Øyvind Langsrud for reporting)
  * the previous (possible unsafe but faster) implemented method can be toggled using parameter `solve_attackerprobs` in `protectTable` and `protectLinkedTable`
- bugfix in internal method `c_gen_mat_m` with problems that have a single dimension
- dominance rules:
  * bugfix when computing rules on weighted data
  * allow nk-dominance rules with n=1 (thx @MaximeBeaute for reporting)
  * increased performance as contributing-units to cells are computed only once
- improved function `attack()`
  * problem-formulation and solution using the `glpkAPI`-package
  * can be used to attack all (suppressed) or specific cells
  * can also be used after computing a solution for the cell suppression problem
- performance improvements
  * `c_quick_suppression()` and `attack()` compute linear deps (`.gen_contraint_matrix`) only once
  * improvements in computation of contributing units to a cell (`contributing_indices`)
- new/updated functions/methods:
  * new function `protect_linked_tables()` 
    + will replace `protectLinkedTables()` in the future (currently calls new function internally)
    + only allows the `SIMPLEHEURISTIC` algorithm
  * `cell_info()` replaces `cellInfo()` (defunct)
  * `change_cellstatus()` replaces `changeCellStatus()` (defunct)
  * new function `createRegSDCInput()` allows to compute input for e.g `RegSDC::SuppressDec`
  * removed S4-class definition `safeObj` and related methods
    + results are stored within `sdcProblem`-objects (in slot `results`)
    + rewritten `getInfo()` using an internal helper-function `get_safeobj()`
  * new function `contributing_indices()`
    * returns contributing units (from raw input data) to a cell
    * removed internal helper-function `c_contributing_indices()`
  * replaced internal S4-method `c_cellID` with utility-function `cell_id()`
- improved examples, documentation, test-data and unit-tests
  * improved and updated documentation `getInfo()` and `setInfo()`
  * replaced `microData1.RData` and `microData2.RData` 
    + generation is reproducible in `data-raw`
    + data can be loaded using `data(microdata1)` and `data(microdata2)`
  * replaced `problem.RData` and `problemWithSupps.RData` with `sdc_testproblem()`
  * better document sdc-code `z` in `primarySuppression()`
  * improved and updated vignette (thx to @Krisselack for reporting)

# sdcTable 0.31
- remove debugging output
- fixing typos in vignettes
- bugfix in `protectLinkedTables()`
- remove dependency on package `lpSolveAPI`
- update singleton-detection procedure by allowing to input a threshold value that must be respected for all simple table rows
- check dominance rules (unweighted variables are used)

# sdcTable 0.30
- various fixes and improvements in `createJJFormat()`
- ignore sampling weights in case the input in `makeProblem()` is a complete table
and not microdata
- `sdcProb2df():` if numeric variables are shown; display their weighted values (in case)
sampling weights have been specified in `makeProblem`

# sdcTable 0.29
- change in `primarySuppression()`: for dominance rule it is now only possible to specify underlying variables by name (using argument `numVarName`) and no longer by index to avoid errors
- fixes in primary suppression for dominance rules
- create files from examples in tempdir()

# sdcTable 0.28
- new function `contributing_indices()`
- call `.Defunct` for old exported functions
- make default bounds (`ub`, `lb`) depend on costs
- allow to specify numVarInd also by name via `numVarName` in primarySupression() for dominance rules (`p`, `nk` and `pq`)
- make use of sampling weights (by replicating) values for dominance rules.
- don't export unused functionality
- `createJJFormat()` and `writeJJFormat()` can be used to create text files with
sdcProblems in "JJs Format"
- allow to specify variable names too in `makeProblem`

# sdcTable 0.27
- use functionality from [`sdcHierarchies`](https://cran.r-project.org/package=sdcHierarchies) to build hierarchies.     - removed `create_node()`; please use `hier_create()` instead
    - removed `add_nodes()`; please use `hier_add()` instead
    - removed `delete_nodes()`; please use `hier_delete()` instead
    - removed `rename_nodes()`; please use `hier_rename()` instead

# sdcTable 0.26
* bugfix when computing indices for contributing units used in some primary suppression methods

# sdcTable 0.25
* better error messages in case invalid hierarchies have been specified

# sdcTable 0.24
* update in SIMPLEHEURISTIC-algorithm that now really respects cells (status `"z"`) that must not be suppressed

# sdcTable 0.23
* bugfix in SIMPLEHEURISTIC-algorithm
* code cleanup
* updated package vignette
* new functions `create_node()`, `add_nodes()` and `delete_nodes()` that allow a dynamic generation of hierarchies. For an example have a look at `?makeProblem`

# sdcTable 0.22.x
* Feature to create BATCH files suitable for [**tau-argus**](https://github.com/sdcTools/tauargus)
