# sdcTable next
- bugfix in `protectLinkedTables()`; thx Øyvind Langsrud for spotting and reporting
- bugfix in `c_gen_mat_m` with problems that have a single dimension
- bugfix when computing dominance-rules on weighted data
- new exported function `createRegSDCInput()`
- improved vignette; thx to @Krisselack for reporting
- speedup and simplify computation of contributing units (from raw-data) to a table cell in `contributing_indices()` and removed internal helper-function `c_contributing_indices()`
- allow nk-dominance rules with n=1
- [todo] feature: allow setting sdcStatus input data.frame/list of strIDs
- [todo] more checks/performance improvements in `protectLinkedTables()`
- [todo] improve performance singleton detection procedure

# sdcTable 0.31
- remove debugging outut
- fixing typos in vignettes
- bugfix in `protectLinkedTables()`
- remove dependency on package `lpSolveAPI`
- update singleton-detecton procedure by allowing to input a threshold value that must be respected for all simple table rows
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
