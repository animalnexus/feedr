# Code design

feedr loads, cleans, transforms, and visualizes RFID data from pit-tagged
animals detected at static stations (feeders, nest boxes, etc.). The Shiny UI
was split into the separate feedrUI package, so UI code doesn't belong here.

## Commands

Run from the package root, in R:

```r
devtools::load_all() # Load the package for interactive use
devtools::document() # Regenerate man/ and NAMESPACE from roxygen
devtools::test() # Run all tests
devtools::test(filter = "b03_move") # Run one file (test_b03_move.R)
devtools::check(vignettes = FALSE) # Closest local match to the R-CMD-check workflow
devtools::build_readme() # Rebuild README.md from README.Rmd
pkgdown::build_site() # Build the site, including vignettes
```

Format R code with `air format .` from the shell or format-on-save in the IDE
(`air.toml` contains settings, if empty, means using the defaults).

## Generated files

Edit the source, then regenerate. Don't edit these by hand:

  | Generated             | Source                   |
  | --------------------- | ------------------------ |
  | `man/`, `NAMESPACE`   | roxygen comments in `R/` |
  | `README.md`           | `README.Rmd`             |
  | `docs/` (git-ignored) | `pkgdown::build_site()`  |

`README.Rmd` runs `covr::package_coverage()` to compute its coverage badge, so
rebuilding it also runs the test suite.

## Data pipeline

Each function's output is the next one's input. Argument names indicate the type
of data expected: `r` raw reads, `v` visits, `p` presence, `m` movements, `a`
activity, `d` displacements.

```
load_raw() / load_raw_all() / load_format()
  -> check_ids() / check_problems()      Clean raw reads
  -> r (raw: animal_id, logger_id, time)
       |-> inout()                       Trips in/out of an area
       |-> convert_asnipe(), convert_activity()
       '-> visits()
             |-> move()      --.
             |-> presence()  --+-> map_leaflet(p, m)
             |     '-> activity() -> daily()
             '-> disp()      -> convert_anidom(), convert_dominance(), convert_perc()
```

## Conventions for transformations

Functions in `R/transformations.R` share a structure, which new ones should
follow:

1. **Validate** with the helpers in `R/errorchecks.R`: `check_name()` for
   required columns, `check_time()` for POSIXct columns, and `check_format()`
   for id values.
2. **Record factor levels** of `animal_id` and `logger_id`, then convert them to
   character so joins between different levels stay silent.
3. **Save extra columns** when `pass = TRUE`, using `keep_extra()`. This finds
   columns that vary only by animal, logger, or date.
4. **Transform.** `inout()`, `move()`, `presence()`, `activity()`, and `daily()`
   apply a `*_single()` helper to each animal with `group_by()` and `do()`.
5. **Restore** the extra columns with `merge_extra()` and the original factor
   levels, so animals or loggers with no results are still represented.

`_` is a reserved separator in ids. Directions and paths are built by joining
logger ids (`move_dir`, `move_path`, `dir_in` such as `id1_id2`), so
`check_format()` warns about `_` in `animal_id` or `logger_id` values.

Package code uses `%>%`, `.data$col`, and explicit `pkg::fun()` calls. Vignettes
and the README use `|>`.

## Testing

Test files are prefixed by area: `a` for utilities, error checks, loading, and
housekeeping; `b` for transformations and conversions; and `c` for maps and
summaries.

Tests use the bundled datasets (`finches`, `finches_lg`, `chickadees`) and the
files in `inst/extdata/`. `inst/extdata/import_tests/` holds input format
variations for the loading functions.

Leaflet snapshots (`_snaps/c05_map_leaflet.md`) are skipped on Windows and
macOS.

## Vignettes and CI

Vignettes are Quarto documents (`vignettes/*.qmd`), so building them requires
the Quarto CLI. The R-CMD-check workflow skips them because Quarto fails on
Windows; the pkgdown workflow builds them instead. A change that affects
vignettes therefore isn't covered by R-CMD-check.

## Release

`RELEASE.R` is the release checklist.
