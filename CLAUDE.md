@../../levante-data-meta/CLAUDE.md

## levantemodels

- **Every new help topic must be listed in `_pkgdown.yml`'s `reference` index**
  (any new exported function, or new `.Rd` file in `man/`). The pkgdown CI
  workflow fails otherwise. After `devtools::document()`, check with
  `pkgdown::check_pkgdown()`.
