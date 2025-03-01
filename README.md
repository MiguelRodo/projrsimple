
<!-- README.md is generated from README.Rmd. Please edit that file -->

# projrsimple

<!-- badges: start -->

[![R-CMD-check](https://github.com/MiguelRodo/projrsimple/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MiguelRodo/projrsimple/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `projrsimple` is to make it easy to run a clean project
workflow.

It helps you initialise a reproducible project structure with
pre‐defined directories, run analysis scripts, and ensure clean output
directories with minimal effort.

## Installation

You can install the development version of `projrsimple` like so:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("MiguelRodo/projrsimple")
```

## TL;DR

1.  Install `projrsimple` (see above).
2.  Open an R session in your project directory (folder where you want
    to work).
3.  Run `projr_init()` to initialise your project.
4.  Write code in scripts (`.R`, `.Rmd`, `.qmd` and/or Quarto projects)
    in the project directory.
5.  Run `projr_run` to execute all scripts. Save outputs to `_output`
    directory.
6.  View rendered documents in the `docs` directory.

To connect to GitHub automatically, run `projr_init(init_github = TRUE)`
(step 3) *after* the following steps:

1.  Create a GitHub account ([link](https://www.github.com)).
2.  Set up a personal access token (PAT) in R
    ([instructions](https://happygitwithr.com/https-pat#tldr)).

## Details

For details, view the introductory article on the [package
website](https://miguelrodo.github.io/projrsimple/articles/intro.html)
or as a vignette (`vignette("intro", package = "projrsimple")`).

## Citation

To cite `projrsimple` in publications, use:

> **Miguel Rodo** (2024). *projrsimple: Initialise and run a simple
> project workflow.* Version 1.0.0. Available at:
> [https://github.com/MiguelRodo/projrsimple](https://github.com/MiguelRodo/projrsimple/#readme).

Alternatively, in `BibTeX` format:

``` bibtex
@Misc{rodo,
  title = {projrsimple: Initialise and run a simple project workflow},
  author = {Miguel Rodo},
  url = {https://github.com/MiguelRodo/projrsimple/#readme},
  abstract = {Initialise and run a simple R project workflow},
  version = {1.0.0},
}
```
