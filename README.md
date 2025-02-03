
<!-- README.md is generated from README.Rmd. Please edit that file -->

# projrsimple

<!-- badges: start -->

[![R-CMD-check](https://github.com/MiguelRodo/projrsimple/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MiguelRodo/projrsimple/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of projrsimple is to make it easy to run a clean project
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
4.  Write code in scripts in the project directory.
5.  Run `projr_run` to execute all scripts. Save outputs to `_output`
    directory.
6.  View rendered documents in the `docs` directory.

To connect to GitHub automatically, run `projr_init(init_github = TRUE)`
(step 3) *after* the following steps:

1.  Create a GitHub account ([link](https://www.github.com)).
2.  Set up a personal access token (PAT) in R
    ([instructions](https://happygitwithr.com/https-pat#tldr)).

## Details

### Project Initialisation with `projr_init`

The function `projr_init` is designed to help you quickly set up a
standard project  
structure. It will (all of these features are optional):

- **Create Standard Directories:**

  By default, it creates directories for:

  - Raw data: `_raw_data`
  - Temporary/cache files: `_tmp`
  - Rendered documents: `docs`
  - Final outputs: `_output`
  - Raw (non‐rendered) docs: `_reference`

  All of these directory names are fully customizable by supplying your
  own values  
  (or you can set any to `NULL` if you don’t want that directory).

- **Initialise a Git Repository:**

  If you choose, `projr_init` can initialise a Git repository in your
  project. It will automatically add the cache directory (by default
  `_tmp`) to your `.gitignore` to avoid committing  
  temporary files, and (if enabled) will add and commit any changes.  
  *Note:* To push your repository to GitHub, you’ll need a GitHub
  account and must  
  ensure that R is configured with your personal access token (PAT).

- **Create a README File:**

  A default README is created with instructions on how to run your
  analyses,  
  reproduce the results, and a description of the directory structure.
  You can  
  customise or remove this by setting the corresponding parameter to
  `FALSE`.

- **Connect to GitHub (Optional):**

  If desired, you can instruct `projr_init` to connect your local Git
  repository  
  to GitHub (using `usethis::use_github()`). The repository can be
  created as private  
  by default, with a message reminding you that you must add
  collaborators manually  
  via GitHub’s settings.

### Running Analyses with `projr_run`

After you have initialised your project, you can use `projr_run` to
execute your  
analyses. By default:

- **Script Detection and Execution:**

  All scripts in your working directory with extensions `.R`,
  `.Rmd`/`.rmd`, and  
  `.qmd` are run automatically. In the presence of a Quarto project (a
  file named  
  `_quarto.yml`), individual `.qmd` files may be skipped in favour of
  running the  
  project as a whole.

- **Copying Generated Documents:**

  The function will copy any generated documents (typically `.html`,
  `.pdf`, or  
  `.docx`) to the `docs` directory. This behavior is optional.

- **Output Directory Management:**

  Optionally, the output directory (by default `_output`) is cleared
  before running  
  all the scripts.

- **Selective Execution:**

  If you prefer, you can select which scripts to run by providing a
  character  
  vector of filenames. This enables you to run only a subset of your
  scripts.

### Multiple pipelines

For more advanced workflows, you can run `projr_run` multiple times to
create  
separate “pipelines”. For example, you might run one pipeline that
processes raw data  
and another that generates reports, each using different script
selections and  
output/document directories. This modular approach allows you to
flexibly manage  
complex projects with multiple stages.

## Citation

To cite `projrsimple` in publications use:

@Misc{rodo, title = {projrsimple: Initialise and run a simple project
workflow}, author = {Miguel Rodo}, url =
{<https://github.com/MiguelRodo/projrsimple/#readme>}, abstract =
{Initialise and run a simple project workflow}, version = {0.0.0-1}, }
