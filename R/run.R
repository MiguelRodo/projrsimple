#' Run project scripts, optionally clearing output and copying docs
#'
#' @description
#' This function:
#'
#' 1. Collects R, Rmd, or qmd scripts from `dir_scripts`, or uses a supplied
#'    character vector.
#' 2. Optionally clears `dir_output` and `dir_docs`.
#' 3. Sources or renders each script, skipping `.qmd` scripts if a Quarto project
#'    is detected.
#' 4. By default, copies generated `.html`, `.pdf`, or `.docx` files to
#'    `dir_docs`.
#' 5. By default, renders the entire Quarto project if `_quarto.yml` is found.
#'
#' @param scripts Character vector of script filenames, e.g.
#'   `c("my_script.R", "my_doc.Rmd")`. If `NULL`, all `.R`, `.Rmd`/`.rmd`, and
#'   `.qmd` files in `dir_scripts` are used.
#' @param skip_quarto_project Logical. If `TRUE`, does not render the Quarto
#'   project, even if `_quarto.yml` is found.
#' @param clear_output_and_docs Logical. If `TRUE`, clears out `dir_output` and
#'   `dir_docs` before running.
#' @param copy_docs Logical. If `TRUE`, copies generated output files (`.html`,
#'   `.pdf`, `.docx`) into `dir_docs`. Does not apply to Quarto projects.
#' @param dir_scripts Directory where scripts reside. Defaults to the working
#'    directory if `NULL`.
#' @param dir_output Directory to clear if `clear_output_and_docs = TRUE`.
#'   Defaults to `"_output"` if `NULL`.
#' @param dir_docs Directory where final docs are copied. Defaults to `"docs"`
#'   if `NULL`.
#'
#' @return Invisibly returns `TRUE` when complete.
#' 
#' @details 
#' To avoid accidental deletion of outputs,
#' `clear_output_and_docs` is by default `FALSE`.
#' However, for a clean run, it is recommended to set `clear_output_and_docs`
#' to `TRUE`.
#'
#' @examples
#' \dontrun{
#' # Run all scripts in "scripts", remove old docs/outputs, and copy
#' # newly generated docs to "docs"
#' projr_run(
#'   skip_quarto_project = TRUE,
#'   clear_output_and_docs = TRUE,
#'   copy_docs = TRUE
#' )
#' }
projr_run <- function(scripts = NULL,
                      skip_quarto_project = FALSE,
                      clear_output_and_docs = FALSE,
                      copy_docs = TRUE,
                      dir_scripts = NULL,
                      dir_output = NULL,
                      dir_docs = NULL,
                      dir_exec = NULL) {
  if (!is.null(dir_exec)) stopifnot(dir.exists(dir_exec))

  # get scripts to run individually
  scripts_run <- .get_scripts_run(scripts, dir_scripts)

  # install deps if not already installed
  .install_deps(scripts)

  # Optionally clear output and docs
  .clear_dir(clear_output_and_docs, dir_output, TRUE, "_output")
  .clear_dir(clear_output_and_docs, dir_docs, copy_docs, "docs")

  # Run each script, then copy its docs
  for (i in seq_along(scripts_run)) {
    .run_script(scripts_run[[i]], dir_exec)
    .copy_docs(scripts_run[[i]], copy_docs, dir_docs)
  }

  # If there's a _quarto.yml, optionally render the entire project
  .run_quarto_project(scripts_run, skip_quarto_project)

  invisible(TRUE)
}

# -----------------------------------------------------------------------
# Install dependencies if not already installed
# -----------------------------------------------------------------------

.install_deps <- function(scripts) {
  if (length(scripts) == 0L) {
    return(invisible(FALSE))
  }
  .install_deps_scripts(scripts)
  if (file.exists("_quarto.yml")) {
    .install_deps_quarto()
  }

  invisible(TRUE)
}

.install_deps_scripts <- function(scripts) {
  if (length(scripts) == 0L) {
    return(invisible(FALSE))
  }
  if (any(grepl("\\.qmd$", scripts))) {
    .install_deps_quarto()
  }
  if (any(grepl("\\.Rmd$", scripts))) {
    .install_deps_actual("rmarkdown")
  }
}

.install_deps_quarto <- function() {
  .install_deps_actual("quarto")
  if (inherits(try(quarto::quarto_version()), "try-error")) {
    stop(paste0(
      "\n",
      "Quarto (the program, not the R package) is not installed or not working.\n",
      "Install from https://quarto.org/docs/get-started/")
    )
  }
}

.install_deps_actual <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    message("Installing package: ", x)
    if (.detect_renv()) {
      if (!requireNamespace("renv", quietly = TRUE)) {
        utils::install.packages("renv")
      }
      renv::install(x)
    } else {
      utils::install.packages(x)
    }
  }
}

.detect_renv <- function() {
  dir.exists("renv") && file.exists("renv.lock")
}

.install_dep <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    message("Installing package: ", x)
    utils::install.packages(x)
  }
}

#-----------------------------------------------------------------------
# Helper: Clear a directory if requested
#-----------------------------------------------------------------------
.clear_dir <- function(do_clear,
                       path_dir,
                       create_if_nonexistent,
                       path_default) {
  if (!do_clear) {
    if (create_if_nonexistent) {
      dir_create <- if (is.null(path_dir)) path_default else path_dir
      if (!dir.exists(dir_create)) {
        dir.create(dir_create, recursive = TRUE)
      }
    }
    return(invisible(FALSE))
  }
  path_dir <- if (is.null(path_dir)) path_default else path_dir
  if (dir.exists(path_dir)) {
    unlink(path_dir, recursive = TRUE)
  }
  dir.create(path_dir, recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}

#-----------------------------------------------------------------------
# Helper: Get scripts to run (skip .qmd if _quarto.yml is present)
#-----------------------------------------------------------------------
.get_scripts_run <- function(scripts, dir_scripts) {
  dir_scripts <- if (is.null(dir_scripts)) getwd() else dir_scripts

  scripts <- if (is.null(scripts)) {
    .get_scripts_run_null(dir_scripts)
  } else {
    .get_scripts_run_non_null(dir_scripts, scripts)
  }
  
  # Check if any scripts are available to run
  .check_scripts_available_to_run(scripts)

  # If a Quarto project is found, skip individually running .qmd
  scripts <- .rm_scripts_qmd_if_project(scripts)

  scripts
}

.get_scripts_run_null <- function(dir_scripts) {
  # collect all .R, .Rmd/.rmd, .qmd
  pattern <- "\\.R$|\\.rmd$|\\.Rmd$"
  if (!file.exists("_quarto.yml")) {
    pattern <- paste0(pattern, "|\\.qmd$")
  }
  list.files(
    dir_scripts,
    full.names = TRUE, pattern = pattern
  )
}

.get_scripts_run_non_null <- function(dir_scripts, scripts) {
  stopifnot(is.character(scripts))
  scripts <- setdiff(scripts, "")
  stopifnot(length(scripts) > 0L)
  fn <- file.path(dir_scripts, scripts)
  fn_non_existent <- fn[!file.exists(fn)]
  if (length(fn_non_existent) > 0) {
    stop(
      "The following script(s) do not exist: ",
      paste(fn_non_existent, collapse = ", "))
  }
  fn
}

.check_scripts_available_to_run <- function(scripts) {
  if (length(scripts) == 0L && !file.exists("_quarto.yml")) {
    stop(
      "Nothing to run: no scripts specified/found and/or no quarto project found" # nolint
    )
  }
}

.rm_scripts_qmd_if_project <- function(scripts) {
  # If a Quarto project is found, skip individually running .qmd
  if (file.exists("_quarto.yml") && length(scripts) > 0) {
    qmd_scripts <- scripts[grepl("\\.qmd$", scripts, ignore.case = TRUE)]
    if (length(qmd_scripts) > 0) {
      warning(
        "Quarto project detected (_quarto.yml exists), so .qmd scripts ",
        "are not run individually."
      )
      scripts <- setdiff(scripts, qmd_scripts)
    }
  }
  scripts
}

#-----------------------------------------------------------------------
# Helper: Identify script type (R, qmd, Rmd/rmd)
#-----------------------------------------------------------------------
.get_script_type <- function(script) {
  if (grepl("\\.R$", script, ignore.case = FALSE)) {
    "R"
  } else if (grepl("\\.qmd$", script, ignore.case = TRUE)) {
    "qmd"
  } else if (grepl("\\.Rmd$", script, ignore.case = TRUE)) {
    "rmd"
  } else {
    stop("Unknown script type for file: ", script)
  }
}

#-----------------------------------------------------------------------
# Helper: Run a single script based on type
#-----------------------------------------------------------------------
.run_script <- function(script, dir_exec) {
  message("Running script: ", script)
  switch(
    .get_script_type(script),
    R   = {
      if (!is.null(dir_exec)) {
        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(dir_exec)
      }
      source(script)
    },
    qmd = {
      if (!is.null(dir_exec)) {
        quarto::quarto_render(script, execute_dir = dir_exec)
      } else {
        quarto::quarto_render(script)
      }
    },
    rmd = {
      if (!is.null(dir_exec)) {
        rmarkdown::render(script, knit_root_dir = dir_exec)
      } else {
        rmarkdown::render(script)
      }
    }
  )
}

#-----------------------------------------------------------------------
# Helper: Actually copy docs (html/pdf/docx) + resource dirs (e.g. _files)
#-----------------------------------------------------------------------
.copy_docs_actual <- function(script, dir_docs) {
  # Identify base script name (no extension)
  path_dir     <- dirname(script)
  script_base  <- gsub("\\.qmd$|\\.Rmd$|\\.rmd$", "", basename(script))

  .copy_docs_actual_doc(script_base, path_dir, dir_docs)

  if (!grepl("\\.qmd$", script)) {
    return(invisible(TRUE))
  }

  .copy_docs_actual_dir_files(script_base, path_dir, dir_docs)
}

.copy_docs_actual_doc <- function(script_base,
                                  path_dir,
                                  dir_docs) {
  if (grepl("\\.R$", script_base)) {
    return(invisible(FALSE))
  }
  # Copy files like script_base.html, script_base.pdf, script_base.docx
  pattern_fn   <- paste0("^", script_base, "\\.(html|pdf|docx)$")
  path_fn      <- list.files(path_dir, pattern = pattern_fn, full.names = TRUE)
  if (length(path_fn) == 0L) {
    warning("No docs found for script: ", script_base)
    return(invisible(FALSE))
  }
  for (f in path_fn) {
    path_to <- file.path(dir_docs, basename(f))
    if (file.exists(path_to)) {
      invisible(file.remove(path_to))
    }
    file.rename(f, file.path(dir_docs, basename(f)))
  }
  invisible(TRUE)
} 

.copy_docs_actual_dir_files <- function(script_base,
                                        path_dir,
                                        dir_docs) {

  # Copy resource directories that Quarto uses (scriptname_files)
  path_dir_resource    <- file.path(path_dir, paste0(script_base, "_files"))
  if (!dir.exists(path_dir_resource)) {
    return(invisible(TRUE))
  }
  fn_vec <- list.files(path_dir_resource, recursive = TRUE)
  if (length(fn_vec) == 0) {
    unlink(path_dir_resource)
    return(invisible(TRUE))
  }
  dir_vec <- dirname(fn_vec) |> unique()
  for (i in seq_along(dir_vec)) {
    path_dir_curr <- file.path(dir_docs, dir_vec[[i]])
    if (!dir.exists(path_dir_curr)) {
      dir.create(path_dir_curr, recursive = TRUE)
    }
  }
  for (f in fn_vec) {
    file.rename(file.path(path_dir_resource, f), file.path(dir_docs, f))
  }
  unlink(path_dir_resource, recursive = TRUE)
}

#-----------------------------------------------------------------------
# Helper: Copy docs if requested (skip if Quarto project)
#-----------------------------------------------------------------------
.copy_docs <- function(script, do_copy, dir_docs) {
  if (!do_copy) {
    return(invisible(FALSE))
  }
  if (file.exists("_quarto.yml")) {
    # skip if it's a quarto project, as Quarto does its own doc copying
    return(invisible(FALSE))
  }
  dir_docs <- if (is.null(dir_docs)) "docs" else dir_docs
  if (!dir.exists(dir_docs)) {
    dir.create(dir_docs, recursive = TRUE)
  }
  .copy_docs_actual(script, dir_docs)
  invisible(TRUE)
}

#-----------------------------------------------------------------------
# Helper: Optionally render Quarto project if _quarto.yml is present
#-----------------------------------------------------------------------
.run_quarto_project <- function(scripts, skip_quarto_project) {
  if (!file.exists("_quarto.yml")) {
    return(invisible(FALSE))
  }
  if (skip_quarto_project) {
    message("Skipping quarto project rendering.")
    return(invisible(FALSE))
  }
  message("Rendering entire Quarto project (_quarto.yml).")
  quarto::quarto_render()
  invisible(TRUE)
}