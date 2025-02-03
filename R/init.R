#' Initialise a New Project with Standard Directories, Git, and README
#'
#' @description
#' `projr_init` creates a standard project structure by making several
#' directories for raw data, temporary/cache files, final outputs, rendered docs,
#' and raw (non-rendered) docs. It also can initialise a Git repository (adding
#' the cache directory to `.gitignore` and optionally adding & committing all
#' changes), write a README file with recommended project instructions, and, if
#' desired, connect the project to GitHub.
#'
#' @param dir_raw_data Character. Directory for raw data. Defaults to
#'   `"_raw_data"`. Set to `NULL` to skip creating this directory.
#' @param dir_cache Character. Directory for temporary or cache files. Defaults to
#'   `"_tmp"`. Set to `NULL` to skip.
#' @param dir_docs Character. Directory for rendered documents. Defaults to
#'   `"docs"`. Set to `NULL` to skip.
#' @param dir_output Character. Directory for final outputs. Defaults to
#'   `"_output"`. Set to `NULL` to skip.
#' @param dir_reference Character. Directory for non-rendered docs. Defaults to
#'   `"_reference"`. Set to `NULL` to skip.
#' @param init_git Logical. If `TRUE` (the default), initialises a Git repository
#'   in the current project.
#' @param git_add_and_commit_all Logical. If `TRUE` (the default), attempts to add
#'   and commit all untracked and modified files after Git initialisation.
#' @param init_readme Logical. If `TRUE` (the default), creates a README file with
#'   recommended project instructions.
#' @param init_github Logical. If `TRUE`, connects the project to GitHub via
#'   `usethis::use_github()`. Defaults to `FALSE`.
#' @param github_private Logical. If `TRUE` (the default) and if GitHub is
#'   initialised (i.e. when `init_github = TRUE`), the GitHub repository is created
#'   as private. A message is issued to remind the user that collaborators must be
#'   added manually via GitHub settings.
#'
#' @return Invisibly returns `TRUE` when complete.
#'
#' @details
#' The default behaviour is to create all standard directories, initialise Git
#' (adding the cache directory to `.gitignore` and committing changes), and write a
#' README. If you do not wish to create one or more of the directories, pass
#' `NULL` for that parameter.
#'
#' @examples
#' \dontrun{
#' projr_init(
#'   dir_raw_data = "_raw_data",
#'   dir_cache = "_tmp",
#'   dir_docs = "docs",
#'   dir_output = "_output",
#'   dir_reference = "_reference",
#'   init_git = TRUE,
#'   git_add_and_commit_all = TRUE,
#'   init_readme = TRUE,
#'   init_github = TRUE,
#'   github_private = TRUE
#' )
#' }
projr_init <- function(dir_raw_data = "_raw_data",
                       dir_cache    = "_tmp",
                       dir_docs     = "docs",
                       dir_output   = "_output",
                       dir_reference = "_reference",
                       init_git     = TRUE,
                       git_add_and_commit_all = TRUE,
                       init_readme  = TRUE,
                       init_github  = FALSE,
                       github_private = TRUE) {

  # Create the standard directories if requested.
  .create_dirs(c(dir_raw_data, dir_cache, dir_docs, dir_output, dir_reference))

  # Initialise Git repository if requested.
  .git_init(init_git, dir_cache, git_add_and_commit_all)
  
  # Write a README file if requested.
  .init_readme(init_readme)
  
  # Connect to GitHub if requested.
  .init_github(init_github, dir_cache, git_add_and_commit_all, github_private)

  invisible(TRUE)
}

.create_dirs <- function(dirs) {
  for (dir in dirs) {
    .create_dir(dir)
  }
}

.create_dir <- function(dirname) {
  if (!is.null(dirname) && !dir.exists(dirname)) {
    dir.create(dirname, recursive = TRUE)
    message("Created directory: ", dirname)
  }
}

.git_init <- function(init_git, dir_cache, git_add_and_commit_all) {
  # Initialise Git repository if requested.
  if (!init_git) {
    return(invisible(FALSE))
  }
  message("Initialising Git repository...")
  # Ensure usethis is available.
  .install_deps_actual("usethis")
  usethis::proj_set(force = TRUE)
  # Initialise Git repository using gert.
  gert::git_init(path = ".")
  .git_init_gitignore()
  if (!is.null(dir_cache)) {
    usethis::proj_set(force = TRUE)
    usethis::use_git_ignore(dir_cache)
    message("Added '", dir_cache, "' to .gitignore.")
  }
  # Ensure that Git user.name and user.email are set.
  .ensure_git_config()
  .git_add_and_commit_all(git_add_and_commit_all)
  invisible(TRUE)
}

# Helper: Ensure Git user configuration is set
# Helper: Ensure Git user configuration is set
.ensure_git_config <- function() {
  # Attempt to get the Git configuration table.
  gitconfig_tbl <- tryCatch(
    gert::git_config(),
    error = function(e) {
      # Return an empty data frame if there's an error
      data.frame(name = character(), value = character(), level = character(), 
                 stringsAsFactors = FALSE)
    }
  )
  
  # Get the latest value for user.name, if any.
  name_cfg <- {
    ind_nm <- which(gitconfig_tbl$name == "user.name")
    if (length(ind_nm) == 0L) NULL else gitconfig_tbl$value[ind_nm][length(ind_nm)]
  }
  
  # Get the latest value for user.email, if any.
  email_cfg <- {
    ind_em <- which(gitconfig_tbl$name == "user.email")
    if (length(ind_em) == 0L) NULL else gitconfig_tbl$value[ind_em][length(ind_em)]
  }
  
  # Prompt for user.name if missing or empty.
  if (is.null(name_cfg) || name_cfg == "") {
    choice <- utils::menu(c("Yes", "No"), 
      title = "Your Git user name is not set. Would you like to set it now?")
    if (choice == 1) {
      user_name <- readline("Please enter your Git user name: ")
      if (nchar(user_name) > 0) {
        gert::git_config_global_set("user.name", user_name)
        message("Git user.name set to: ", user_name)
      } else {
        stop("Git user.name is required for committing changes.")
      }
    } else {
      stop("Git user.name is required. Please configure it and try again.")
    }
  }
  
  # Prompt for user.email if missing or empty.
  if (is.null(email_cfg) || email_cfg == "") {
    choice <- utils::menu(c("Yes", "No"), 
      title = "Your Git user email is not set. Would you like to set it now?")
    if (choice == 1) {
      user_email <- readline("Please enter your Git user email: ")
      if (nchar(user_email) > 0) {
        gert::git_config_global_set("user.email", user_email)
        message("Git user.email set to: ", user_email)
      } else {
        stop("Git user.email is required for committing changes.")
      }
    } else {
      stop("Git user.email is required. Please configure it and try again.")
    }
  }
  invisible(TRUE)
}

.git_init_gitignore <- function() {
  gitignore <- if (file.exists(".gitignore")) {
    readLines(".gitignore")
  } else {
    character(0)
  }
  gitignore <- c(
    gitignore, ".Rproj.user", ".Rhistory", ".Rdata", ".httr-oauth",
    ".DS_Store", ".quarto"
  )
  gitignore <- unique(gitignore)
  writeLines(gitignore, con = ".gitignore")
}

.init_readme <- function(init_readme) {
  if (!init_readme) {
    return(invisible(FALSE))
  }
  readme_contents <- c(
    "# README",
    "",
    "The purpose of this project is to",
    "`[briefly describe the project's goals and objectives]`",
    "",
    "## Reproducing the analysis",
    "",
    "Run the following code from the project working directory:",
    "",
    "```r",
    "if (!requireNamespace(\"projrsimple\", quietly = TRUE)) {",
    "  if (!requireNamespace(\"remotes\", quietly = TRUE)) {",
    "    install.packages(\"remotes\")",
    "  }",
    "  remotes::install_github(\"MiguelRodo/projrsimple\")",
    "}",
    "projrsimple::projr_run()",
    "```",
    "",
    "<!--",
    "Change the above instructions, e.g. specify non-default parameters or",
    "another approach entirely",
    "-->",
    "",
    "## Directory Structure",
    "",
    "This project follows a structured directory layout to organise input",
    "materials, generated outputs, and temporary files efficiently.",
    "",
    "- **`_reference/**: Static documents that are not generated by analyses,",
    "  such as assignment questions and reference materials.",
    "- **`_raw_data/**: Raw, unprocessed data files.",
    "- **`docs/**: Rendered documents produced from R Markdown (`Rmd`) or",
    "  Quarto (`qmd`) scripts.",
    "- **`_output/**: Final analysis results, processed data, and exported",
    "  reports.",
    "- **`_tmp/**: Temporary storage for cached outputs, intermediate",
    "  results, and shared files.",
    "",
    "## Links",
    "",
    "- `[URLs to data sources (e.g. OneDrive), GitHub repos, etc.]`",
    "",
    "## Details",
    "",
    "`[Methods, timeline, team, data sources, software/tools, etc.]`"
  )
  writeLines(readme_contents, con = "README.md")
  message("Created README.md.")
  invisible(TRUE)
}

.init_github <- function(init_github,
                         dir_cache,
                         git_add_and_commit_all,
                         github_private) {
  if (!init_github) {
    return(invisible(FALSE))
  }
  .install_deps_actual("usethis")
  usethis::proj_set(force = TRUE)
  # If Git is not yet initialised, warn and initialise it.
  if (!dir.exists(".git")) {
    message("Git repository not initialised. Initialising now...")
    .git_init(TRUE, dir_cache, git_add_and_commit_all)
  }
  .git_add_and_commit_all(git_add_and_commit_all)
  message("Connecting to GitHub...")
  usethis::use_github(private = github_private)
  if (github_private) {
    message(
      "The repository is private. To add collaborators, please go to the ",
      "repo on GitHub, choose 'Settings' and then 'Collaborators'."
    )
  }
  invisible(TRUE)
}

.git_add_and_commit_all <- function(should_try) {
  if (!should_try || !dir.exists(".git")) {
    return(invisible(FALSE))
  }
  .ensure_git_config()
  add_vec <- c(.git_modified_get(), .git_new_get())
  if (length(add_vec) == 0L) {
    return(invisible(FALSE))
  }
  message("Adding and committing all changes...")
  msg <- "Add and commit all changes"
  .git_commit_file(add_vec, msg)
}

.git_modified_get <- function() {
  git_tbl_status <- gert::git_status()
  git_tbl_status[["file"]][git_tbl_status[["status"]] == "modified"]
}

.git_new_get <- function() {
  git_tbl_status <- gert::git_status()
  git_tbl_status[["file"]][git_tbl_status[["status"]] == "new"]
}

.git_commit_file <- function(file, msg = NULL) {
  if (length(file) == 0L) {
    return(invisible(FALSE))
  }
  gert::git_add(file)
  gert::git_commit(msg)
  message("Committed changes.")
  invisible(TRUE)
}
