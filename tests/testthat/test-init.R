library(testthat)
library(withr)

test_that("projr_init creates all standard directories and README", {
  with_tempdir({
    # Call projr_init with default parameters, but disable GitHub connection.
    projr_init(
      init_git = TRUE,
      init_readme = TRUE,
      init_github = FALSE
    )
    
    # Check that standard directories are created.
    expect_true(dir.exists("_raw_data"))
    expect_true(dir.exists("_tmp"))
    expect_true(dir.exists("docs"))
    expect_true(dir.exists("_output"))
    expect_true(dir.exists("_reference"))
    
    # Check that README.md exists.
    expect_true(file.exists("README.md"))
    
    # Check that a Git repository has been initialised.
    expect_true(dir.exists(".git"))
    
    # Check that the cache directory (_tmp) appears in .gitignore.
    if (file.exists(".gitignore")) {
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("_tmp", gitignore)))
    } else {
      fail(".gitignore was not created.")
    }
  })
})

test_that("projr_init respects NULL settings and readme = FALSE", {
  with_tempdir({
    # Call projr_init with some directory parameters set to NULL and no README.
    projr_init(
      dir_raw_data = NULL,
      dir_cache    = NULL,
      dir_docs     = "docs",
      dir_output   = NULL,
      dir_reference = NULL,
      init_git          = TRUE,
      init_readme       = FALSE,
      init_github       = FALSE
    )
    
    # Check that only the non-NULL directory ('docs') is created.
    expect_true(dir.exists("docs"))
    expect_false(dir.exists("_raw_data"))
    expect_false(dir.exists("_tmp"))
    expect_false(dir.exists("_output"))
    expect_false(dir.exists("_reference"))
    
    # README.md should not exist.
    expect_false(file.exists("README.md"))
  })
})
