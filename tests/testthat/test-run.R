# tests/testthat/test-projr_run.R

library(testthat)
library(withr)

test_that("Running an R script by itself", {
  with_tempdir({
    # Create minimal "scripts" directory and an R script
    dir.create("scripts", showWarnings = FALSE)
    dir.create("_output", showWarnings = FALSE)
    writeLines(
      'file.create("_output/test_r_script_output.txt")',
      con = file.path("scripts", "test_script.R")
    )

    # Run the function
    expect_true(projr_run(
      scripts = NULL,             # will pick up R script from "scripts"
      skip_quarto_project = TRUE, # no Quarto rendering
      clear_output_and_docs = FALSE,
      copy_docs = FALSE,
      dir_scripts = "scripts",
      dir_exec = getwd()
    ))

    expect_true(file.exists("_output/test_r_script_output.txt"))

    # Check that the script was run: no direct output unless we tested side effects
    # A minimal side effect is a printed message, but we won't capture it here.
    # If you want to verify it actually ran, you could have the R script
    # create a file and then check it below. For example:
    # writeLines("writeLines('script has run', 'test_r_script_output.txt')", "scripts/test_script.R")
    # Then:
    # expect_true(file.exists("test_r_script_output.txt"))
  })
})

test_that("Running an Rmd by itself", {
  with_tempdir({
    dir.create("scripts", showWarnings = FALSE)
    # A simple Rmd that should produce an HTML file
    writeLines(
      c(
        '---',
        'title: Test Rmd',
        'output: html_document',
        '---',
        '',
        '```{r}',
        'dir.create("_output", showWarnings = FALSE)',
        'file.create("_output/test_r_script_output.txt")',
        '```',
        ''
      ),
      con = file.path("scripts", "test_doc.Rmd")
    )

    expect_true(
      projr_run(
        scripts = NULL,  # pick up from "scripts"
        skip_quarto_project = TRUE,
        clear_output_and_docs = FALSE,
        dir_scripts = "scripts",
        dir_exec = getwd(),
        copy_docs = TRUE  # Will attempt to copy generated .html into 'docs'
      )
    )
    expect_true(file.exists("_output/test_r_script_output.txt"))

    # Check 'docs' directory to confirm the HTML file is there
    expect_true(dir.exists("docs"))
    html_file <- file.path("docs", "test_doc.html")
    expect_true(file.exists(html_file))
  })
})

test_that("Running a qmd that creates NO `_files` directory", {
  with_tempdir({
    dir.create("scripts", showWarnings = FALSE)
    writeLines(
      c(
        '---',
        'title: Test qmd',
        'format:',
        '  html:',
        '    embed-resources: true',
        '---',
        '```{r}',
        'dir.create("_output", showWarnings = FALSE)',
        'file.create("_output/test_r_script_output.txt")',
        '```',
        ''
      ),
      con = file.path("scripts", "no_files.qmd")
    )


    # If no _quarto.yml, the .qmd will be rendered individually
    expect_true(
      projr_run(
        scripts = NULL,
        copy_docs = TRUE,
        dir_scripts = "scripts",
        dir_exec = getwd()
      )
    )

    expect_true(file.exists(file.path("docs", "no_files.html")))
    # No `_files` directory, so nothing to check there
    expect_false(dir.exists(file.path("docs", "no_files_files")))
    
    
    expect_true(file.exists("_output/test_r_script_output.txt"))

  })
})

test_that("Running a qmd that DOES create a `_files` directory", {
  with_tempdir({
    dir.create("scripts")
    writeLines(
      c(
        '---',
        'title: Test qmd',
        'format:',
        '  html:',
        '    embed-resources: false',
        '---',
        '```{r}',
        'dir.create("_output", showWarnings = FALSE)',
        'file.create("_output/test_r_script_output.txt")',
        '```',
        ''
      ),
      con = file.path("scripts", "no_files.qmd")
    )


    # If no _quarto.yml, the .qmd will be rendered individually
    expect_true(
      projr_run(
        scripts = NULL,
        copy_docs = TRUE,
        dir_scripts = "scripts",
        dir_exec = getwd()
      )
    )

    expect_true(file.exists(file.path("docs", "no_files.html")))
    # No `_files` directory, so nothing to check there
    expect_false(dir.exists(file.path("docs", "no_files_files")))
    expect_true(file.exists("_output/test_r_script_output.txt"))
  })
})


test_that("Running an R script, Rmd, and qmd together", {
  with_tempdir({
    dir.create("scripts", showWarnings = FALSE)

    # 1) Minimal R script => writes a file into _output
    writeLines(
      c('dir.create("_output", showWarnings = FALSE); file.create("_output/test_r.txt")', ''),
      file.path("scripts", "a_script.R")
    )

    # 2) Rmd => writes a file into _output and produces an .html
    writeLines(
      c(
        "---",
        "title: Mixed Rmd",
        "output: html_document",
        "---",
        "",
        "```{r}",
        'dir.create("_output", showWarnings = FALSE)',
        'file.create("_output/test_rmd.txt")',
        "plot(1,1)",
        "```",
        ""
      ),
      file.path("scripts", "b_doc.Rmd")
    )

    # 3) Qmd => writes a file into _output and produces an .html
    writeLines(
      c(
        "---",
        "title: Mixed Qmd",
        "format: html",
        "---",
        "",
        "```{r}",
        'dir.create("_output", showWarnings = FALSE)',
        'file.create("_output/test_qmd.txt")',
        "cat('Hello from Qmd')",
        "```",
        ""
      ),
      file.path("scripts", "c_doc.qmd")
    )

    # Run them all (no _quarto.yml => .qmd is rendered individually)
    expect_true(
      projr_run(
        scripts = NULL,
        skip_quarto_project = FALSE,
        clear_output_and_docs = FALSE,
        copy_docs = TRUE,
        dir_scripts = "scripts",
        dir_exec = getwd()
      )
    )

    # Check side effect files from each script
    expect_true(file.exists("_output/test_r.txt"))
    expect_true(file.exists("_output/test_rmd.txt"))
    expect_true(file.exists("_output/test_qmd.txt"))

    # Check docs for generated HTML from the Rmd and Qmd
    expect_true(dir.exists("docs"))
    expect_true(file.exists(file.path("docs", "b_doc.html")))
    expect_true(file.exists(file.path("docs", "c_doc.html")))
  })
})

test_that("Running a Quarto project (nothing else) - _quarto.yml only", {
  skip_if_not_installed("quarto")

  with_tempdir({
    # Put a _quarto.yml in the working directory, no scripts in 'scripts'
    writeLines("project:\n  type: website\n", "_quarto.yml")
    dir.create("scripts", showWarnings = FALSE)
    writeLines(
      c(
        "---",
        "title: Mixed Qmd",
        "format: html",
        "---",
        "",
        "```{r}",
        'dir.create("_output", showWarnings = FALSE)',
        'file.create("_output/test_qmd.txt")',
        "cat('Hello from Qmd')",
        "```",
        ""
      ),
      "c_doc.qmd"
    )

    # We expect no individual scripts to run, but Quarto to render (though
    # we have no .qmd in this minimal example). We'll just ensure it doesn't
    # error out.
    expect_true(
      projr_run(
        scripts = NULL,
        skip_quarto_project = FALSE,
        clear_output_and_docs = FALSE,
        copy_docs = TRUE,
        # no scripts -> dir_scripts is irrelevant here, but we can set it:
        dir_scripts = NULL,
        dir_exec = getwd()
      )
    )

    expect_true(dir.exists("_site"))
    expect_true(file.exists("_output/test_qmd.txt"))
    # If you had an index.qmd or something, you'd check for its output in _site/,
    # docs/, or wherever Quarto is configured to put it.
  })
})

test_that("Running a Quarto project + an R script", {
  skip_if_not_installed("quarto")

  with_tempdir({
    # Quarto project in working directory
    # Put a _quarto.yml in the working directory, no scripts in 'scripts'
    writeLines("project:\n  type: website\n", "_quarto.yml")
    dir.create("scripts", showWarnings = FALSE)
    writeLines(
      c(
        "---",
        "title: Mixed Qmd",
        "format: html",
        "---",
        "",
        "```{r}",
        'dir.create("_output", showWarnings = FALSE)',
        'file.create("_output/test_qmd.txt")',
        "cat('Hello from Qmd')",
        "```",
        ""
      ),
      "c_doc.qmd"
    )


    # Also an R script in "scripts"
    dir.create("scripts", showWarnings = FALSE)
    dir.create("_output", showWarnings = FALSE)
    writeLines(
      'file.create("_output/test_r_script_output.txt")',
      file.path("scripts", "my.R")
    )

    # This should run the R script individually, then run the Quarto project.
    expect_true(
      projr_run(
        scripts = NULL,
        skip_quarto_project = FALSE,
        clear_output_and_docs = TRUE,
        copy_docs = TRUE,
        dir_scripts = "scripts",
        dir_exec = getwd()
      )
    )

    # Check that clearing occurred => both _output and docs recreated
    expect_true(dir.exists("_output"))
    expect_true(dir.exists("_site"))

    # The R script should have created the file in _output
    expect_true(file.exists("_output/test_r_script_output.txt"))
    expect_true(file.exists("_output/test_qmd.txt"))
    # Quarto was rendered (though there's no .qmd doc to produce new output).
  })
})

test_that("Quarto project exists, but we skip it; something else to run", {
  skip_if_not_installed("quarto")

  with_tempdir({
    writeLines("project:\n  type: website\n", "_quarto.yml")

    dir.create("scripts", showWarnings = FALSE)
    dir.create("_output", showWarnings = FALSE)
    # R script present => we'll see if it still runs
    writeLines(
      'file.create("_output/test_r_script_output.txt")',
      file.path("scripts", "test.R")
    )
    writeLines(
      c(
        "---",
        "title: Mixed Qmd",
        "format: html",
        "---",
        "",
        "```{r}",
        'dir.create("_output", showWarnings = FALSE)',
        'file.create("_output/test_qmd.txt")',
        "cat('Hello from Qmd')",
        "```",
        ""
      ),
      "c_doc.qmd"
    )


    # Skipping Quarto means it won't render the project, but it will run the
    # R script if present
    expect_true(
      projr_run(
        scripts = NULL,
        skip_quarto_project = TRUE,
        clear_output_and_docs = FALSE,
        copy_docs = FALSE,
        dir_scripts = "scripts",
        dir_exec = getwd()
      )
    )

    expect_true(file.exists("_output/test_r_script_output.txt"))
  })
})

test_that("Scripts specified (no auto-detect)", {
  with_tempdir({
    dir.create("scripts", showWarnings = FALSE)
    # Minimal R script
    dir.create("_output", showWarnings = FALSE)
    writeLines(
      'file.create("_output/test_specified.txt")',
      file.path("scripts", "spec.R")
    )

    # Because we explicitly supply scripts="spec.R", no other files are run
    expect_true(
      projr_run(
        scripts = "spec.R",
        skip_quarto_project = TRUE,
        clear_output_and_docs = FALSE,
        copy_docs = FALSE,
        dir_scripts = "scripts",
        dir_exec = getwd()
      )
    )

    # Check that the script was actually run
    expect_true(file.exists("_output/test_specified.txt"))
  })
})

test_that("Qmd specified, but quarto project exists", {
  skip_if_not_installed("quarto")
  
  with_tempdir({
    # Create a Quarto project by adding _quarto.yml in the working directory.
    writeLines("project:\n  type: website\n", "_quarto.yml")
    dir.create("scripts", showWarnings = FALSE)
    # Write a Qmd script that would normally create a file if run individually.
    writeLines(
      c(
        "---",
        "title: Ignored Qmd",
        "format: html",
        "---",
        "",
        "```{r}",
        'file.create("_output/test_qmd_specified.txt")',
        "```",
        ""
      ),
      con = file.path("scripts", "ignore_me.qmd")
    )
    
    # Because a Quarto project exists, the function should warn that .qmd
    # scripts are not run individually.
    expect_warning(
      projr_run(
        scripts = "ignore_me.qmd",
        skip_quarto_project = FALSE,
        clear_output_and_docs = FALSE,
        copy_docs = TRUE,
        dir_scripts = "scripts",
        dir_exec = getwd()
      ),
      "Quarto project detected"
    )
    
    # The Qmd script should not be run individually, so the file should not exist.
    expect_false(file.exists("_output/test_qmd_specified.txt"))
  })
})

test_that("Clearing output, not clearing docs, etc.", {
  with_tempdir({
    dir.create("scripts", showWarnings = FALSE)
    dir.create("_output", showWarnings = FALSE)
    # Minimal R script
    writeLines(
      'file.create("_output/clear_test.txt")',
      file.path("scripts", "clear_test.R")
    )

    # By default, dir_output = "_output", dir_docs = "docs"
    dir.create("_output", showWarnings = FALSE)
    writeLines("some file", file.path("_output", "keep_me.txt"))
    dir.create("docs", showWarnings = FALSE)
    writeLines("some doc file", file.path("docs", "doc_keep.txt"))

    # The function's single `clear_output_and_docs` arg clears BOTH `_output` and
    # `docs` if `TRUE`. So "We only clear output, not docs" isn't possible unless
    # you add separate arguments. This test thus checks that *both* got cleared.
    expect_true(
      projr_run(
        scripts = NULL,
        skip_quarto_project = TRUE,
        clear_output_and_docs = TRUE,  # Clears both by design
        copy_docs = FALSE,
        dir_scripts = "scripts",
        dir_exec = getwd()
      )
    )

    # Check that _output is recreated and empty except for the newly created file
    expect_true(dir.exists("_output"))
    expect_false(file.exists(file.path("_output", "keep_me.txt")))
    # The new script ran => clear_test.txt should exist
    expect_true(file.exists(file.path("_output", "clear_test.txt")))

    # Check that docs is recreated and empty
    expect_true(dir.exists("docs"))
    expect_false(file.exists(file.path("docs", "doc_keep.txt")))
  })
})

test_that("Not clearing output", {
  with_tempdir({
    dir.create("scripts", showWarnings = FALSE)
    dir.create("_output", showWarnings = FALSE)
    writeLines(
      'file.create("_output/no_clear_script.txt")',
      file.path("scripts", "noclear.R")
    )

    # Create an existing file in _output
    writeLines("keep me!", file.path("_output", "dont_remove.txt"))

    # Since clear_output_and_docs = FALSE, the existing file should remain.
    expect_true(
      projr_run(
        scripts = NULL,
        skip_quarto_project = TRUE,
        clear_output_and_docs = FALSE,
        copy_docs = FALSE,
        dir_scripts = "scripts",
        dir_exec = getwd()
      )
    )
    expect_true(file.exists(file.path("_output", "dont_remove.txt")))
    expect_true(file.exists(file.path("_output", "no_clear_script.txt")))
  })
})


test_that("Copying to docs vs not copying", {
  with_tempdir({
    dir.create("scripts", showWarnings = FALSE)
    # Minimal Rmd that produces an HTML file.
    writeLines(
      c(
        "---",
        "title: Copy doc test",
        "output: html_document",
        "---",
        "",
        "```{r}",
        'dir.create("_output", showWarnings = FALSE)',
        'file.create("_output/copy_doc_ran.txt")',
        "```",
        ""
      ),
      con = file.path("scripts", "copy_doc.Rmd")
    )

    # Case 1: copy_docs = FALSE. The docs directory should be (re)created,
    # but no HTML file should be copied there.
    expect_true(
      projr_run(
        scripts = "copy_doc.Rmd",
        skip_quarto_project = TRUE,
        clear_output_and_docs = TRUE,
        copy_docs = FALSE,
        dir_scripts = "scripts",
        dir_exec = getwd()
      )
    )

    expect_false(file.exists(file.path("docs", "copy_doc.html")))
    expect_true(file.exists("_output/copy_doc_ran.txt"))

    # Case 2: copy_docs = TRUE. The generated HTML should be copied to docs.
    expect_true(
      projr_run(
        scripts = "copy_doc.Rmd",
        skip_quarto_project = TRUE,
        clear_output_and_docs = FALSE,
        copy_docs = TRUE,
        dir_scripts = "scripts",
        dir_exec = getwd()
      )
    )
    expect_true(file.exists(file.path("docs", "copy_doc.html")))
  })
})

test_that("Setting scripts directory, output directory, docs directory", {
  with_tempdir({
    dir.create("my_scripts", showWarnings = FALSE)
    dir.create("my_outdir", showWarnings = FALSE)
    file.create("my_outdir/to_remove.txt")
    dir.create("my_docs", showWarnings = FALSE)
    file.create("my_outdir/to_remove.txt")

    # Minimal R script
    writeLines(
      'file.create("my_outdir/test_custom_dirs.txt")',
      file.path("my_scripts", "custom.R")
    )
    # Minimal Rmd
    writeLines(
      c(
        "---",
        "title: Copy doc test",
        "output: html_document",
        "---",
        "",
        "```{r}",
        'dir.create("my_outdir", showWarnings = FALSE)',
        'file.create("my_outdir/test.Rmd")',
        "```",
        ""
      ),
      con = file.path("my_scripts", "copy_doc.Rmd")
    )


    # The default `_output` or `docs` won't be used because we override them
    expect_true(
      projr_run(
        scripts = NULL,
        skip_quarto_project = TRUE,
        clear_output_and_docs = TRUE,
        copy_docs = TRUE,
        dir_scripts = "my_scripts",
        dir_output = "my_outdir",
        dir_docs = "my_docs",
        dir_exec = getwd()
      )
    )

    expect_false(file.exists("my_outdir/to_remove.txt"))
    expect_false(file.exists("my_docs/to_remove.txt"))

    expect_true(file.exists("my_outdir/test_custom_dirs.txt"))
    expect_true(file.exists("my_outdir/test.Rmd"))
    expect_true(file.exists("my_docs/copy_doc.html"))
  })
})

