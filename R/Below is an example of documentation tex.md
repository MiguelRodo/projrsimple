Below is an example of documentation text you might include in your README (or a separate user guide) to explain how to use projr_init and projr_run. You can adjust the wording as needed:

---

## Using projrsimple

### Project Initialisation with `projr_init`

The function `projr_init` is designed to help you quickly set up a standard project  
structure. It will (all of these features are optional):

- **Create Standard Directories:**  
  By default, it creates directories for:
  - **Raw data:** (default: `_raw_data`)
  - **Temporary/cache files:** (default: `_tmp`)
  - **Rendered documents:** (default: `docs`)
  - **Final outputs:** (default: `_output`)
  - **Raw (non‐rendered) docs:** (default: `_raw_docs`)  
  All of these directory names are fully customizable by supplying your own values  
  (or you can set any to `NULL` if you don’t want that directory).

- **Initialise a Git Repository:**  
  If you choose, `projr_init` can initialise a Git repository in your project  
  (using `usethis::use_git()` and `gert::git_init()`). It will automatically add the  
  cache directory (by default `_tmp`) to your `.gitignore` to avoid committing  
  temporary files, and (if enabled) will add and commit any changes.  
  **Note:** To push your repository to GitHub, you’ll need a GitHub account and must  
  ensure that R is configured with your personal access token (PAT).

- **Create a README File:**  
  A default README is created with instructions on how to run your analyses,  
  reproduce the results, and a description of the directory structure. You can  
  customise or remove this by setting the corresponding parameter to `FALSE`.

- **Connect to GitHub (Optional):**  
  If desired, you can instruct `projr_init` to connect your local Git repository  
  to GitHub (using `usethis::use_github()`). The repository can be created as private  
  by default, with a message reminding you that you must add collaborators manually  
  via GitHub’s settings.

### Running Analyses with `projr_run`

After you have initialised your project, you can use `projr_run` to execute your  
analyses. By default:

- **Script Detection and Execution:**  
  All scripts in your working directory with extensions `.R`, `.Rmd`/`.rmd`, and  
  `.qmd` are run automatically. In the presence of a Quarto project (a file named  
  `_quarto.yml`), individual `.qmd` files may be skipped in favour of running the  
  project as a whole.

- **Copying Generated Documents:**  
  The function will copy any generated documents (typically `.html`, `.pdf`, or  
  `.docx`) to the `docs` directory. This behavior is optional and customizable.

- **Output Directory Management:**  
  Optionally, the output directory (by default `_output`) is cleared before running  
  all the scripts. This ensures that you always work with a clean slate, though you  
  can disable this if you wish.

- **Selective Execution:**  
  If you prefer, you can select which scripts to run by providing a character  
  vector of filenames. This enables you to run only a subset of your scripts.

### Advanced Usage

For more advanced workflows, you can run `projr_run` multiple times to create  
separate "pipelines." For example, you might run one pipeline that processes raw data  
and another that generates reports, each using different script selections and  
output/document directories. This modular approach allows you to flexibly manage  
complex projects with multiple stages.

---

Place this text in your README (e.g. as part of your README.md) or in a separate  
user guide document (e.g., `USAGE.md`) in the root of your project. This will help  
users understand the full capabilities of projrsimple and how to tailor it to their  
needs.