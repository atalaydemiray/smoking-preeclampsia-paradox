# Fail-closed reconstruction and confidence-set selection tests. No records read.
before <- list.files("output", recursive = TRUE, all.files = TRUE)
source("run_all.R")
source("R/tables_october.R")
stopifnot(identical(before, list.files("output", recursive = TRUE, all.files = TRUE)))
must_fail <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))
root <- normalizePath(getwd(), winslash = "/")
for (path in c(".", "publication/new-output", "R/new-output", "results/new-output", "output/../publication/new-output")) {
  must_fail(publication_output_path(path, root))
}
must_fail(publication_output_path(NA_character_, root))
must_fail(publication_output_path(character(), root))
fresh <- tempfile("publication_contract_")
dir.create(fresh)
writeLines("preserve", file.path(fresh, "sentinel.txt"))
must_fail(publication_output_path(fresh, root))
stopifnot(identical(readLines(file.path(fresh, "sentinel.txt")), "preserve"))
link <- file.path(fresh, "source-link")
if (file.symlink(root, link)) must_fail(publication_output_path(file.path(link, "publication/new-output"), root))
stopifnot(identical(oct_root_component("[41.097, 45.000] U [32.271, 34.860]", 33.3), "[32.271, 34.860]"))
stopifnot(identical(oct_root_component("[20, 25] U (25, 30]", 25), "[20, 25]"))
must_fail(oct_root_component("(20, 25) U (25, 30]", 25))
must_fail(oct_root_component("[20, 30] U [25, 35]", 28))
must_fail(oct_root_component("bad", 28))
must_fail(oct_root_component("20, 30", 28))
must_fail(oct_root_component("[30, 20]", 28))
must_fail(oct_root_component(NA_character_, 28))
tables <- build_october_tables()
stopifnot(ncol(tables$Table_3) == 5L, nrow(tables$Table_3) == 4L,
          !any(grepl("41.097", tables$Table_3, fixed = TRUE)),
          any(grepl("41.097", tables$Table_S19, fixed = TRUE)))
main <- oct_joint("main_summary")
main <- main[match(c("sn_vs_ss", "nn_vs_ss", "n_vs_s"), main$model_id), ]
stopifnot(identical(unname(tables$Table_3[-1, 4]), main$simultaneous_strict_negative),
          identical(unname(tables$Table_3[-1, 5]), main$simultaneous_strict_positive))
cat("PASS: inert sourcing, protected paths, symlink/overwrite guards, interval boundaries and Table 3/S19 scope.\n")
