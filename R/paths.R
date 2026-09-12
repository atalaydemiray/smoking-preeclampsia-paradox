# Project paths. Every path in this repository is relative to the project root, so the code
# runs unchanged wherever the repository is cloned.
#
# The project root is the directory that holds run_all.R. Scripts are expected to be run
# from there, either through run_all.R or with Rscript from the root.

project_root <- function() {
  root <- getwd()
  for (i in 1:5) {
    if (file.exists(file.path(root, "run_all.R"))) return(normalizePath(root, winslash = "/"))
    root <- dirname(root)
  }
  stop("Run this from the repository root, the directory that contains run_all.R.")
}

# results/  holds the estimates the manuscript reports. They are written by the code in
#           model-fitting/ and are shipped with the repository so that every table and figure
#           can be rebuilt without the source data files.
res_path <- function(...) file.path(project_root(), "results", ...)

# output/   holds what this repository rebuilds: the tables and figures of the manuscript.
out_path <- function(...) file.path(project_root(), "output", ...)

read_result <- function(...) {
  path <- res_path(...)
  if (!file.exists(path)) stop("missing result file: ", sub(project_root(), "", path, fixed = TRUE))
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

write_table_csv <- function(rows, name) {
  dir.create(out_path("tables"), recursive = TRUE, showWarnings = FALSE)
  path <- out_path("tables", paste0(name, ".csv"))
  utils::write.table(as.data.frame(rows, stringsAsFactors = FALSE), path,
                     sep = ",", row.names = FALSE, col.names = FALSE, qmethod = "double", na = "")
  invisible(path)
}
