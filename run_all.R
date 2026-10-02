# Reconstruct publication exhibits from aggregates; never fit record-level models.
# Terminal: Rscript --vanilla run_all.R [new-output-directory]
# RStudio: source("run_all.R"); reproduce_publication()

publication_output_path <- function(output, repository) {
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    stop("Supply one nonempty output directory.")
  }
  parts <- strsplit(gsub("\\\\", "/", output), "/", fixed = TRUE)[[1]]
  if (".." %in% parts) stop("Output paths must not contain '..'; use an explicit destination.")
  absolute <- grepl("^(/|[A-Za-z]:[/\\\\])", output)
  destination <- if (absolute) output else file.path(repository, output)
  parent <- destination
  suffix <- character()
  while (!dir.exists(parent)) {
    if (file.exists(parent)) stop("Output destination or one of its parents is a file.")
    suffix <- c(basename(parent), suffix)
    next_parent <- dirname(parent)
    if (identical(parent, next_parent)) stop("Cannot resolve output destination.")
    parent <- next_parent
  }
  destination <- do.call(file.path, as.list(c(normalizePath(parent, winslash = "/"), suffix)))
  inside <- identical(destination, repository) || startsWith(destination, paste0(repository, "/"))
  output_root <- file.path(repository, "output")
  allowed <- identical(destination, output_root) || startsWith(destination, paste0(output_root, "/"))
  if (inside && !allowed) stop("Within the repository, write only under output/. Sources and reference tables are read-only.")
  if (dir.exists(destination) && length(list.files(destination, all.files = TRUE, no.. = TRUE))) {
    stop("Output is not empty. Choose a new directory; existing results are never deleted.")
  }
  destination
}

publication_file_manifest <- function(paths, root) {
  paths <- sort(unique(paths))
  data.frame(path = substring(paths, nchar(root) + 2L),
             bytes = unname(file.info(paths)$size),
             md5 = unname(tools::md5sum(paths)), stringsAsFactors = FALSE)
}

reproduce_publication <- function(output = "output/2026-10-02", repository = getwd()) {
  repository <- normalizePath(repository, winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(repository, "publication/source_manifest.csv"))) {
    stop("Repository inputs are missing. Supply the directory containing run_all.R as repository=.")
  }
  destination <- publication_output_path(output, repository)
  previous <- setwd(repository)
  on.exit(setwd(previous), add = TRUE)
  started <- Sys.time()
  source("R/tables_october.R", local = TRUE)

  # Record reconstruction code/aggregate inputs, never external birth records.
  inputs <- c("run_all.R", "tests/check_publication.R", list.files(
    c("R", "publication", "results"), recursive = TRUE, full.names = TRUE, all.files = TRUE))
  inputs <- inputs[!dir.exists(inputs)]
  input_manifest <- publication_file_manifest(file.path(repository, inputs), repository)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(destination, "tables"))
  dir.create(file.path(destination, "figures"))
  tables <- build_october_tables()
  for (name in names(tables)) {
    expected <- oct_matrix(file.path("publication/reference_tables", paste0(name, ".csv")))
    actual <- tables[[name]]
    if (!identical(dim(actual), dim(expected)) || !all(actual == expected)) {
      if (identical(dim(actual), dim(expected))) {
        differences <- which(actual != expected, arr.ind = TRUE)
        for (i in seq_len(min(nrow(differences), 8L))) {
          cell <- differences[i, ]
          message(name, " row ", cell[1], " col ", cell[2], ": ",
                  actual[cell[1], cell[2]], " != ", expected[cell[1], cell[2]])
        }
      }
      stop("Reconstructed table differs from the approved reference: ", name)
    }
    write.table(actual, file.path(destination, "tables", paste0(name, ".csv")),
                sep = ",", row.names = FALSE, col.names = FALSE, qmethod = "double", na = "")
  }
  stopifnot(file.copy("publication/table_metadata.csv", file.path(destination, "tables/table_metadata.csv")))
  source("publication/Code/reproduce_figures.R", local = TRUE)
  reproduce_figures("publication", file.path(destination, "figures"))
  source("tests/check_publication.R", local = TRUE)
  check_publication(destination)
  if (!identical(input_manifest, publication_file_manifest(file.path(repository, inputs), repository))) {
    stop("A reconstruction input changed during the run; no successful receipt will be written.")
  }

  # A successful receipt is written only after all reconstruction checks pass.
  write.csv(input_manifest, file.path(destination, "reconstruction_inputs.csv"), row.names = FALSE)
  git <- Sys.which("git")
  commit <- if (nzchar(git) && file.exists(".git"))
    system2(git, c("rev-parse", "HEAD"), stdout = TRUE) else "not available in exported source tree"
  status <- if (nzchar(git) && file.exists(".git"))
    system2(git, c("status", "--porcelain"), stdout = TRUE) else "not checked without Git metadata"
  receipt <- data.frame(
    field = c("status", "mode", "manuscript_snapshot", "code_commit", "working_tree",
              "started_utc", "finished_utc", "elapsed_seconds", "tables", "figures", "estimation_rerun"),
    value = c("passed", "public aggregate reconstruction", "2026-10-01 plus approved Table 3 revision",
              commit, if (!length(status)) "clean" else paste(status, collapse = "; "),
              format(started, tz = "UTC", usetz = TRUE), format(Sys.time(), tz = "UTC", usetz = TRUE),
              format(as.numeric(difftime(Sys.time(), started, units = "secs")), digits = 7),
              length(tables), "6", "no"), stringsAsFactors = FALSE)
  write.csv(receipt, file.path(destination, "run_receipt.csv"), row.names = FALSE)
  writeLines(c("27 table files and 6 figures rebuilt from approved aggregates.",
               "Table cells match the reference files; this is not record-level estimation.",
               capture.output(sessionInfo())), file.path(destination, "session_info.txt"))
  generated <- list.files(destination, recursive = TRUE, full.names = TRUE)
  write.csv(publication_file_manifest(generated, destination),
            file.path(destination, "output_manifest.csv"), row.names = FALSE)
  message("PASS: publication reconstruction and numerical checks. Output: ", destination)
  invisible(destination)
}

if (sys.nframe() == 0L) {
  arguments <- commandArgs(trailingOnly = TRUE)
  reproduce_publication(if (length(arguments)) arguments[1] else "output/2026-10-02")
}
