# Verify imported sources with SHA256 from R's bundled tools package.
# No study records, add-on packages or network access are needed.
check_source_integrity <- function(root) {
  require_check <- function(ok, message) {
    if (!isTRUE(ok)) stop(message, call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  require_check(exists("sha256sum", asNamespace("tools"), inherits = FALSE),
                "Source checks require R 4.6.0 with tools::sha256sum().")
  rows <- read.csv(file.path(root, "publication/source_manifest.csv"),
                   colClasses = "character", check.names = FALSE)
  require_check(nrow(rows) > 0L && all(c("path", "bytes", "sha256") %in% names(rows)) &&
                  !anyNA(rows[c("path", "bytes", "sha256")]), "Invalid source manifest")
  require_check(!anyDuplicated(rows$path), "Duplicate source-manifest paths")
  for (i in seq_len(nrow(rows))) {
    name <- rows$path[i]
    portable <- gsub("\\", "/", name, fixed = TRUE)
    require_check(nzchar(name) && !grepl("^(/|[A-Za-z]:)", portable) &&
                    !".." %in% strsplit(portable, "/", fixed = TRUE)[[1L]],
                  "Source-manifest paths must stay inside the repository")
    path <- file.path(root, name)
    require_check(file.exists(path) && !dir.exists(path) &&
                    isTRUE(Sys.readlink(path) == ""), paste("Missing source or symlink:", name))
    resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
    require_check(startsWith(resolved, paste0(root, "/")),
                  "Source-manifest paths must stay inside the repository")
    require_check(grepl("^[a-f0-9]{64}$", rows$sha256[i]) &&
                    identical(unname(tools::sha256sum(path)), rows$sha256[i]),
                  paste("SHA256 mismatch:", name))
    require_check(grepl("^[0-9]+$", rows$bytes[i]) &&
                    isTRUE(file.info(path)$size == as.numeric(rows$bytes[i])),
                  paste("Size mismatch:", name))
  }
  excluded <- c("rds", "rda", "rdata", "parquet", "feather", "docx", "pdf", "zip")
  for (directory in c("publication", "model-fitting/current")) {
    files <- list.files(file.path(root, directory), recursive = TRUE, all.files = TRUE,
                        full.names = TRUE, no.. = TRUE)
    for (path in files) {
      name <- substring(path, nchar(root) + 2L)
      require_check(!tolower(tools::file_ext(path)) %in% excluded,
                    paste("Excluded release input type:", name))
      require_check(isTRUE(file.info(path)$size < 15000000),
                    paste("Oversized release input:", name))
    }
  }
  refs <- list.files(file.path(root, "publication/reference_tables"), "^Table_.*[.]csv$")
  require_check(length(refs) == 27L, "Expected 27 reference tables")
  cat("PASS:", nrow(rows), "imported-file SHA256 checks; no record-level binary files",
      "or manuscripts in current release inputs.\n")
}

if (sys.nframe() == 0L) {
  script <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(script) != 1L) stop("Run this check with Rscript --vanilla.", call. = FALSE)
  check_source_integrity(file.path(dirname(sub("^--file=", "", script)), ".."))
}
