# Synthetic release-gate fixtures. No study inputs, records or network access.
check_integrity_contract <- function(source_dir) {
  require_check <- function(ok, message) {
    if (!isTRUE(ok)) stop(message, call. = FALSE)
  }
  root <- tempfile("sep_integrity_fixture_")
  dir.create(file.path(root, "tests"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  checkers <- c("check_source_integrity.R", "check_release_contents.R")
  require_check(all(file.copy(file.path(source_dir, checkers), file.path(root, "tests"))),
                "Could not copy synthetic fixture checkers")
  run_check <- function(name, expected_message = NULL) {
    result <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(file.path(root, "tests", name))), stdout = TRUE, stderr = TRUE))
    status <- attr(result, "status")
    success <- is.null(status) || identical(status, 0L)
    require_check(identical(success, is.null(expected_message)), paste("Unexpected gate status:", name))
    if (!is.null(expected_message)) require_check(any(grepl(expected_message, result, fixed = TRUE)),
      paste("Unexpected failure reason:", name))
  }
  refs <- file.path(root, "publication/reference_tables")
  dir.create(refs, recursive = TRUE)
  for (i in seq_len(27L)) writeLines("label,value\nsynthetic,1", file.path(refs, paste0("Table_fixture_", i, ".csv")))
  input <- file.path(root, "publication/input.csv")
  writeLines("synthetic fixture,not study data", input)
  original <- readBin(input, "raw", n = file.info(input)$size)
  manifest <- file.path(root, "publication/source_manifest.csv")
  row <- data.frame(path = "publication/input.csv", bytes = as.character(file.info(input)$size),
                    sha256 = unname(tools::sha256sum(input)), source = "synthetic")
  put_manifest <- function(rows = row) write.csv(rows, manifest, row.names = FALSE)
  put_manifest()
  run_check(checkers[1L])
  tampered <- original
  tampered[1L] <- charToRaw("X")
  writeBin(tampered, input)
  run_check(checkers[1L], "SHA256 mismatch")
  writeBin(original, input)
  changed <- row
  changed$bytes <- as.character(as.numeric(row$bytes) + 1L)
  put_manifest(changed)
  run_check(checkers[1L], "Size mismatch")
  put_manifest(rbind(row, row))
  run_check(checkers[1L], "Duplicate source-manifest paths")
  for (path in c("../outside.csv", "/outside.csv", "C:/outside.csv")) {
    changed <- row
    changed$path <- path
    put_manifest(changed)
    run_check(checkers[1L], "Source-manifest paths must stay inside the repository")
  }
  put_manifest()
  require_check(file.rename(input, paste0(input, ".hold")), "Could not prepare missing-source fixture")
  run_check(checkers[1L], "Missing source or symlink")
  require_check(file.symlink(paste0(input, ".hold"), input), "Could not prepare symlink fixture")
  run_check(checkers[1L], "Missing source or symlink")
  unlink(input)
  require_check(file.rename(paste0(input, ".hold"), input), "Could not restore fixture")
  excluded <- file.path(root, "publication/unapproved.rds")
  writeLines("synthetic record fixture", excluded)
  run_check(checkers[1L], "Excluded release input type")
  unlink(excluded)
  oversized <- file.path(root, "publication/oversized.txt")
  writeBin(raw(15000000), oversized)
  run_check(checkers[1L], "Oversized release input")
  unlink(oversized)
  require_check(file.rename(file.path(refs, "Table_fixture_27.csv"), file.path(refs, "held.csv")),
                "Could not prepare reference-count fixture")
  run_check(checkers[1L], "Expected 27 reference tables")
  require_check(file.rename(file.path(refs, "held.csv"), file.path(refs, "Table_fixture_27.csv")),
                "Could not restore reference-count fixture")
  run_check(checkers[1L])
  # A temporary index only: no commit, push or modification of the study repo.
  status <- system2("git", c("init", "-q", shQuote(root)), stdout = FALSE, stderr = FALSE)
  require_check(identical(status, 0L), "Could not initialize synthetic Git fixture")
  run_check(checkers[2L])
  excluded_dir <- file.path(root, "data")
  dir.create(excluded_dir)
  writeLines("synthetic record fixture", file.path(excluded_dir, "unapproved.csv"))
  run_check(checkers[2L], "Excluded path")
  unlink(excluded_dir, recursive = TRUE)
  excluded <- file.path(root, "unapproved.RDS")
  writeLines("synthetic record fixture", excluded)
  run_check(checkers[2L], "Excluded file type")
  unlink(excluded)
  candidate <- file.path(root, "candidate.txt")
  patterns <- list(
    "private key" = paste0("-----BEGIN ", "PRIVATE KEY-----"),
    "GitHub token" = paste0("ghp_", strrep("A", 40)),
    "AWS access key" = paste0("AKIA", strrep("A", 16)),
    "OpenAI key" = paste0("sk-", "proj-", strrep("A", 40)),
    "private home path" = paste0("/", "Users", "/synthetic/project")
  )
  for (label in names(patterns)) {
    writeLines(patterns[[label]], candidate)
    run_check(checkers[2L], paste(label, "candidate:"))
  }
  writeBin(as.raw(c(65, 0, 66)), candidate)
  run_check(checkers[2L], "Large/binary file")
  writeBin(as.raw(c(65, 255, 66)), candidate)
  run_check(checkers[2L], "Invalid UTF-8 file")
  writeBin(raw(15000000), candidate)
  run_check(checkers[2L], "Large/binary file")
  unlink(candidate)
  spaced <- file.path(root, "synthetic with spaces.txt")
  writeLines("synthetic public text", spaced)
  writeLines("synthetic public text", file.path(root, "synthetic\nname.txt"))
  run_check(checkers[2L])
  candidate <- file.path(root, "synthetic-link.txt")
  require_check(file.symlink(spaced, candidate), "Could not prepare release symlink fixture")
  run_check(checkers[2L], "Non-file or symlink")
  unlink(candidate)
  run_check(checkers[2L])
  cat("PASS: R integrity gates reject tampered sources, unsafe paths/files, symlinks,",
      "invalid text and recognized secret patterns in synthetic fixtures.\n")
}

if (sys.nframe() == 0L) {
  script <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(script) != 1L) stop("Run this check with Rscript --vanilla.", call. = FALSE)
  check_integrity_contract(normalizePath(dirname(sub("^--file=", "", script)), mustWork = TRUE))
}
