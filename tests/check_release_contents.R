# Check tracked and nonignored candidate files, without printing secret values.
# Ignored output is not a release input. This does not audit Git history.
check_release_contents <- function(root) {
  require_check <- function(ok, message) {
    if (!isTRUE(ok)) stop(message, call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  require_check(nzchar(Sys.which("git")), "The release-content check requires Git.")
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(root)
  listing <- tempfile("release_names_")
  on.exit(unlink(listing), add = TRUE)
  status <- system2("git", c("ls-files", "--cached", "--others", "--exclude-standard", "-z"),
                    stdout = listing, stderr = FALSE)
  require_check(identical(status, 0L), "Could not list candidate release files")
  # Decode NUL-separated Git paths without putting NUL bytes into R strings.
  bytes <- readBin(listing, "raw", n = file.info(listing)$size)
  ends <- which(bytes == as.raw(0))
  require_check(!length(bytes) || (length(ends) > 0L && tail(ends, 1L) == length(bytes)),
                "Invalid candidate-file listing")
  starts <- c(1L, head(ends, -1L) + 1L)
  candidates <- if (length(ends)) vapply(seq_along(ends), function(i) {
    if (starts[i] == ends[i]) "" else rawToChar(bytes[seq.int(starts[i], ends[i] - 1L)])
  }, "") else character()
  candidates <- sort(unique(candidates[nzchar(candidates)]))
  forbidden_parts <- c(".claude", ".codex", ".DS_Store", "__pycache__", "archive", "_maintainer",
                       "data", "derived", "cache", "library", "output", "outputs", "work")
  forbidden_suffixes <- c("rds", "rda", "rdata", "parquet", "feather", "docx", "pdf",
                          "png", "jpg", "jpeg", "zip", "gz", "pyc", "log")
  obsolete <- c("model-fitting/R/", "model-fitting/scripts/", "model-fitting/reference/",
                "model-fitting/vendor/", "results/bias/")
  patterns <- c(
    "private key" = "-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----",
    "GitHub token" = "\\b(?:gh[pousr]_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{40,})\\b",
    "AWS access key" = "\\b(?:AKIA|ASIA)[A-Z0-9]{16}\\b",
    "OpenAI key" = "\\bsk-(?:proj-|svcacct-)?[A-Za-z0-9_-]{40,}\\b",
    "private home path" = "/(?:Users|home)/[A-Za-z0-9_.-]+/"
  )
  for (name in candidates) {
    require_check(!any(strsplit(name, "/", fixed = TRUE)[[1L]] %in% forbidden_parts),
                  paste("Excluded path:", name))
    require_check(!any(startsWith(name, obsolete)), paste("Obsolete pipeline:", name))
    require_check(!tolower(tools::file_ext(name)) %in% forbidden_suffixes,
                  paste("Excluded file type:", name))
    path <- file.path(root, name)
    require_check(file.exists(path) && !dir.exists(path) && isTRUE(Sys.readlink(path) == ""),
                  paste("Non-file or symlink:", name))
    require_check(isTRUE(file.info(path)$size < 15000000), paste("Large/binary file:", name))
    data <- readBin(path, "raw", n = file.info(path)$size)
    require_check(!any(data == as.raw(0)), paste("Large/binary file:", name))
    content <- iconv(rawToChar(data), from = "UTF-8", to = "UTF-8", sub = NA_character_)
    require_check(!is.na(content), paste("Invalid UTF-8 file:", name))
    for (label in names(patterns)) {
      require_check(!grepl(patterns[[label]], content, perl = TRUE),
                    paste(label, "candidate:", name))
    }
  }
  cat("PASS:", length(candidates), "candidate public files; no excluded paths, binary data,",
      "home paths or recognized secret patterns.\n")
}

if (sys.nframe() == 0L) {
  script <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(script) != 1L) stop("Run this check with Rscript --vanilla.", call. = FALSE)
  check_release_contents(file.path(dirname(sub("^--file=", "", script)), ".."))
}
