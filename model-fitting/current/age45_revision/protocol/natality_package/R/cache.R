#' Locate and inspect the natality data cache
#'
#' @param create Create the cache directory when it does not exist.
#' @return `natality_cache_dir()` returns a path. `natality_cache_info()` returns
#'   a data frame describing cached files.
#' @export
natality_cache_dir <- function(create = FALSE) {
  if (!is.logical(create) || length(create) != 1L || is.na(create)) {
    stop("`create` must be TRUE or FALSE.", call. = FALSE)
  }
  configured <- getOption("natality.cache_dir")
  path <- if (is.null(configured)) tools::R_user_dir("natality", "cache") else configured
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`natality.cache_dir` must be one nonempty directory path.", call. = FALSE)
  }
  path <- path.expand(path)
  if (create && !dir.exists(path) && !dir.create(path, recursive = TRUE)) {
    stop("Could not create the natality cache directory.", call. = FALSE)
  }
  path
}

#' @rdname natality_cache_dir
#' @export
natality_cache_info <- function() {
  path <- natality_cache_dir()
  if (!dir.exists(path)) {
    return(data.frame(file = character(), year = integer(), bytes = numeric()))
  }
  files <- list.files(path, pattern = "^natality_[0-9]{4}([.](parquet|rds|dta)|_source_v[0-9]+[.]parquet)$", full.names = TRUE)
  if (!length(files)) {
    return(data.frame(file = character(), year = integer(), bytes = numeric()))
  }
  info <- file.info(files)
  data.frame(
    file = basename(files),
    year = as.integer(sub("^natality_([0-9]{4}).*$", "\\1", basename(files))),
    bytes = unname(info$size),
    stringsAsFactors = FALSE
  )
}

#' Clear cached annual natality files
#'
#' @param years Optional years to remove. When omitted, all annual data files in
#'   the natality cache are removed. Metadata bundled with the package are not
#'   affected.
#' @return The removed paths, invisibly.
#' @export
natality_cache_clear <- function(years = NULL) {
  path <- natality_cache_dir()
  if (!dir.exists(path)) return(invisible(character()))
  files <- list.files(path, pattern = "^natality_[0-9]{4}([.](parquet|rds|dta)|_source_v[0-9]+[.]parquet)$", full.names = TRUE)
  if (!is.null(years)) {
    years <- .nat_validate_years(years, allow_null = FALSE)
    file_years <- as.integer(sub("^natality_([0-9]{4}).*$", "\\1", basename(files)))
    files <- files[file_years %in% years]
  }
  if (length(files) && any(!file.remove(files))) {
    stop("One or more cached files could not be removed.", call. = FALSE)
  }
  invisible(files)
}

.nat_sha256 <- function(path) {
  unname(digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE))
}

# Validate the manifest before constructing any destination or trusting a file.
.nat_validate_release <- function(entry) {
  if (!entry$data_status %in% c("published", "validated_local")) {
    stop("No validated package artifact has been built for this year. Supply `path` to a local annual file.", call. = FALSE)
  }
  if (!is.character(entry$artifact) || length(entry$artifact) != 1L ||
      is.na(entry$artifact) ||
      !grepl("^natality_[0-9]{4}_source_v[0-9]+[.]parquet$", entry$artifact) ||
      !startsWith(entry$artifact, paste0("natality_", entry$year, "_"))) {
    stop("The release manifest has an invalid artifact name or year.", call. = FALSE)
  }
  if (!is.numeric(entry$artifact_bytes) || length(entry$artifact_bytes) != 1L ||
      !is.finite(entry$artifact_bytes) || entry$artifact_bytes <= 0 ||
      entry$artifact_bytes != floor(entry$artifact_bytes) ||
      !is.character(entry$artifact_sha256) || length(entry$artifact_sha256) != 1L ||
      is.na(entry$artifact_sha256) || !grepl("^[a-fA-F0-9]{64}$", entry$artifact_sha256)) {
    stop("The release manifest has an invalid size or SHA-256.", call. = FALSE)
  }
}

.nat_valid_artifact <- function(path, entry) {
  file.exists(path) && !dir.exists(path) &&
    identical(unname(file.info(path)$size), as.numeric(entry$artifact_bytes)) &&
    identical(.nat_sha256(path), tolower(entry$artifact_sha256))
}

.nat_annual_release <- function(year) {
  entries <- Filter(function(x) identical(as.integer(x$year), as.integer(year)),
                    .nat_catalog("data-manifest")$years)
  if (length(entries) != 1L) stop("No unique annual release manifest entry.", call. = FALSE)
  entries[[1]]
}

#' Import a verified local release into the cache
#'
#' @param year One supported data year.
#' @param path A local artifact whose size and SHA-256 match the bundled manifest.
#' @return The cached path, invisibly. The supplied source is never changed.
#' @export
natality_cache_import <- function(year, path) {
  year <- .nat_validate_years(year, allow_null = FALSE)
  if (length(year) != 1L) stop("`year` must contain exactly one year.", call. = FALSE)
  entry <- .nat_annual_release(year)
  .nat_validate_release(entry)
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !.nat_valid_artifact(path, entry)) {
    stop("The local artifact does not match the release size and SHA-256; the cache was not updated.", call. = FALSE)
  }
  cache <- natality_cache_dir(create = TRUE)
  destination <- file.path(cache, entry$artifact)
  if (.nat_valid_artifact(destination, entry)) return(invisible(destination))
  temporary <- tempfile(pattern = "import-", tmpdir = cache)
  on.exit(unlink(temporary), add = TRUE)
  if (!file.copy(path, temporary) || !.nat_valid_artifact(temporary, entry)) {
    stop("The copied artifact failed verification; the cache was not updated.", call. = FALSE)
  }
  .nat_install_cache_file(temporary, destination)
  invisible(destination)
}

.nat_install_cache_file <- function(temporary, destination) {
  # A damaged old file may need removing on Windows. Never touch the source.
  if (file.exists(destination) && !file.remove(destination)) {
    stop("The damaged cache entry could not be replaced.", call. = FALSE)
  }
  if (!file.rename(temporary, destination)) {
    stop("The verified artifact could not be moved into the cache.", call. = FALSE)
  }
}

.nat_release_path <- function(entry, download = TRUE, fetch = utils::download.file) {
  .nat_validate_release(entry)
  cache <- natality_cache_dir(create = TRUE)
  destination <- file.path(cache, entry$artifact)
  if (.nat_valid_artifact(destination, entry)) return(destination)
  if (!identical(entry$data_status, "published") || !isTRUE(entry$published)) {
    stop("This locally validated release is not in the verified cache and has not been published. ",
         "Use `natality_cache_import(year, path)` to import the matching local artifact, ",
         "or supply `path` to `read_natality()`.", call. = FALSE)
  }
  if (!download) {
    stop("The validated file is not available in the natality cache. ",
         "Run again with `download = TRUE` on a connected machine.", call. = FALSE)
  }
  if (!is.character(entry$download_url) || length(entry$download_url) != 1L ||
      is.na(entry$download_url) || !grepl("^https://", entry$download_url)) {
    stop("Package-hosted releases must use HTTPS.", call. = FALSE)
  }
  mirrors <- entry$mirror_urls
  # jsonlite keeps JSON arrays as lists in the bundled catalog reader.
  if (is.list(mirrors) && all(vapply(mirrors, function(x) is.character(x) &&
                                   length(x) == 1L && !is.na(x), logical(1)))) {
    mirrors <- unlist(mirrors, use.names = FALSE)
  }
  if (!is.null(mirrors) && (!is.character(mirrors) || anyNA(mirrors) ||
                          any(!grepl("^https://", mirrors)))) {
    stop("Release mirrors must be HTTPS URLs in the bundled manifest.", call. = FALSE)
  }
  urls <- unique(c(entry$download_url, mirrors))
  temporary <- tempfile(pattern = "download-", tmpdir = cache)
  on.exit(unlink(temporary), add = TRUE)
  # Annual files can exceed 100 MB. Preserve an explicitly longer timeout.
  old_timeout <- options(timeout = max(600, getOption("timeout", 60)))
  on.exit(options(old_timeout), add = TRUE)
  failures <- character()
  for (url in urls) {
    unlink(temporary)
    failure <- tryCatch({
      fetch(url, temporary, mode = "wb", quiet = TRUE)
      if (!.nat_valid_artifact(temporary, entry)) {
        stop("The downloaded annual file does not match the published size and SHA-256.")
      }
      NULL
    }, error = function(e) conditionMessage(e))
    if (is.null(failure)) {
      .nat_install_cache_file(temporary, destination)
      return(destination)
    }
    failures <- c(failures, failure)
  }
  stop("No release download passed verification; the cache was not updated. ",
       paste(unique(failures), collapse = " "), call. = FALSE)
}
