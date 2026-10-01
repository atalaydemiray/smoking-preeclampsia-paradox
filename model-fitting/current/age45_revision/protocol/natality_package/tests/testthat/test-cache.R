test_that("cache helpers remain inside the configured directory", {
  cache <- file.path(tempdir(), paste0("natality-cache-", Sys.getpid()))
  old <- options(natality.cache_dir = cache)
  on.exit({
    options(old)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  expect_identical(natality_cache_dir(), cache)
  expect_true(dir.create(cache, recursive = TRUE))
  writeLines("fixture", file.path(cache, "natality_2024.rds"))
  writeLines("keep", file.path(cache, "unrelated.txt"))
  expect_equal(natality_cache_info()$year, 2024L)
  natality_cache_clear(2024)
  expect_false(file.exists(file.path(cache, "natality_2024.rds")))
  expect_true(file.exists(file.path(cache, "unrelated.txt")))
})

test_that("release downloads are cached only after size and SHA-256 verification", {
  cache <- file.path(tempdir(), paste0("natality-release-cache-", Sys.getpid()))
  source <- tempfile(fileext = ".parquet")
  writeBin(charToRaw("verified fixture"), source)
  old <- options(natality.cache_dir = cache)
  on.exit({
    options(old)
    unlink(cache, recursive = TRUE)
    unlink(source)
  }, add = TRUE)
  entry <- list(
    year = 2024L,
    data_status = "published",
    published = TRUE,
    artifact = "natality_2024_source_v1.parquet",
    artifact_bytes = unname(file.info(source)$size),
    artifact_sha256 = natality:::.nat_sha256(source),
    download_url = "https://example.test/natality_2024_source_v1.parquet"
  )
  fetch <- function(url, destination, mode, quiet) {
    expect_identical(url, entry$download_url)
    expect_true(file.copy(source, destination))
  }
  cached <- natality:::.nat_release_path(entry, download = TRUE, fetch = fetch)
  expect_true(file.exists(cached))
  expect_identical(natality:::.nat_sha256(cached), entry$artifact_sha256)
  expect_identical(
    natality:::.nat_release_path(entry, download = FALSE),
    cached
  )

  writeBin(charToRaw("damaged"), cached)
  expect_error(
    natality:::.nat_release_path(entry, download = FALSE),
    "not available in the natality cache"
  )
  bad_fetch <- function(url, destination, mode, quiet) {
    writeBin(charToRaw("wrong download"), destination)
  }
  expect_error(
    natality:::.nat_release_path(entry, download = TRUE, fetch = bad_fetch),
    "does not match the published size and SHA-256"
  )
})

test_that("versioned cache files are inventoried and cleared by year only", {
  cache <- tempfile("cache-versioned-")
  dir.create(cache)
  old <- options(natality.cache_dir = cache)
  on.exit({options(old); unlink(cache, recursive = TRUE)}, add = TRUE)
  files <- c("natality_2024_source_v1.parquet", "natality_2023_source_v1.parquet",
             "natality_2024_source_v1.parquet.part", "unrelated.parquet")
  for (name in files) writeLines("fixture", file.path(cache, name))
  expect_setequal(natality_cache_info()$year, c(2023L, 2024L))
  natality_cache_clear(2024)
  expect_false(file.exists(file.path(cache, files[[1]])))
  expect_true(all(file.exists(file.path(cache, files[-1]))))
})

test_that("local alpha import verifies identity and enables offline reads", {
  skip_if_not_installed("arrow")
  source <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data.frame(DOB_YY = c("2024", "2024"),
                                RESTATUS = c("1", "4"), MAGER = c("30", "40")), source)
  entry <- list(year = 2024L, data_status = "validated_local", published = FALSE,
                artifact = "natality_2024_source_v1.parquet",
                artifact_bytes = unname(file.info(source)$size),
                artifact_sha256 = natality:::.nat_sha256(source), source_url = "https://example.test/source")
  local_mocked_bindings(.nat_annual_release = function(year) entry)
  cache <- tempfile("local-alpha-")
  old <- options(natality.cache_dir = cache)
  on.exit({options(old); unlink(cache, recursive = TRUE); unlink(source)}, add = TRUE)
  source_hash <- natality:::.nat_sha256(source)
  expect_error(read_natality(2024, "MAGER", download = FALSE), "has not been published")
  cached <- natality_cache_import(2024, source)
  expect_identical(natality:::.nat_sha256(source), source_hash)
  expect_equal(natality_cache_info()$year, 2024L)
  x <- read_natality(2024, "MAGER", population = "us_residents", download = FALSE)
  expect_identical(x$MAGER, "30")
  expect_identical(attr(x, "natality_provenance")$artifact_sha256, source_hash)
  expect_match(attr(x, "natality_provenance")$source_status, "validated_local")
  writeBin(charToRaw("damaged"), cached)
  expect_error(read_natality(2024, "MAGER", download = FALSE), "verified cache")
  natality_cache_import(2024, source)
  expect_identical(natality:::.nat_sha256(cached), source_hash)
  writeBin(charToRaw("wrong source"), source)
  expect_error(natality_cache_import(2024, source), "does not match")
  expect_identical(natality:::.nat_sha256(cached), source_hash)
})

test_that("an interrupted download never installs partial data", {
  cache <- tempfile("interrupted-")
  old <- options(natality.cache_dir = cache)
  on.exit({options(old); unlink(cache, recursive = TRUE)}, add = TRUE)
  entry <- list(year = 2024L, data_status = "published", published = TRUE,
                artifact = "natality_2024_source_v1.parquet", artifact_bytes = 20,
                artifact_sha256 = paste(rep("a", 64), collapse = ""),
                download_url = "https://example.test/source")
  fetch <- function(url, destination, ...) {
    writeLines("partial", destination)
    stop("connection interrupted")
  }
  expect_error(natality:::.nat_release_path(entry, fetch = fetch), "connection interrupted")
  expect_length(list.files(cache), 0L)
  entry$artifact <- "../natality_2024_source_v1.parquet"
  expect_error(natality:::.nat_release_path(entry, fetch = fetch), "invalid artifact")
})

test_that("a published mirror must return the same pinned artifact", {
  cache <- tempfile("mirror-")
  source <- tempfile()
  writeBin(charToRaw("exact release fixture"), source)
  old <- options(natality.cache_dir = cache, timeout = 37)
  on.exit({options(old); unlink(cache, recursive = TRUE); unlink(source)}, add = TRUE)
  entry <- list(year = 2024L, data_status = "published", published = TRUE,
                artifact = "natality_2024_source_v1.parquet",
                artifact_bytes = unname(file.info(source)$size),
                artifact_sha256 = natality:::.nat_sha256(source),
                download_url = "https://primary.test/data",
                mirror_urls = list("https://wrong.test/data", "https://archive.test/data"))
  visited <- character()
  fetch <- function(url, destination, ...) {
    visited <<- c(visited, url)
    if (url == entry$download_url) stop("primary unavailable")
    if (url == entry$mirror_urls[[1]]) writeLines("HTML error page", destination)
    else file.copy(source, destination)
  }
  cached <- natality:::.nat_release_path(entry, fetch = fetch)
  expect_identical(visited, c(entry$download_url, unlist(entry$mirror_urls)))
  expect_identical(natality:::.nat_sha256(cached), entry$artifact_sha256)
  expect_equal(getOption("timeout"), 37)
  expect_identical(natality:::.nat_release_path(entry, download = FALSE,
                     fetch = function(...) stop("must stay offline")), cached)
  expect_identical(list.files(cache), entry$artifact)
  unlink(cached)
  entry$mirror_urls <- "http://insecure.test/data"
  expect_error(natality:::.nat_release_path(entry, fetch = fetch), "HTTPS")
  expect_length(list.files(cache), 0L)
})
