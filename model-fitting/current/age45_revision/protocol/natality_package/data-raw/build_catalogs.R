suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grepl("^--file=", args)]
if (length(script_arg) != 1L) stop("Run this file with Rscript")
script <- normalizePath(sub("^--file=", "", script_arg))
package_dir <- dirname(dirname(script))
root <- normalizePath(file.path(package_dir, "..", ".."))
output <- file.path(package_dir, "inst", "extdata")
dir.create(output, recursive = TRUE, showWarnings = FALSE)

inputs <- c(
  "source-value-issues" = file.path(root, "metadata", "source_value_issues.json"),
  "source-layouts" = file.path(root, "metadata", "us_source_layouts.json"),
  "variable-index" = file.path(root, "site", "public", "catalog", "variable-index.json"),
  "variable-history" = file.path(root, "site", "public", "catalog", "variable-history.json")
)
if (any(!file.exists(inputs))) stop("Source website catalogs are missing")
for (name in names(inputs)) {
  destination <- file.path(output, paste0(name, ".json"))
  if (!file.copy(inputs[[name]], destination, overwrite = TRUE, copy.date = TRUE)) {
    stop("Could not copy ", name)
  }
}

index <- read_json(inputs[["variable-index"]], simplifyVector = FALSE)
release_catalog <- read_json(
  file.path(root, "metadata", "package_data_releases.json"),
  simplifyVector = FALSE
)
release_by_year <- setNames(
  release_catalog$releases,
  vapply(release_catalog$releases, function(x) as.character(x$year), character(1))
)
manifest <- list(
  schema_version = 1,
  package_version = unname(read.dcf(file.path(package_dir, "DESCRIPTION"))[1, "Version"]),
  status = if (all(vapply(release_catalog$releases, function(x) isTRUE(x$published), logical(1))))
    "Published source-access alpha; independent scientific review pending" else
    "Built artifacts validated locally; some package-hosted downloads not yet published",
  years = lapply(index$years, function(x) {
    release <- release_by_year[[as.character(x$year)]]
    base <- list(
      year = as.integer(x$year),
      metadata_status = "available",
      data_status = if (is.null(release)) "not_built" else release$data_status,
      guide_url = x$guide_url,
      source_url = if (!is.null(release)) release$source_url else NULL
    )
    if (is.null(release)) return(base)
    c(base, release[setdiff(names(release), c("year", "data_status", "guide_url", "source_url"))])
  })
)
write_json(
  manifest,
  file.path(output, "data-manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)
