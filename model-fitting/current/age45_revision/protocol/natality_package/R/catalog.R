#' List supported natality metadata years
#'
#' @param details Return annual metadata and package-data status when `TRUE`.
#' @return An integer vector, or a data frame when `details = TRUE`.
#' @export
natality_years <- function(details = FALSE) {
  if (!is.logical(details) || length(details) != 1L || is.na(details)) {
    stop("`details` must be TRUE or FALSE.", call. = FALSE)
  }
  index <- .nat_catalog("variable-index")
  years <- sort(vapply(index$years, function(x) as.integer(x$year), integer(1)))
  if (!details) return(years)
  manifest <- .nat_catalog("data-manifest")
  status <- stats::setNames(
    vapply(manifest$years, function(x) x$data_status, character(1)),
    vapply(manifest$years, function(x) as.character(x$year), character(1))
  )
  rows <- lapply(index$years, function(x) {
    release <- Filter(function(r) as.integer(r$year) == as.integer(x$year), manifest$years)[[1]]
    data.frame(
      year = as.integer(x$year),
      observed_conversion_fields = as.integer(x$observed_fields),
      source_matches = as.integer(x$source_matches),
      unresolved = as.integer(x$unresolved),
      artifact_fields = as.integer(release$columns %||% NA_integer_),
      documented_source_fields = as.integer(release$documented_source_fields %||% release$columns %||% NA_integer_),
      auxiliary_columns = length(release$auxiliary_columns),
      artifact_records = as.numeric(release$records %||% NA_real_),
      metadata_status = "available",
      data_status = unname(status[as.character(x$year)]),
      guide_url = x$guide_url,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$year), , drop = FALSE]
}

#' Search natality variables
#'
#' @param pattern Optional regular expression matched against source names,
#'   descriptions and annual rule text.
#' @param years Optional years used to restrict availability.
#' @return A data frame with one row per matching canonical source name.
#' @export
natality_vars <- function(pattern = NULL, years = NULL) {
  if (!is.null(pattern) && (!is.character(pattern) || length(pattern) != 1L || is.na(pattern))) {
    stop("`pattern` must be NULL or one character string.", call. = FALSE)
  }
  selected_years <- .nat_validate_years(years)
  catalog <- .nat_catalog("source-layouts")
  annual <- unlist(lapply(selected_years, function(y) {
    lapply(catalog[[as.character(y)]]$fields, function(f) {
      f$year <- y
      f
    })
  }), recursive = FALSE)
  keys <- vapply(annual, function(f) tolower(f$source_field), character(1))
  groups <- split(annual, keys)
  rows <- lapply(groups, function(fields) {
    haystack <- paste(unlist(lapply(fields, function(f) c(f$source_field, f$description, f$rules_text))), collapse = " ")
    if (!is.null(pattern) && !grepl(pattern, haystack, ignore.case = TRUE)) return(NULL)
    latest <- fields[[which.max(vapply(fields, function(f) f$year, integer(1)))]]
    data.frame(variable = latest$source_field, local_name = tolower(latest$source_field),
               label = latest$description, topic = "Annual U.S. source field",
               years = .nat_collapse_years(vapply(fields, function(f) f$year, integer(1))),
               source_names = paste(unique(vapply(fields, function(f) f$source_field, character(1))), collapse = ", "),
               stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(variable=character(),local_name=character(),label=character(),
                                       topic=character(),years=character(),source_names=character()))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(tolower(out$variable)), , drop = FALSE]
}
