#' Read annual source documentation for a variable
#'
#' @param variable A source or local catalog variable name.
#' @param years Optional years to return.
#' @return A `natality_codebook` object containing annual source entries.
#' @export
natality_codebook <- function(variable, years = NULL) {
  selected_years <- .nat_validate_years(years)
  if (!is.character(variable) || length(variable)!=1L || is.na(variable) || !nzchar(variable)) {
    stop("`variable` must be one nonempty name.", call. = FALSE)
  }
  index_item <- tryCatch(.nat_resolve_variable(variable), error = function(e) NULL)
  source <- .nat_catalog("source-layouts")
  annual <- lapply(selected_years, function(y) {
    entry <- source[[as.character(y)]]
    direct <- Filter(function(f) tolower(f$source_field) == tolower(variable), entry$fields)
    observed <- if (is.null(index_item)) list() else Filter(function(a) as.integer(a$year)==y, index_item$years)
    if (!length(direct) && length(observed) && !is.null(observed[[1]]$source_field)) {
      direct <- Filter(function(f) tolower(f$source_field)==tolower(observed[[1]]$source_field),entry$fields)
    }
    if (length(direct)!=1L) return(NULL)
    field <- direct[[1]]
    field$year <- y
    field$guide_url <- entry$guide_url
    field$storage_type <- if (length(observed)) observed[[1]]$storage_type else NULL
    field$comparability <- "not_established"
    field
  })
  annual <- Filter(Negate(is.null), annual)
  item <- if (is.null(index_item)) list(id=tolower(variable),label=variable,topic="Annual U.S. source field") else index_item
  if (!length(annual)) {
    stop("The variable is not documented in the requested years.", call. = FALSE)
  }
  rows <- lapply(annual, function(x) {
    position <- if (is.null(x$position)) NA_character_ else paste(unlist(x$position), collapse = "\u2013")
    pages <- if (is.null(x$pages)) x$page %||% NA else unlist(x$pages)
    data.frame(
      year = as.integer(x$year),
      source_field = x$source_field %||% item$id,
      description = x$description %||% item$label,
      position = position,
      pdf_pages = paste(pages, collapse = ", "),
      observed_conversion_storage = x$storage_type %||% NA_character_,
      printed_width = x$width %||% NA_integer_,
      position_span = x$position_span %||% NA_integer_,
      comparability = x$comparability %||% "not_established",
      review_status = x$review_status %||% NA_character_,
      layout_conflict = isTRUE(x$layout_conflict),
      rules_text = x$rules_text %||% NA_character_,
      guide_url = x$guide_url %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  out <- list(
    variable = item$id,
    label = item$label,
    topic = item$topic,
    years = do.call(rbind, rows),
    source_observations = .nat_source_observations(selected_years, vapply(annual, function(x) x$source_field, character(1)))
  )
  class(out) <- "natality_codebook"
  out
}

#' @export
print.natality_codebook <- function(x, ...) {
  cat("Natality variable:", x$label, "(", x$variable, ")\n", sep = "")
  cat("Topic:", x$topic, "\n")
  shown <- x$years[, c("year", "source_field", "description", "pdf_pages", "comparability")]
  print(shown, row.names = FALSE)
  for (issue in x$source_observations) cat("\nSource-data observation:", issue$note, "\n")
  invisible(x)
}

#' Inspect recorded annual changes for a variable
#'
#' @param variable A source or local catalog variable name.
#' @param years Optional years used to restrict comparisons and notes.
#' @return A `natality_changes` object with rule eras, adjacent-year comparisons,
#'   and recorded methodological notes.
#' @export
natality_changes <- function(variable, years = NULL) {
  selected_years <- .nat_validate_years(years)
  index_item <- .nat_resolve_variable(variable)
  history <- .nat_catalog("variable-history")
  key <- tolower(index_item$id)
  item <- history$variables[[key]]
  if (is.null(item)) {
    stop("No annual history is available for this variable family.", call. = FALSE)
  }
  eras <- lapply(item$eras, function(x) {
    era_years <- intersect(as.integer(unlist(x$years)), selected_years)
    if (!length(era_years)) return(NULL)
    data.frame(
      years = .nat_collapse_years(era_years),
      representative_source_year = as.integer(x$source_year),
      stringsAsFactors = FALSE
    )
  })
  comparisons <- lapply(item$comparisons, function(x) {
    pair <- c(as.integer(x$older_year), as.integer(x$newer_year))
    if (!all(pair %in% selected_years)) return(NULL)
    changes <- unlist(x$changes, recursive = TRUE, use.names = FALSE)
    data.frame(
      older_year = pair[[1]],
      newer_year = pair[[2]],
      result = x$result,
      classification = x$classification,
      era_change = isTRUE(x$era_change),
      changes = if (length(changes)) paste(changes, collapse = "; ") else "",
      stringsAsFactors = FALSE
    )
  })
  notes <- lapply(item$notes, function(x) {
    note_years <- intersect(as.integer(unlist(x$years)), selected_years)
    if (!length(note_years)) return(NULL)
    data.frame(
      years = .nat_collapse_years(note_years),
      kind = x$kind,
      note = x$text,
      stringsAsFactors = FALSE
    )
  })
  empty_eras <- data.frame(years = character(), representative_source_year = integer())
  empty_comparisons <- data.frame(
    older_year = integer(), newer_year = integer(), result = character(),
    classification = character(), era_change = logical(), changes = character()
  )
  empty_notes <- data.frame(years = character(), kind = character(), note = character())
  bind_or_empty <- function(x, empty) {
    x <- Filter(Negate(is.null), x)
    if (length(x)) do.call(rbind, x) else empty
  }
  out <- list(
    variable = item$id,
    comparability = item$comparability,
    scope = item$scope,
    eras = bind_or_empty(eras, empty_eras),
    comparisons = bind_or_empty(comparisons, empty_comparisons),
    notes = bind_or_empty(notes, empty_notes)
  )
  class(out) <- "natality_changes"
  out
}

#' @export
print.natality_changes <- function(x, ...) {
  cat("Natality change history:", x$variable, "\n")
  cat("Comparability:", x$comparability, "\n")
  if (nrow(x$eras)) {
    cat("\nDocumented rule eras:\n")
    print(x$eras, row.names = FALSE)
  }
  changed <- x$comparisons[x$comparisons$result != "same_layout_text", , drop = FALSE]
  if (nrow(changed)) {
    cat("\nAdjacent-year differences:\n")
    print(changed, row.names = FALSE)
  }
  if (nrow(x$notes)) {
    cat("\nMethodological notes:\n")
    print(x$notes, row.names = FALSE)
  }
  invisible(x)
}
