.natality_state <- new.env(parent = emptyenv())

.nat_source_observations <- function(years, fields) {
  Filter(function(x) as.integer(x$year) %in% years &&
           any(tolower(unlist(x$fields)) %in% tolower(fields)),
         .nat_catalog("source-value-issues")$issues)
}

.nat_warn_source_values <- function(year, data) {
  found <- list()
  for (issue in .nat_source_observations(year, names(data))) {
    affected <- rep(FALSE, nrow(data))
    fields <- intersect(names(data), unlist(issue$fields))
    for (field in fields) {
      affected <- affected | trimws(as.character(data[[field]])) %in% unlist(issue$codes[[field]])
    }
    if (any(affected)) {
      warning(year, " source documentation issue: ", paste(fields, collapse = ", "),
              " contain codes absent from the annual layout in ", sum(affected),
              " returned record(s). Values are unchanged; inspect natality_codebook().",
              call. = FALSE)
      found[[length(found)+1L]] <- list(id=issue$id, fields=fields,
                                     affected_returned_records=sum(affected), note=issue$note)
    }
  }
  found
}

.nat_catalog <- function(name) {
  key <- paste0("catalog_", name)
  if (exists(key, envir = .natality_state, inherits = FALSE)) {
    return(get(key, envir = .natality_state, inherits = FALSE))
  }
  path <- system.file("extdata", paste0(name, ".json"), package = "natality")
  if (!nzchar(path)) {
    stop("The installed natality metadata are missing; reinstall the package.", call. = FALSE)
  }
  value <- jsonlite::read_json(path, simplifyVector = FALSE)
  assign(key, value, envir = .natality_state)
  value
}

.nat_validate_years <- function(years, allow_null = TRUE) {
  available <- sort(vapply(.nat_catalog("variable-index")$years, function(x) {
    as.integer(x$year)
  }, integer(1)))
  if (is.null(years) && allow_null) return(available)
  if (!is.numeric(years) || !length(years) || anyNA(years) ||
      any(!is.finite(years)) || any(abs(years) > .Machine$integer.max) ||
      any(years != as.integer(years)) || anyDuplicated(years)) {
    stop("`years` must contain unique whole-number years.", call. = FALSE)
  }
  years <- as.integer(years)
  unsupported <- setdiff(years, available)
  if (length(unsupported)) {
    stop(
      "Unsupported metadata year(s): ", paste(unsupported, collapse = ", "),
      ". Available years are ", min(available), "\u2013", max(available), ".",
      call. = FALSE
    )
  }
  years
}

.nat_collapse_years <- function(years) {
  years <- sort(unique(as.integer(years)))
  if (!length(years)) return("")
  groups <- cumsum(c(TRUE, diff(years) != 1L))
  pieces <- vapply(split(years, groups), function(x) {
    if (length(x) == 1L) as.character(x) else paste0(min(x), "\u2013", max(x))
  }, character(1))
  paste(pieces, collapse = ", ")
}

.nat_resolve_variable <- function(variable, index = .nat_catalog("variable-index")) {
  if (!is.character(variable) || length(variable) != 1L || is.na(variable) || !nzchar(variable)) {
    stop("`variable` must be one nonempty name.", call. = FALSE)
  }
  target <- tolower(variable)
  matches <- Filter(function(item) {
    source_names <- vapply(item$years, function(x) x$source_field %||% item$id, character(1))
    target == tolower(item$id) || target %in% tolower(source_names)
  }, index$variables)
  if (!length(matches)) {
    stop(
      "Variable ", dQuote(variable), " was not found. Use `natality_vars()` to search.",
      call. = FALSE
    )
  }
  if (length(matches) > 1L) {
    stop("Variable name is ambiguous across catalog families: ", variable, call. = FALSE)
  }
  matches[[1]]
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
