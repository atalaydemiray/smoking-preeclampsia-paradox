.nat_annual_source_fields <- function(vars, year) {
  index <- .nat_catalog("variable-index")
  source <- .nat_catalog("source-layouts")[[as.character(year)]]$fields
  fields <- vapply(vars, function(variable) {
    direct <- Filter(function(f) tolower(f$source_field) == tolower(variable), source)
    if (length(direct) == 1L) return(direct[[1]]$source_field)
    item <- .nat_resolve_variable(variable, index)
    annual <- Filter(function(x) identical(as.integer(x$year), as.integer(year)), item$years)
    if (!length(annual)) {
      stop("Variable ", dQuote(variable), " is unavailable in ", year, ".", call. = FALSE)
    }
    if (is.null(annual[[1]]$position)) {
      stop("Variable ", dQuote(variable), " has no resolved source position in ", year, ".", call. = FALSE)
    }
    field <- annual[[1]]$source_field %||% item$id
    if (!tolower(field) %in% vapply(source, function(f) tolower(f$source_field), character(1))) {
      stop("Variable is not in the documented U.S.-file layout for ", year, ".", call. = FALSE)
    }
    field
  }, character(1))
  if (anyDuplicated(tolower(fields))) {
    stop("Requested names resolve to duplicate annual source fields.", call. = FALSE)
  }
  unname(fields)
}

.nat_read_local <- function(path, needed) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "dta") {
    if (!requireNamespace("haven", quietly = TRUE) || !requireNamespace("tidyselect", quietly = TRUE)) {
      stop("Reading Stata files requires the suggested packages `haven` and `tidyselect`.", call. = FALSE)
    }
    header <- haven::read_dta(path, n_max = 0)
    available <- names(header)
    if (anyDuplicated(tolower(available))) stop("Local file has ambiguous case-insensitive column names.", call. = FALSE)
    lookup <- stats::setNames(available, tolower(available))
    actual <- unname(lookup[tolower(needed)])
    if (anyNA(actual)) {
      stop("Local file is missing source field(s): ", paste(needed[is.na(actual)], collapse = ", "), call. = FALSE)
    }
    data <- haven::read_dta(path, col_select = tidyselect::all_of(actual))
  } else if (extension == "rds") {
    object <- readRDS(path)
    data <- if (is.list(object) && !is.data.frame(object) && is.data.frame(object$data)) object$data else object
    if (!is.data.frame(data)) stop("The RDS file does not contain a data frame.", call. = FALSE)
    available <- names(data)
    normalized <- tolower(sub("^source_", "", available, ignore.case = TRUE))
    if (anyDuplicated(normalized)) stop("Local file has ambiguous source column names.", call. = FALSE)
    lookup <- stats::setNames(available, tolower(sub("^source_", "", available, ignore.case = TRUE)))
    actual <- unname(lookup[tolower(needed)])
    if (anyNA(actual)) {
      stop("Local file is missing source field(s): ", paste(needed[is.na(actual)], collapse = ", "), call. = FALSE)
    }
    data <- data[, actual, drop = FALSE]
  } else if (extension == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE) || !requireNamespace("tidyselect", quietly = TRUE)) {
      stop("Reading Parquet files requires the suggested packages `arrow` and `tidyselect`.", call. = FALSE)
    }
    available <- names(arrow::read_parquet(path, as_data_frame = TRUE, col_select = character()))
    if (!length(available)) available <- names(arrow::open_dataset(path))
    if (anyDuplicated(tolower(available))) stop("Local file has ambiguous case-insensitive column names.", call. = FALSE)
    lookup <- stats::setNames(available, tolower(available))
    actual <- unname(lookup[tolower(needed)])
    if (anyNA(actual)) {
      stop("Local file is missing source field(s): ", paste(needed[is.na(actual)], collapse = ", "), call. = FALSE)
    }
    data <- arrow::read_parquet(path, as_data_frame = TRUE, col_select = tidyselect::all_of(actual))
  } else {
    stop("Supported local formats are .dta, .rds, and .parquet.", call. = FALSE)
  }
  names(data) <- needed
  as.data.frame(data, stringsAsFactors = FALSE)
}

#' Read selected natality source variables
#'
#' @param year One supported data year.
#' @param vars Source or local catalog variable names. The development version
#'   requires an explicit selection.
#' @param path Optional user-supplied annual `.dta`, `.rds`, or `.parquet` file.
#' @param population Either `"all_occurrence"` or `"us_residents"`.
#' @param download Allow a package-hosted download when a validated, published
#'   annual release is present. Cached files remain available when `FALSE`.
#' @return A data frame containing the requested source fields and `year`.
#' @export
.nat_read_year <- function(
  year,
  vars,
  path = NULL,
  population = c("all_occurrence", "us_residents"),
  download = TRUE
) {
  year <- .nat_validate_years(year, allow_null = FALSE)
  if (length(year) != 1L) stop("`year` must contain exactly one year.", call. = FALSE)
  if (missing(vars) || !is.character(vars) || !length(vars) || anyNA(vars) || any(!nzchar(vars))) {
    stop("`vars` must explicitly name one or more source variables.", call. = FALSE)
  }
  if (anyDuplicated(tolower(vars))) stop("`vars` contains duplicate names.", call. = FALSE)
  population <- match.arg(population)
  if (!is.logical(download) || length(download) != 1L || is.na(download)) {
    stop("`download` must be TRUE or FALSE.", call. = FALSE)
  }
  requested <- .nat_annual_source_fields(vars, year)
  support <- "DOB_YY"
  if (population == "us_residents") support <- c(support, "RESTATUS")
  needed <- unique(c(requested, support))
  package_release <- NULL
  if (is.null(path)) {
    entry <- .nat_annual_release(year)
    path <- .nat_release_path(entry, download = download)
    package_release <- entry
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
    stop("`path` must identify one existing local annual file.", call. = FALSE)
  }
  path <- normalizePath(path, mustWork = TRUE)
  data <- .nat_read_local(path, needed)
  observed_year <- trimws(as.character(data$DOB_YY))
  if (anyNA(observed_year) || any(observed_year != as.character(year))) {
    stop("The local file's DOB_YY values do not match the requested year.", call. = FALSE)
  }
  input_rows <- nrow(data)
  if (population == "us_residents") {
    residence <- trimws(as.character(data$RESTATUS))
    if (anyNA(residence) || any(!residence %in% as.character(1:4))) {
      stop("RESTATUS contains missing or unsupported values.", call. = FALSE)
    }
    data <- data[residence %in% as.character(1:3), , drop = FALSE]
  }
  out <- data[, requested, drop = FALSE]
  out$year <- year
  rownames(out) <- NULL
  source_issues <- .nat_warn_source_values(year, out)
  interpretation_notes <- .nat_warn_interpretation(year, requested)
  attr(out, "natality_provenance") <- list(
    year = year,
    package_version = as.character(utils::packageVersion("natality")),
    scientific_review = "pending",
    parsing_decisions = package_release$parsing_decisions %||% character(),
    source_value_issues = source_issues,
    interpretation_notes = interpretation_notes,
    population = population,
    input_file = basename(path),
    input_rows = input_rows,
    output_rows = nrow(out),
    source_status = if (is.null(package_release)) {
      "user-supplied local file; acquisition provenance not authenticated by the package"
    } else {
      paste0("artifact verified against the bundled release size and SHA-256; ",
             package_release$data_status)
    },
    artifact_sha256 = package_release$artifact_sha256 %||% NA_character_,
    source_url = package_release$source_url %||% NA_character_
  )
  out
}

#' Read one or more annual files without silently harmonizing source fields
#' @export
read_natality <- function(year, vars, path = NULL,
                          population = c("all_occurrence", "us_residents"),
                          download = TRUE) {
  years <- .nat_validate_years(year, allow_null = FALSE)
  population <- match.arg(population)
  if (length(years) == 1L) {
    return(.nat_read_year(years, vars, path, population, download))
  }
  if (!is.null(path) && (!is.character(path) || is.null(names(path)) ||
      anyDuplicated(names(path)) || !setequal(names(path), as.character(years)))) {
    stop("For multiple years, `path` must be NULL or a character vector named by every requested year.", call. = FALSE)
  }
  if (is.null(path)) {
    for (y in years) .nat_validate_release(.nat_annual_release(y))
  } else if (anyNA(path) || any(!file.exists(path))) {
    stop("Every requested year must have an existing local file.", call. = FALSE)
  }
  # Check all metadata mappings before any large read or potential download.
  fields <- lapply(years, function(y) .nat_annual_source_fields(vars, y))
  if (!all(vapply(fields, identical, logical(1), fields[[1]]))) {
    stop("Requested fields have different annual source names. Read the years separately and inspect `natality_changes()`.", call. = FALSE)
  }
  histories <- lapply(vars, function(variable) {
    history <- tryCatch(natality_changes(variable, seq.int(min(years), max(years))),
                        error = function(e) NULL)
    if (is.null(history)) return(NULL)
    transitions <- history$comparisons[history$comparisons$era_change, , drop = FALSE]
    if (!nrow(transitions)) return(NULL)
    data.frame(variable = variable, transitions, stringsAsFactors = FALSE)
  })
  histories <- Filter(Negate(is.null), histories)
  known_changes <- if (length(histories)) do.call(rbind, histories) else data.frame()
  details <- if (nrow(known_changes)) paste0(" Known transitions: ",
    paste(paste0(known_changes$variable, " ", known_changes$older_year, " -> ",
                 known_changes$newer_year, " (", known_changes$classification, ")"), collapse = "; "), ".") else ""
  warning("Multiple years return unchanged source codes; cross-year comparability is not established.",
          details, " Inspect `natality_changes()` and reporting coverage before pooling analyses.", call. = FALSE)
  parts <- lapply(years, function(y) {
    .nat_read_year(y, vars, if (is.null(path)) NULL else unname(path[as.character(y)]),
                  population, download)
  })
  # Factors, labelled values, and differing local types must not be coerced by rbind.
  signature <- function(x) lapply(x, function(column) list(type = typeof(column), attributes = attributes(column)))
  signatures <- lapply(parts, signature)
  if (!all(vapply(signatures, identical, logical(1), signatures[[1]]))) {
    stop("Annual column types or labels differ. Read years separately; no automatic coercion was applied.", call. = FALSE)
  }
  provenance <- stats::setNames(lapply(parts, attr, which = "natality_provenance"), as.character(years))
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  attr(out, "natality_provenance") <- list(
    years = years, population = population, output_rows = nrow(out),
    comparability = "not_established", known_transitions = known_changes, annual_releases = provenance
  )
  out
}
