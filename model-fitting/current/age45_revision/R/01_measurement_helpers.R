# Pure measurement helpers for the proposed early-pregnancy analysis.
# No file reads, cohort exclusions, model fits, or package dependencies.
#
# Reporting flags must first be mapped by a year/source-specific dictionary to:
# reported, not_reported, unknown, or not_provided. The last means no flag was
# supplied; an explicit unknown flag does NOT authorize use of the field.
# Sources: NCHS natality user guides and
# https://wonder.cdc.gov/wonder/help/natality-expanded.html

.expand_to_rows <- function(x, n, label) {
  if (length(x) == n) return(x)
  if (length(x) == 1L) return(rep(x, n))
  stop(label, " must have length 1 or the number of records (", n, ").",
       call. = FALSE)
}

.reporting_status <- function(x, n, label) {
  x <- .expand_to_rows(as.character(x), n, label)
  allowed <- c("reported", "not_reported", "unknown", "not_provided")
  if (anyNA(x) || any(!x %in% allowed)) {
    stop(label, " must use normalized reporting statuses: ",
         paste(allowed, collapse = ", "), ".", call. = FALSE)
  }
  x
}

.raw_character <- function(x, label) {
  # A bare NA is logical in R; accept missing-only logical vectors, never TRUE/FALSE.
  missing_only <- is.logical(x) && all(is.na(x))
  if (!(is.numeric(x) || is.character(x) || is.factor(x) || missing_only) || !is.null(dim(x))) {
    stop(label, " must be a numeric, character, or factor vector.", call. = FALSE)
  }
  trimws(as.character(x))
}

decode_cigarettes <- function(x, reporting_status = "not_provided",
                              field = "cigarettes") {
  raw <- .raw_character(x, field)
  blank <- is.na(raw) | raw == ""
  numeric_syntax <- !blank & grepl("^[0-9]+$", raw)
  code <- rep(NA_real_, length(raw))
  code[numeric_syntax] <- suppressWarnings(as.numeric(raw[numeric_syntax]))
  bad <- !blank & (!numeric_syntax | !is.finite(code) | code < 0 | code > 99)
  if (any(bad)) {
    stop(field, ": unsupported cigarette codes at rows ",
         paste(head(which(bad), 8L), collapse = ", "), ".", call. = FALSE)
  }
  reporting <- .reporting_status(reporting_status, length(raw),
                                  paste0(field, " reporting_status"))
  status <- rep("unknown_blank", length(raw))
  status[!blank & code == 99] <- "unknown_code99"
  status[!blank & code == 0] <- "observed_zero"
  status[!blank & code >= 1 & code <= 97] <- "observed_positive"
  status[!blank & code == 98] <- "observed_topcoded"
  status[reporting == "not_reported"] <- "not_reported"
  status[reporting == "unknown"] <- "unknown_reporting_status"
  observed <- status %in% c("observed_zero", "observed_positive", "observed_topcoded")
  smoking <- rep(NA, length(raw))
  smoking[observed] <- code[observed] > 0
  exact <- lower <- upper <- rep(NA_real_, length(raw))
  exact[observed & code <= 97] <- code[observed & code <= 97]
  lower[observed] <- upper[observed] <- code[observed]
  upper[observed & code == 98] <- Inf
  data.frame(raw_code = code, status = status, reporting_status = reporting,
             smoking = smoking, topcoded = observed & !is.na(code) & code == 98,
             exact_cigarettes = exact, dose_lower_bound = lower,
             dose_upper_bound = upper, stringsAsFactors = FALSE)
}

classify_early_smoking <- function(cig_0, cig_1,
                                 pre_reporting_status = "not_provided",
                                 t1_reporting_status = "not_provided") {
  if (length(cig_0) != length(cig_1)) {
    stop("cig_0 and cig_1 must have equal lengths; exposure values are not recycled.",
         call. = FALSE)
  }
  pre <- decode_cigarettes(cig_0, pre_reporting_status, "cig_0")
  t1 <- decode_cigarettes(cig_1, t1_reporting_status, "cig_1")
  pre_known <- !is.na(pre$smoking)
  t1_known <- !is.na(t1$smoking)
  known <- pre_known & t1_known
  classification_status <- rep("classified", nrow(pre))
  classification_status[!pre_known & t1_known] <- "pre_unknown"
  classification_status[pre_known & !t1_known] <- "t1_unknown"
  classification_status[!pre_known & !t1_known] <- "pre_and_t1_unknown"
  pattern <- rep(NA_character_, nrow(pre))
  pattern[known & !pre$smoking & !t1$smoking] <- "no_reported_pre_or_t1_smoking"
  pattern[known & pre$smoking & !t1$smoking] <- "pre_smoking_t1_abstinence"
  pattern[known & pre$smoking & t1$smoking] <- "pre_and_t1_smoking"
  pattern[known & !pre$smoking & t1$smoking] <- "t1_only_smoking"

  # This is eligibility for the EXPOSURE contrast, not clinical/cohort eligibility.
  eligible <- known & pre$smoking %in% TRUE
  contrast <- rep(NA_integer_, nrow(pre))
  contrast[eligible] <- as.integer(t1$smoking[eligible])
  contrast_status <- rep("eligible", nrow(pre))
  contrast_status[!pre_known] <- "baseline_unknown"
  contrast_status[pre_known & !pre$smoking] <- "baseline_not_smoker"
  contrast_status[pre_known & pre$smoking & !t1_known] <- "t1_unknown"
  out <- data.frame(pattern = pattern, classification_status = classification_status,
                    prepregnancy_smoking = pre$smoking,
                    first_trimester_smoking = t1$smoking,
                    exposure_eligible = eligible,
                    within_prepregnancy_smokers = contrast,
                    contrast_status = contrast_status, stringsAsFactors = FALSE)
  names(pre) <- paste0("pre_", names(pre))
  names(t1) <- paste0("t1_", names(t1))
  out <- cbind(out, pre, t1)
  attr(out, "contrast_definition") <- paste(
    "Among people reporting prepregnancy smoking: 1 = reported T1 smoking;",
    "0 = reported T1 abstinence. Reported patterns, not sustained interventions."
  )
  out
}

build_early_exposure <- function(data, pre_col = "cig_0", t1_col = "cig_1",
                                pre_reporting_col = NULL, t1_reporting_col = NULL) {
  if (!is.data.frame(data)) stop("data must be a data.frame.", call. = FALSE)
  required <- c(pre_col, t1_col, pre_reporting_col, t1_reporting_col)
  if (anyDuplicated(names(data)) || any(!required %in% names(data))) {
    stop("Required exposure/reporting columns are missing or column names are duplicated.",
         call. = FALSE)
  }
  # No T2, T3, outcome, or gestation columns are read by this adapter.
  classify_early_smoking(
    data[[pre_col]], data[[t1_col]],
    if (is.null(pre_reporting_col)) "not_provided" else data[[pre_reporting_col]],
    if (is.null(t1_reporting_col)) "not_provided" else data[[t1_reporting_col]]
  )
}

decode_art <- function(art, infertility,
                       art_reporting_status = "not_provided",
                       infertility_reporting_status = "not_provided") {
  if (length(art) != length(infertility)) {
    stop("ART and infertility parent fields must have equal lengths.", call. = FALSE)
  }
  a <- toupper(.raw_character(art, "ART"))
  parent <- toupper(.raw_character(infertility, "infertility"))
  for (x in list(a, parent)) {
    if (any(!is.na(x) & !x %in% c("Y", "N", "U", "X", ""))) {
      stop("Unsupported ART/infertility code; expected Y, N, U, X, or blank.",
           call. = FALSE)
    }
  }
  n <- length(a)
  ar <- .reporting_status(art_reporting_status, n, "ART reporting_status")
  pr <- .reporting_status(infertility_reporting_status, n, "infertility reporting_status")
  usable <- !ar %in% c("not_reported", "unknown") &
            !pr %in% c("not_reported", "unknown")
  contradiction <- usable & ((parent %in% "N" & a %in% "Y") |
                              (parent %in% "Y" & a %in% "X"))
  if (any(contradiction)) {
    stop("Contradictory ART/infertility parent reports at rows ",
         paste(head(which(contradiction), 8L), collapse = ", "), ".",
         call. = FALSE)
  }
  value <- rep(NA_integer_, n)
  status <- rep("unknown_art", n)
  status[a %in% "X"] <- "indeterminate_structural_status"
  yes <- usable & a %in% "Y"
  no <- usable & a %in% "N"
  structural <- usable & parent %in% "N" & a %in% "X"
  value[yes] <- 1L
  value[no | structural] <- 0L
  status[yes] <- "observed_art"
  status[no] <- "observed_no_art"
  status[structural] <- "structural_nonuse_parent_no"
  status[ar == "unknown" | pr == "unknown"] <- "unknown_reporting_status"
  status[ar == "not_reported" | pr == "not_reported"] <- "not_reported"
  data.frame(art = value, status = status, art_raw = a,
             infertility_raw = parent, art_reporting_status = ar,
             infertility_reporting_status = pr, stringsAsFactors = FALSE)
}
