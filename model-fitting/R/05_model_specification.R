# Pure model specification/selection. No fitting, imputation, I/O or knot estimation.
# Source R/01_measurement_helpers.R before prepare_sep_model_data().

.sep_spec_numeric <- function(x, label, missing = FALSE) {
  # Factor labels are source codes; internal factor level indices are never codes.
  if (!(is.numeric(x) || is.character(x) || is.factor(x) ||
        (is.logical(x) && all(is.na(x)))))
    stop(label, " must be numeric or source-code text.")
  text <- as.character(x)
  z <- suppressWarnings(as.numeric(text))
  if (any(!is.na(text) & (!is.finite(z) | !grepl("^[0-9]+([.][0-9]+)?$", text))) ||
      (!missing && anyNA(z))) stop("Invalid numeric model input: ", label)
  z
}

.sep_spec_basis <- function(knots, boundaries, prefix) {
  if (!is.numeric(knots) || !length(knots) || any(!is.finite(knots)) ||
      is.unsorted(knots, strictly = TRUE) || !is.numeric(boundaries) ||
      length(boundaries) != 2L || any(!is.finite(boundaries)) ||
      boundaries[1] >= boundaries[2] ||
      any(knots <= boundaries[1] | knots >= boundaries[2]))
    stop("Explicit fixed, increasing interior knots and two boundaries are required.")
  list(knots = knots, boundaries = boundaries,
       columns = paste0(prefix, seq_len(length(knots) + 1L)))
}

.sep_spec_ns_term <- function(variable, basis) {
  numbers <- function(x) paste(sprintf("%.17g", x), collapse = ",")
  paste0("splines::ns(", variable, ",knots=c(", numbers(basis$knots),
         "),Boundary.knots=c(", numbers(basis$boundaries), "),intercept=FALSE)")
}

lock_sep_model_specification <- function(tier, contrast, years, age_knots,
    bmi_knots = NULL, age_boundaries = c(15, 45), bmi_boundaries = c(13, 69.9),
    race_mode = "edited") {
  tier <- match.arg(tier, c("natality_main", "shared_core", "shared_augmented"))
  contrast <- match.arg(contrast, c("within_prepregnancy_smokers", "complementary", "prepregnancy_only"))
  race_mode <- match.arg(race_mode, c("edited", "mask_original_unknown_nonhispanic"))
  if (!is.numeric(years) || !length(years) || anyNA(years) ||
      any(!years %in% 2014:2024) || is.unsorted(years, strictly = TRUE))
    stop("Lock distinct increasing source years explicitly.")
  age_basis <- .sep_spec_basis(age_knots, age_boundaries, "sep_age_basis_")
  if (!identical(as.numeric(age_boundaries), c(15, 45)))
    stop("Age-45 amendment requires age boundaries 15 and 45.")
  class(age_basis) <- "sep_age_spec"
  augmented <- tier != "shared_core"
  if (!augmented && !is.null(bmi_knots)) stop("Shared core does not use BMI knots.")
  bmi_basis <- if (augmented) .sep_spec_basis(bmi_knots, bmi_boundaries, "sep_bmi_basis_") else NULL
  if (augmented && !identical(as.numeric(bmi_boundaries), c(13, 69.9)))
    stop("Current BMI source boundaries are 13 and 69.9; no silent trimming.")
  covariates <- c("year_factor", "race_ethnicity", "prepreg_diabetes", "prior_living4")
  if (augmented) covariates <- c(covariates, "education4", "bmi")
  if (tier == "natality_main") covariates <- c(covariates, "prior_preterm", "prior_cesarean", "nativity")
  if (contrast == "within_prepregnancy_smokers") covariates <- c(covariates, "prepreg_dose5")
  rhs <- c(paste0("A * ", .sep_spec_ns_term("age", age_basis)),
           ifelse(covariates == "bmi", if (augmented) .sep_spec_ns_term("bmi", bmi_basis) else "", covariates))
  formula <- stats::as.formula(paste("Y ~", paste(rhs, collapse = " + ")), env = baseenv())
  fit_covariates <- unlist(lapply(covariates, function(x)
    if (x == "bmi") bmi_basis$columns else x), use.names = FALSE)
  structure(list(tier = tier, contrast = contrast, years = as.integer(years),
    race_mode = race_mode, age_spec = age_basis, bmi_spec = bmi_basis,
    covariates = covariates, formula = formula, fit_covariates = fit_covariates,
    outcome = "Y", exposure = "A", age = "age",
    estimand_label = "standardized recorded-outcome association in explicitly eligible pregnancy-ending records",
    forbidden_adjustments = c("source_type", "live_birth", "fetal_death", "oe_weeks", "gestational_diabetes",
      "birthweight", "gestational_weight_gain", "delivery_mode", "CIG1_intensity", "CIG2", "CIG3")),
    class = "sep_model_specification")
}

.sep_spec_status <- function(status, field) {
  raw <- as.character(status)
  observed <- c("observed", "observed_yes", "observed_no")
  item <- c("unknown_blank", "unknown_code", "unknown_codeU")
  supported <- c(observed, item, "not_reported", "unknown_reporting_status", "structurally_unavailable",
                 "observed_reporting_support_unresolved")
  if (anyNA(raw) || any(!raw %in% supported))
    stop("Invalid or unrecognized baseline status for ", field,
         "; do not turn invalid codes into ordinary imputation NA.")
  reason <- rep("observed_edited", length(raw))
  reason[raw %in% item] <- "item_unknown"
  reason[raw == "not_reported"] <- "structural_not_reporting"
  reason[raw == "unknown_reporting_status"] <- "reporting_support_unknown"
  reason[raw == "structurally_unavailable"] <- "structural_unavailable"
  reason[raw == "observed_reporting_support_unresolved"] <- "documentation_unsupported"
  list(raw = raw, observed = raw %in% observed, reason = reason)
}

.sep_spec_baseline <- function(data, field, allowed = NULL, bounds = NULL) {
  needed <- c(field, paste0(field, "_status"))
  if (any(!needed %in% names(data))) stop("Missing required baseline field/status: ", field)
  z <- .sep_spec_numeric(data[[field]], field, missing = TRUE)
  st <- .sep_spec_status(data[[needed[2]]], field)
  if (any(st$observed & is.na(z))) stop("Observed baseline value is missing: ", field)
  if (any(!st$observed & !is.na(z) & st$reason != "documentation_unsupported"))
    stop("Unsupported nonmissing baseline candidate: ", field)
  if (!is.null(allowed) && any(!is.na(z) & !z %in% allowed)) stop("Out-of-domain baseline: ", field)
  if (!is.null(bounds) && any(!is.na(z) & (z < bounds[1] | z > bounds[2])))
    stop("Out-of-domain baseline: ", field)
  z[!st$observed] <- NA_real_
  list(value = z, reason = st$reason, raw_status = st$raw)
}

.sep_spec_membership <- function(x, expected, label, allow_missing = FALSE) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    if (any(!is.na(x) & !x %in% c("TRUE", "FALSE"))) stop("Invalid membership field: ", label)
    x <- ifelse(is.na(x), NA, x == "TRUE")
  }
  known <- !is.na(x)
  if (!is.logical(x) || length(x) != length(expected) || (!allow_missing && anyNA(x)) ||
      !identical(unname(x[known]), unname(expected[known])))
    stop("Saved and reconstructed membership disagree: ", label)
}

prepare_sep_model_data <- function(data, spec, source) {
  if (!inherits(spec, "sep_model_specification")) stop("A locked model specification is required.")
  if (!is.data.frame(data) || !nrow(data) || anyDuplicated(names(data))) stop("Nonempty unique-column data.frame required.")
  if (!exists("decode_cigarettes", mode = "function") || !exists("classify_early_smoking", mode = "function"))
    stop("Source R/01_measurement_helpers.R first.")
  # Avoid data.table's different single-bracket column-selection semantics.
  data <- as.data.frame(data)
  n <- nrow(data)
  if (length(source) == 1L) source <- rep(source, n)
  source <- as.character(source)
  if (length(source) != n || anyNA(source) || any(!source %in% c("natality", "fetal_death")))
    stop("Supply explicit natality/fetal_death source for every input row.")
  if ("source_type" %in% names(data)) {
    saved_source <- as.character(data$source_type)
    if (any(!is.na(saved_source) & saved_source != source))
      stop("Declared source conflicts with saved source provenance.")
  }
  if (spec$tier == "natality_main" && any(source != "natality"))
    stop("Natality main specification cannot be applied to fetal records.")
  required <- c("source_row", "year", "age", "oe_weeks", "gh", "gh_status", "source_restatus", "source_dplural")
  if (any(!required %in% names(data))) stop("Missing clinical adapter columns.")
  year <- .sep_spec_numeric(data$year, "year")
  age <- .sep_spec_numeric(data$age, "age", missing = TRUE)
  oe <- .sep_spec_numeric(data$oe_weeks, "oe_weeks", missing = TRUE)
  source_row <- .sep_spec_numeric(data$source_row, "source_row")
  if (any(source_row < 1 | source_row != floor(source_row)) ||
      anyDuplicated(paste(source, year, source_row, sep = ":"))) stop("Invalid or duplicate source-record identity.")
  if (any(!year %in% spec$years)) stop("Input contains years outside the explicit lock; subset years upstream explicitly.")
  if (any(source == "fetal_death" & !year %in% 2018:2024)) stop("Unsupported fetal source year.")
  source_column <- function(natality, fetal) {
    result <- rep(NA_character_, n)
    for (s in unique(source)) {
      field <- if (s == "natality") natality else fetal
      if (!field %in% names(data)) stop("Missing raw clinical/exposure source field: ", field)
      result[source == s] <- as.character(data[[field]][source == s])
    }
    result
  }
  flag <- function(x) {
    if (any(!is.na(x) & !x %in% c("", "0", "1"))) stop("Invalid raw eligibility/exposure reporting flag.")
    ifelse(x %in% "1", "reported", ifelse(x %in% "0", "not_reported", "unknown"))
  }
  chtn <- source_column("source_rf_phype", "source_phyp")
  cflag <- flag(source_column("source_f_rf_phyper", "source_f_phyp"))
  if (any(!is.na(chtn) & !chtn %in% c("", "Y", "N", "U"))) stop("Invalid raw chronic hypertension code.")
  clinical <- as.character(data$source_restatus) %in% c("1", "2", "3") &
    as.character(data$source_dplural) %in% "1" & !is.na(age) & age %in% 15:45 &
    !is.na(oe) & oe %in% 20:47 & chtn %in% "N" & cflag == "reported"
  # R03 has no clinical_member column; pooled frames can legitimately have NA
  # in this optional metadata for natality rows. Raw criteria are still checked.
  if ("clinical_member" %in% names(data))
    .sep_spec_membership(data$clinical_member, clinical, "clinical_member", allow_missing = TRUE)
  gh <- .sep_spec_baseline(data, "gh", allowed = c(0, 1))
  raw_pre <- source_column("source_cig_0", "source_cig0")
  pre_flag <- flag(source_column("source_f_cigs_0", "source_f_cig0"))
  pre <- decode_cigarettes(raw_pre, pre_flag, "prepregnancy smoking")
  known_outcome <- clinical & !is.na(gh$value)
  A <- rep(NA_integer_, n)
  if (spec$contrast == "prepregnancy_only") {
    eligible <- known_outcome & !is.na(pre$smoking)
    A[eligible] <- as.integer(pre$smoking[eligible])
  } else {
    early <- classify_early_smoking(raw_pre,
      source_column("source_cig_1", "source_cig1"), pre_flag,
      flag(source_column("source_f_cigs_1", "source_f_cig1")))
    if (spec$contrast == "within_prepregnancy_smokers") {
      eligible <- known_outcome & early$exposure_eligible
      A[eligible] <- early$within_prepregnancy_smokers[eligible]
      member <- "primary_member"
    } else {
      eligible <- known_outcome & early$pattern %in% c("no_reported_pre_or_t1_smoking", "pre_and_t1_smoking")
      A[eligible] <- as.integer(early$pattern[eligible] == "pre_and_t1_smoking")
      member <- "complementary_member"
    }
    if (member %in% names(data)) .sep_spec_membership(data[[member]], eligible, member)
  }
  index <- which(eligible)
  if (!length(index)) stop("No exposure/outcome-eligible records; no model population created.")
  selected <- data[index, , drop = FALSE]
  out <- data.frame(Y = gh$value[index], A = A[index], age = age[index],
    year_factor = factor(year[index], levels = spec$years), stringsAsFactors = FALSE)
  reasons <- raw_status <- list()
  add <- function(name, field, allowed = NULL, bounds = NULL, transform = identity) {
    z <- .sep_spec_baseline(selected, field, allowed, bounds)
    out[[name]] <<- transform(z$value)
    reasons[[name]] <<- z$reason
    raw_status[[name]] <<- z$raw_status
  }
  race_labels <- c("Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic AIAN",
    "Non-Hispanic Asian", "Non-Hispanic NHOPI", "Non-Hispanic multiple race", "Hispanic")
  add("race_ethnicity", "race_hispanic_origin", 1:7,
      transform = function(x) factor(x, levels = 1:7, labels = race_labels))
  race_code <- .sep_spec_numeric(selected$race_hispanic_origin, "race_hispanic_origin", missing = TRUE)
  race_support <- rep("all_seven_categories", length(index))
  supported_race <- reasons$race_ethnicity == "observed_edited"
  race_support[supported_race & race_code %in% 1:6] <- "nonhispanic_categories_1_to_6"
  race_support[supported_race & race_code %in% 7] <- "hispanic_category_7"
  race_mask <- rep(FALSE, length(index))
  if (!"race_imputation_status" %in% names(selected)) stop("Race imputation provenance must be retained.")
  race_provenance <- as.character(selected$race_imputation_status)
  if (anyNA(race_provenance) || any(!race_provenance %in% c("not_imputed", "unknown_race_imputed",
      "formerly_other_race_imputed", "source_missing", "unknown_source_na", "field_not_supplied", "undocumented_source_code")))
    stop("Unrecognized race-imputation provenance.")
  if (spec$race_mode == "mask_original_unknown_nonhispanic") {
    if (any(race_provenance %in% c("source_missing", "unknown_source_na", "field_not_supplied")))
      stop("Original-unknown race sensitivity requires known imputation provenance.")
    race_mask <- supported_race & race_code %in% 1:6 & race_provenance == "unknown_race_imputed"
    out$race_ethnicity[race_mask] <- NA
    reasons$race_ethnicity[race_mask] <- "source_imputed_original_unknown_sensitivity"
  }
  yesno <- function(x) factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  add("prepreg_diabetes", "prepregnancy_diabetes", 0:1, transform = yesno)
  add("prior_living4", "prior_liveborn_children_now_living", 0:30,
      transform = function(x) factor(pmin(x, 3), levels = 0:3, labels = c("0", "1", "2", "3 or more")))
  if (spec$tier != "shared_core") {
    add("bmi", "prepregnancy_bmi", bounds = spec$bmi_spec$boundaries)
    add("education4", "education", 1:8, transform = function(x)
      factor(c(1, 1, 2, 3, 3, 4, 4, 4)[x], levels = 1:4,
             labels = c("Less than high school", "High school or GED", "Some college or associate", "Bachelor or higher")))
  }
  if (spec$tier == "natality_main") {
    add("prior_preterm", "previous_preterm_birth", 0:1, transform = yesno)
    add("prior_cesarean", "previous_cesarean", 0:1, transform = yesno)
    add("nativity", "nativity", 1:2, transform = function(x)
      factor(x, levels = 1:2, labels = c("Born in 50 US states", "Born elsewhere including territories")))
  }
  if (spec$contrast == "within_prepregnancy_smokers") {
    dose <- pre$raw_code[index]
    if (any(!dose %in% 1:98)) stop("Within-smoker dose is not known positive source intensity.")
    out$prepreg_dose5 <- cut(dose, breaks = c(0, 5, 10, 20, 40, Inf), right = TRUE,
      labels = c("1-5", "6-10", "11-20", "21-40", "41 or more"))
    reasons$prepreg_dose5 <- ifelse(dose == 98, "observed_topcoded_98_or_more", "observed_edited")
    raw_status$prepreg_dose5 <- pre$status[index]
  }
  missingness <- as.data.frame(lapply(reasons, factor), stringsAsFactors = TRUE)
  raw_status <- as.data.frame(lapply(raw_status, factor), stringsAsFactors = TRUE)
  baseline_fields <- intersect(spec$covariates, names(missingness))
  missingness_counts <- do.call(rbind, lapply(baseline_fields, function(f) {
    h <- as.data.frame(table(missingness[[f]]), stringsAsFactors = FALSE)
    names(h) <- c("reason", "n"); h$field <- f; h
  }))
  identity <- data.frame(input_row = index, source_type = factor(source[index]),
    year = as.integer(year[index]), source_row = source_row[index])
  retained <- c(input_records = n, clinically_eligible = sum(clinical),
    known_recorded_outcome = sum(known_outcome), selected_exposure_contrast = length(index))
  entering <- c(n, head(retained, -1L))
  ledger <- data.frame(stage = names(retained), n_entering = unname(entering),
    n_retained = unname(retained), n_excluded = unname(entering - retained))
  structure(list(data = out, identity = identity, missingness = missingness,
    original_status = raw_status, missingness_counts = missingness_counts,
    race_provenance = factor(race_provenance), race_mask = race_mask,
    race_allowed_set = factor(race_support), race_levels = race_labels,
    spec = spec, eligibility_ledger = ledger, n_input = n, n_selected = length(index), n_excluded_eligibility = n - length(index),
    baseline_records_dropped = 0L,
    requires_missingness_resolution = anyNA(out[c("Y", "A", "age", spec$covariates)]),
    source_is_provenance_not_adjustment = TRUE), class = "sep_prepared_model_data")
}

refresh_sep_model_bases <- function(data, spec) {
  if (!inherits(spec, "sep_model_specification") || !is.data.frame(data)) stop("Locked spec and data.frame required.")
  result <- as.data.frame(data)
  if (!is.null(spec$bmi_spec)) {
    if (!"bmi" %in% names(result)) stop("BMI column required to refresh locked nonlinear terms.")
    x <- .sep_spec_numeric(result$bmi, "bmi", missing = TRUE)
    b <- spec$bmi_spec
    if (any(!is.na(x) & (x < b$boundaries[1] | x > b$boundaries[2]))) stop("BMI outside locked source range.")
    values <- matrix(NA_real_, nrow(result), length(b$columns), dimnames = list(NULL, b$columns))
    known <- !is.na(x)
    if (any(known)) values[known, ] <- splines::ns(x[known], knots = b$knots,
      Boundary.knots = b$boundaries, intercept = FALSE)
    result[b$columns] <- values
  }
  result
}
