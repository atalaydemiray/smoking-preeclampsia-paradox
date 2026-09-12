# Source-versioned adapter for the LOCAL NBER-converted US natality files.
# Pure transformations only. See config/natality_schema.csv for annual guide
# evidence; the feasibility driver checks the guide hashes before reading data.
# Source 01_measurement_helpers.R first. No archived configuration is used.

natality_source_spec <- function(year) {
  if (length(year) != 1L || is.na(year) || !year %in% 2014:2024) {
    stop("Supported natality source years are 2014-2024.")
  }
  # 2014/2015 official layouts explicitly direct RF_INFTR to field 319.
  # 2016/2017 guides require field 328, omitted from our local .dta files.
  # Do NOT substitute ART's reporting flag, or presume full reporting.
  parent_flag <- if (year <= 2015) "f_rf_pdiab" else "f_rf_inft"
  list(year = as.integer(year), source_version = paste0("NCHS_natality_", year,
       "_NBER_local_dta_v2.0"), infertility_flag = parent_flag,
       allow_absent_parent_flag = year %in% 2016:2017,
       plurality_codes = if (year <= 2019) 1:5 else 1:4,
       documented_t3_edit = year >= 2018)
}

natality_provenance_columns <- function() {
  c("mraceimp", "mage_repflg", "compgst_imp", "obgest_flg", "lmpused")
}

natality_selected_columns <- function(year, include_provenance = FALSE) {
  spec <- natality_source_spec(year)
  fields <- c("dob_yy", "restatus", "dplural", "imp_plur", "mager", "mage_impflg",
    "oegest_comb", "rf_phype", "f_rf_phyper", "rf_ghype", "f_rf_ghyper",
    paste0("cig_", 0:3), paste0("f_cigs_", 0:3), "rf_inftr", "rf_artec",
    "f_rf_inf_art", "mracehisp", "f_mhisp", "meduc", "f_meduc", "mbstate_rec",
    "lbo_rec", "priorlive", "priordead", "bmi", "f_m_ht", "f_pwgt",
    "rf_pdiab", "f_rf_pdiab", "rf_ppterm", "f_rf_ppb", "rf_cesar",
    "f_rf_cesar", "pay_rec", "f_pay_rec", "dmar")
  if (!spec$allow_absent_parent_flag) fields <- c(fields, spec$infertility_flag)
  if (isTRUE(include_provenance)) fields <- c(fields, natality_provenance_columns())
  unique(fields)
}

natality_flag <- function(x, field) {
  raw <- .raw_character(x, field)
  blank <- is.na(raw) | raw == ""
  if (any(!blank & !raw %in% c("0", "1"))) stop("Unsupported flag: ", field)
  out <- rep("unknown", length(raw))
  out[raw %in% "0"] <- "not_reported"
  out[raw %in% "1"] <- "reported"
  out
}

natality_numeric <- function(x, allowed, unknown = numeric(),
                            reporting = "not_provided", field) {
  raw <- .raw_character(x, field)
  blank <- is.na(raw) | raw == ""
  syntax <- !blank & grepl("^[0-9]+([.][0-9]+)?$", raw)
  value <- suppressWarnings(as.numeric(raw))
  # Match in floating-point tolerance only for one-decimal BMI stored as Stata
  # float; do not accept arbitrary values between documented one-decimal codes.
  supported <- rep(FALSE, length(raw))
  match_value <- round(value, 1L)
  near <- !is.na(value) & abs(value - match_value) < 0.00001
  supported[near] <- match_value[near] %in% round(c(allowed, unknown), 1L)
  if (any(!blank & (!syntax | !supported))) stop("Unsupported numeric code: ", field)
  value <- match_value
  status <- rep("observed", length(raw))
  status[blank] <- "unknown_blank"
  status[!blank & value %in% unknown] <- "unknown_code"
  flags <- .reporting_status(reporting, length(raw), field)
  status[flags == "unknown"] <- "unknown_reporting_status"
  status[flags == "not_reported"] <- "not_reported"
  value[status != "observed"] <- NA_real_
  list(value = value, status = status)
}

natality_ynu <- function(x, reporting, field) {
  raw <- .raw_character(x, field)
  blank <- is.na(raw) | raw == ""
  if (any(!blank & !raw %in% c("Y", "N", "U"))) stop("Unsupported Y/N/U code: ", field)
  flags <- .reporting_status(reporting, length(raw), field)
  status <- rep("unknown_blank", length(raw))
  status[raw %in% "U"] <- "unknown_codeU"
  status[raw %in% "Y"] <- "observed_yes"
  status[raw %in% "N"] <- "observed_no"
  status[flags == "unknown"] <- "unknown_reporting_status"
  status[flags == "not_reported"] <- "not_reported"
  value <- rep(NA_integer_, length(raw))
  value[status == "observed_yes"] <- 1L
  value[status == "observed_no"] <- 0L
  list(value = value, status = status)
}

natality_imputation <- function(x, field) {
  raw <- .raw_character(x, field)
  if (any(!is.na(raw) & !raw %in% c("", "1"))) stop("Unsupported imputation flag: ", field)
  # Older guides reverse the IMP_PLUR labels. Preserve raw flag categories for
  # plurality across ALL years rather than guess which older label is a typo.
  if (field == "imp_plur") return(ifelse(is.na(raw), "unknown_source_na",
    ifelse(raw %in% "1", "raw_code1", "raw_blank")))
  # Blank means not imputed for the maternal-age flag in these layouts.
  ifelse(is.na(raw), "unknown_source_na", ifelse(raw %in% "1", "imputed", "not_imputed"))
}

natality_race_imputation <- function(x) {
  # Every reviewed 2014-2024 guide: field 111, blank / 1 / 2.
  # This is provenance of the edited race, not a new race category or exclusion.
  raw <- .raw_character(x, "mraceimp")
  if (any(!is.na(raw) & !raw %in% c("", "1", "2"))) stop("Unsupported MRACEIMP code.")
  status <- rep("unknown_source_na", length(raw))
  status[raw %in% ""] <- "not_imputed"
  status[raw %in% "1"] <- "unknown_race_imputed"
  status[raw %in% "2"] <- "formerly_other_race_imputed"
  any_imputed <- rep(NA, length(raw))
  any_imputed[!is.na(raw)] <- raw[!is.na(raw)] %in% c("1", "2")
  list(raw_code = raw, status = status, any_imputed = any_imputed)
}

natality_provenance <- function(data) {
  n <- nrow(data)
  # Existing adapter callers may omit these fields. The input constructor requires
  # them explicitly; absence here remains unknown, never silently not imputed.
  race <- if ("mraceimp" %in% names(data)) natality_race_imputation(data$mraceimp) else
    list(raw_code = rep(NA_character_, n), status = rep("field_not_supplied", n),
         any_imputed = rep(NA, n))
  out <- list(race_imputation = race)
  for (f in setdiff(natality_provenance_columns(), "mraceimp")) {
    if (!f %in% names(data)) {
      out[[f]] <- list(raw_code = rep(NA_character_, n), status = rep("field_not_supplied", n))
    } else {
      raw <- .raw_character(data[[f]], f)
      if (any(!is.na(raw) & !raw %in% c("", "1"))) stop("Unsupported provenance flag: ", f)
      # Preserve source legend, especially OBGEST_FLG vs LMPUSED. Do not turn
      # these into an invented generic 'OE imputed' flag.
      out[[f]] <- list(raw_code = raw, status = ifelse(is.na(raw), "unknown_source_na",
        ifelse(raw == "1", "raw_code1", "raw_blank")))
    }
  }
  out
}

adapt_natality <- function(data, year) {
  spec <- natality_source_spec(year)
  required <- natality_selected_columns(year)
  if (!is.data.frame(data) || anyDuplicated(names(data)) ||
      any(!required %in% names(data))) stop("Missing/duplicated required natality columns.")
  n <- nrow(data)
  flags <- lapply(names(data)[grepl("^f_", names(data))], function(f) natality_flag(data[[f]], f))
  names(flags) <- names(data)[grepl("^f_", names(data))]
  missing_parent_flag <- !spec$infertility_flag %in% names(data)
  if (missing_parent_flag && !spec$allow_absent_parent_flag) stop("Required parent flag absent.")
  parent_flag <- if (missing_parent_flag) rep("unknown", n) else flags[[spec$infertility_flag]]
  numeric_field <- function(f, allowed, unknown = numeric(), flag = NULL) {
    natality_numeric(data[[f]], allowed, unknown,
      if (is.null(flag)) "not_provided" else flags[[flag]], f)
  }
  yn <- function(f, flag) natality_ynu(data[[f]], flags[[flag]], f)
  birth_year <- numeric_field("dob_yy", year)
  if (anyNA(birth_year$value)) stop("Missing birth year.")
  restatus <- numeric_field("restatus", 1:4)
  plurality <- numeric_field("dplural", spec$plurality_codes)
  age <- numeric_field("mager", 12:50)
  oe <- numeric_field("oegest_comb", 17:47, 99)
  chtn <- yn("rf_phype", "f_rf_phyper")
  gh <- yn("rf_ghype", "f_rf_ghyper")
  early <- classify_early_smoking(data$cig_0, data$cig_1, flags$f_cigs_0, flags$f_cigs_1)
  t2 <- decode_cigarettes(data$cig_2, flags$f_cigs_2, "cig_2")
  t3 <- decode_cigarettes(data$cig_3, flags$f_cigs_3, "cig_3")
  infertility <- natality_ynu(data$rf_inftr, parent_flag, "rf_inftr")
  raw_conflict <- (data$rf_inftr %in% "N" & data$rf_artec %in% "Y") |
    (data$rf_inftr %in% "Y" & data$rf_artec %in% "X")
  usable_conflict <- raw_conflict & parent_flag == "reported" &
    flags$f_rf_inf_art == "reported"
  # Quarantine an optional covariate contradiction, never edit its raw value or
  # delete the record. The general helper remains strict; this adapter supplies
  # an unknown flag only to the quarantined rows and preserves the reason below.
  art_flag <- flags$f_rf_inf_art
  art_flag[usable_conflict] <- "unknown"
  art <- decode_art(data$rf_artec, data$rf_inftr, art_flag, parent_flag)
  art$status[usable_conflict] <- "contradictory_parent_child"
  art$art_reporting_status <- flags$f_rf_inf_art
  art$raw_parent_child_conflict <- raw_conflict
  art$usable_parent_child_conflict <- usable_conflict
  baselines <- list(
    maternal_age = age,
    race_hispanic_origin = numeric_field("mracehisp", 1:7, 8, "f_mhisp"),
    education = numeric_field("meduc", 1:8, 9, "f_meduc"),
    nativity = numeric_field("mbstate_rec", 1:2, 3),
    live_birth_order_recode = numeric_field("lbo_rec", 1:8, 9),
    prior_liveborn_children_now_living = numeric_field("priorlive", 0:30, 99),
    prior_liveborn_children_now_dead = numeric_field("priordead", 0:30, 99),
    prepregnancy_bmi = numeric_field("bmi", seq(13, 69.9, .1), 99.9, "f_m_ht"),
    prepregnancy_diabetes = yn("rf_pdiab", "f_rf_pdiab"),
    previous_preterm_birth = yn("rf_ppterm", "f_rf_ppb"),
    previous_cesarean = yn("rf_cesar", "f_rf_cesar"),
    infertility_treatment = infertility,
    art_parent_aware = list(value = art$art, status = art$status),
    payer_timing_unresolved = numeric_field("pay_rec", 1:4, 9, "f_pay_rec"),
    marital_status_timing_unresolved = numeric_field("dmar", 1:2))
  list(year = year, n = n, spec = spec, flags = flags,
       missing_parent_flag = missing_parent_flag, age = age, restatus = restatus,
       plurality = plurality, oe = oe, chtn = chtn, gh = gh, early = early,
       t2 = t2, t3 = t3, art = art, baselines = baselines,
       provenance = natality_provenance(data),
       age_imputation = natality_imputation(data$mage_impflg, "mage_impflg"),
       plurality_imputation = natality_imputation(data$imp_plur, "imp_plur"))
}

natality_input_chunk <- function(data, year, source_row_offset = 0L) {
  # No cohort or final covariate-set change: preserve all source-supported
  # baseline candidates and early/GH unknowns in the clinical eligible input.
  if (length(source_row_offset) != 1L || !is.finite(source_row_offset) ||
      source_row_offset < 0 || source_row_offset != floor(source_row_offset) ||
      as.double(source_row_offset) + nrow(data) > .Machine$integer.max) stop("Invalid row offset.")
  if (any(!natality_provenance_columns() %in% names(data))) stop("Input construction requires provenance columns.")
  a <- adapt_natality(data, year)
  m <- natality_masks(a)
  keep <- m$known_no_prepregnancy_hypertension
  # These are exact source values as imported in this pinned DTA, not recovery of
  # original fixed-width formatting lost in a previous NBER conversion.
  raw <- data[keep, , drop = FALSE]
  for (f in names(raw)) {
    x <- raw[[f]]
    raw[[f]] <- if (is.character(x)) x else if (is.numeric(x)) as.numeric(x) else as.character(x)
  }
  names(raw) <- paste0("source_", names(raw))
  out <- data.frame(source_row = as.integer(source_row_offset + seq_len(a$n))[keep],
    year = rep(as.integer(year), sum(keep)), age = a$age$value[keep],
    oe_weeks = a$oe$value[keep], gh = a$gh$value[keep],
    gh_status = a$gh$status[keep], chtn_status = a$chtn$status[keep],
    early_pattern = a$early$pattern[keep], early_status = a$early$classification_status[keep],
    primary_member = (m$known_recorded_gh_pe & a$early$exposure_eligible)[keep],
    broad_member = m$known_recorded_gh_pe[keep],
    complementary_member = (m$known_recorded_gh_pe & a$early$pattern %in%
      c("no_reported_pre_or_t1_smoking", "pre_and_t1_smoking"))[keep],
    primary_t1_smoking = a$early$within_prepregnancy_smokers[keep],
    oe28_member = natality_masks(a, 28)$known_no_prepregnancy_hypertension[keep],
    stringsAsFactors = FALSE)
  for (prefix in c("pre", "t1")) for (field in c("status", "reporting_status", "smoking",
      "topcoded", "exact_cigarettes", "dose_lower_bound", "dose_upper_bound")) {
    f <- paste0(prefix, "_", field); out[[f]] <- a$early[[f]][keep]
  }
  for (f in names(a$baselines)) {
    out[[f]] <- a$baselines[[f]]$value[keep]
    out[[paste0(f, "_status")]] <- a$baselines[[f]]$status[keep]
  }
  out$age_imputation_status <- a$age_imputation[keep]
  out$plurality_imputation_status <- a$plurality_imputation[keep]
  out$race_imputation_status <- a$provenance$race_imputation$status[keep]
  out$race_any_imputed <- a$provenance$race_imputation$any_imputed[keep]
  for (f in setdiff(natality_provenance_columns(), "mraceimp")) {
    out[[paste0(f, "_status")]] <- a$provenance[[f]]$status[keep]
  }
  out <- cbind(out, raw)
  # Low-cardinality text is factorized only after decoding. Values incl. source
  # blank remain recoverable; source NA is never converted to a blank level.
  for (f in names(out)) if (is.character(out[[f]])) out[[f]] <- factor(out[[f]])
  rownames(out) <- NULL
  stage <- rep("retained_broad", a$n)
  for (f in rev(names(m))) stage[!m[[f]]] <- f
  # Aggregate-only first-exclusion detail retains early/GH unknown reasons.
  exclusion <- as.data.frame(table(stage), stringsAsFactors = FALSE)
  names(exclusion) <- c("first_failed_stage", "n")
  branch <- function(name, status, mask) {
    h <- as.data.frame(table(status[mask]), stringsAsFactors = FALSE)
    names(h) <- c("status", "n"); h$branch <- name; h
  }
  branches <- rbind(
    branch("oe_after_resident_singleton_age", a$oe$status, m$maternal_age_15_45),
    branch("chtn_after_known_oe20", a$chtn$status, m$known_oe_at_least_threshold),
    branch("early_after_clinical_eligibility", a$early$classification_status, keep),
    branch("gh_after_clinical_eligibility", a$gh$status, keep))
  list(data = out, ledger = rbind(natality_ledger(a, 20), natality_ledger(a, 28)),
    exclusion = exclusion, branches = branches, source_n = a$n,
    resident_n = sum(m$us_residents), resident_gh_yes = sum(m$us_residents & a$gh$value %in% 1),
    resident_gh_unknown = sum(m$us_residents & is.na(a$gh$value)))
}

natality_masks <- function(a, oe_threshold = 20) {
  if (!oe_threshold %in% c(20, 28)) stop("Unsupported feasibility threshold.")
  masks <- list(source_records = rep(TRUE, a$n))
  masks$us_residents <- a$restatus$value %in% 1:3
  masks$singleton <- masks$us_residents & a$plurality$value %in% 1
  masks$maternal_age_15_45 <- masks$singleton & a$age$value %in% 15:45
  masks$known_oe_at_least_threshold <- masks$maternal_age_15_45 &
    !is.na(a$oe$value) & a$oe$value >= oe_threshold
  masks$known_no_prepregnancy_hypertension <- masks$known_oe_at_least_threshold &
    a$chtn$value %in% 0
  masks$known_early_pattern <- masks$known_no_prepregnancy_hypertension &
    a$early$classification_status == "classified"
  masks$known_recorded_gh_pe <- masks$known_early_pattern & !is.na(a$gh$value)
  masks
}

natality_ledger <- function(a, oe_threshold = 20) {
  masks <- natality_masks(a, oe_threshold)
  kept <- vapply(masks, sum, numeric(1))
  parent_n <- c(a$n, head(kept, -1))
  data.frame(year = a$year, oe_threshold = oe_threshold, stage = names(kept),
             n_entering = unname(parent_n), n_retained = unname(kept),
             n_excluded = unname(parent_n - kept), row.names = NULL)
}
