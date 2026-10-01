source("R/01_measurement_helpers.R")
source("R/10_model_specification.R")
checks <- 0L
check <- function(x) { stopifnot(isTRUE(x)); checks <<- checks + 1L }
fails <- function(expr) inherits(tryCatch({force(expr); NULL}, error = identity), "error")
spec <- function(tier = "natality_main", contrast = "within_prepregnancy_smokers", race = "edited")
  lock_sep_model_specification(tier, contrast, years = 2019:2024,
    age_knots = c(20, 28, 36), bmi_knots = if (tier == "shared_core") NULL else c(22, 27, 32),
    race_mode = race)
fixture <- function(n = 14L, fetal = FALSE) {
  d <- data.frame(source_row = seq_len(n), year = rep(2019:2024, length.out = n),
    age = rep(c(15, 20, 24, 28, 33, 38, 44), length.out = n), oe_weeks = 38,
    gh = rep(c(0, 1), length.out = n), gh_status = "observed",
    source_restatus = "1", source_dplural = "1", source_rf_phype = "N",
    source_f_rf_phyper = "1", source_cig_0 = "10", source_f_cigs_0 = "1",
    source_cig_1 = rep(c("00", "05"), length.out = n), source_f_cigs_1 = "1",
    race_hispanic_origin = rep(1:7, length.out = n), race_hispanic_origin_status = "observed",
    race_imputation_status = "not_imputed", education = rep(1:8, length.out = n),
    education_status = "observed", prepregnancy_bmi = rep(c(13, 19, 23, 27, 32, 42, 69.9), length.out = n),
    prepregnancy_bmi_status = "observed", prepregnancy_diabetes = rep(0:1, length.out = n),
    prepregnancy_diabetes_status = "observed", prior_liveborn_children_now_living = rep(0:6, length.out = n),
    prior_liveborn_children_now_living_status = "observed", previous_preterm_birth = rep(0:1, length.out = n),
    previous_preterm_birth_status = "observed", previous_cesarean = rep(1:0, length.out = n),
    previous_cesarean_status = "observed", nativity = rep(1:2, length.out = n),
    nativity_status = "observed", stringsAsFactors = FALSE)
  if (fetal) {
    names(d)[match(c("source_rf_phype", "source_f_rf_phyper", "source_cig_0", "source_f_cigs_0",
      "source_cig_1", "source_f_cigs_1"), names(d))] <- c("source_phyp", "source_f_phyp", "source_cig0",
      "source_f_cig0", "source_cig1", "source_f_cig1")
    d$clinical_member <- TRUE
  }
  d
}
d <- fixture(); s <- spec(); p <- prepare_sep_model_data(d, s, "natality")
check(p$n_selected == nrow(d)); check(p$baseline_records_dropped == 0L)
check(!p$requires_missingness_resolution); check(all(p$data$A == rep(0:1, 7)))
check(identical(p$data$Y, as.numeric(d$gh)))
check(identical(p$identity$input_row, seq_len(nrow(d))))
check(!any(grepl("basis", names(p$data))))
check(all(c("bmi", "education4", "prior_preterm", "prior_cesarean", "nativity", "prepreg_dose5") %in% names(p$data)))
check(identical(levels(p$data$education4), c("Less than high school", "High school or GED",
  "Some college or associate", "Bachelor or higher")))
check(identical(as.character(p$data$education4[1:8]), levels(p$data$education4)[c(1, 1, 2, 3, 3, 4, 4, 4)]))
check(identical(levels(p$data$prepreg_diabetes), c("No", "Yes")))
check(identical(as.character(p$data$prior_living4[1:7]), c("0", "1", "2", rep("3 or more", 4))))
check(!any(s$forbidden_adjustments %in% all.vars(s$formula)))
check(all(c("age", "A", "bmi") %in% all.vars(s$formula)))
check(!grepl("df[ ]*=", paste(deparse(s$formula), collapse = "")))
check(grepl("knots = c", paste(deparse(s$formula), collapse = ""), fixed = TRUE))
check(inherits(s$age_spec, "sep_age_spec"))

# Factor labels, not arbitrary internal integer levels, determine source codes.
e <- d; e$education <- factor(e$education, levels = 8:1)
e$race_hispanic_origin <- factor(e$race_hispanic_origin, levels = 7:1)
e$prepregnancy_diabetes <- factor(e$prepregnancy_diabetes, levels = 1:0)
q <- prepare_sep_model_data(e, s, "natality")
check(identical(q$data, p$data))

# Every official smoking-dose boundary and topcode; no exact-98 dose inference.
e <- fixture(12); e$source_cig_0 <- as.character(c(1, 5, 6, 10, 11, 20, 21, 40, 41, 97, 98, 99))
q <- prepare_sep_model_data(e, s, "natality")
check(q$n_selected == 11L)
check(identical(as.character(q$data$prepreg_dose5), c("1-5", "1-5", "6-10", "6-10",
  "11-20", "11-20", "21-40", "21-40", "41 or more", "41 or more", "41 or more")))
check(as.character(q$missingness$prepreg_dose5[11]) == "observed_topcoded_98_or_more")
e$source_f_cigs_0[1] <- "0"; check(prepare_sep_model_data(e, s, "natality")$n_selected == 10L)
e$source_cig_0[1] <- "100"; check(fails(prepare_sep_model_data(e, s, "natality")))

# Complementary and prepregnancy-only contrasts have different eligibility.
e <- fixture(6); e$source_cig_0 <- c("00", "10", "10", "00", "10", "99")
e$source_cig_1 <- c("00", "05", "00", "05", "99", "00")
q <- prepare_sep_model_data(e, spec(contrast = "complementary"), "natality")
check(identical(q$identity$input_row, 1:2)); check(identical(q$data$A, 0:1))
check(!"prepreg_dose5" %in% names(q$data))
q <- prepare_sep_model_data(e, spec(contrast = "prepregnancy_only"), "natality")
check(identical(q$identity$input_row, 1:5)); check(identical(q$data$A, c(0L, 1L, 1L, 0L, 1L)))
e$source_cig_1 <- e$source_f_cigs_1 <- NULL
check(prepare_sep_model_data(e, spec(contrast = "prepregnancy_only"), "natality")$n_selected == 5L)
check(fails(prepare_sep_model_data(e, s, "natality")))

# Baseline unknowns are retained with the original missingness mechanism.
e <- d; e$education[1] <- NA; e$education_status[1] <- "unknown_code"
e$prepregnancy_bmi[2] <- NA; e$prepregnancy_bmi_status[2] <- "not_reported"
e$nativity[3] <- NA; e$nativity_status[3] <- "unknown_reporting_status"
e$race_hispanic_origin[4] <- NA; e$race_hispanic_origin_status[4] <- "unknown_blank"
q <- prepare_sep_model_data(e, s, "natality")
check(q$n_selected == nrow(d)); check(q$requires_missingness_resolution)
check(q$baseline_records_dropped == 0L)
check(is.na(q$data$education4[1]) && as.character(q$missingness$education4[1]) == "item_unknown")
check(is.na(q$data$bmi[2]) && as.character(q$missingness$bmi[2]) == "structural_not_reporting")
check(is.na(q$data$nativity[3]) && as.character(q$missingness$nativity[3]) == "reporting_support_unknown")
check(as.character(q$original_status$race_ethnicity[4]) == "unknown_blank")
check(sum(q$missingness_counts$n[q$missingness_counts$field == "bmi"]) == nrow(d))
e$prepregnancy_bmi[2] <- 25; check(fails(prepare_sep_model_data(e, s, "natality")))
e <- d; e$education[1] <- NA; e$education_status[1] <- "invalid_code"
check(fails(prepare_sep_model_data(e, s, "natality")))
e <- d; e$education_status[1] <- "invalid_reporting_flag"; check(fails(prepare_sep_model_data(e, s, "natality")))
e <- d; e$prepregnancy_bmi[1] <- 70; check(fails(prepare_sep_model_data(e, s, "natality")))
e <- d; e$education[1] <- NA; check(fails(prepare_sep_model_data(e, s, "natality")))

# Race masking never converts known Hispanic origin into missing race/ethnicity.
e <- d; e$race_imputation_status[c(1, 2, 7)] <- "unknown_race_imputed"
e$race_imputation_status[3] <- "formerly_other_race_imputed"
e$race_imputation_status[4] <- "undocumented_source_code"
q <- prepare_sep_model_data(e, spec(race = "mask_original_unknown_nonhispanic"), "natality")
check(identical(which(q$race_mask), 1:2)); check(all(is.na(q$data$race_ethnicity[1:2])))
check(as.character(q$data$race_ethnicity[7]) == "Hispanic")
check(as.character(q$race_allowed_set[7]) == "hispanic_category_7")
check(all(as.character(q$race_allowed_set[1:2]) == "nonhispanic_categories_1_to_6"))
check(!anyNA(q$data$race_ethnicity[3:7]))
check(as.character(q$missingness$race_ethnicity[1]) == "source_imputed_original_unknown_sensitivity")
check(q$n_selected == nrow(d))
e$race_imputation_status[1] <- "field_not_supplied"
check(fails(prepare_sep_model_data(e, spec(race = "mask_original_unknown_nonhispanic"), "natality")))
check(prepare_sep_model_data(e, s, "natality")$n_selected == nrow(d))

# Shared models cannot accidentally include unavailable fetal history/nativity.
f <- fixture(fetal = TRUE)
f$nativity <- f$nativity_status <- f$previous_preterm_birth <- f$previous_preterm_birth_status <- NULL
q <- prepare_sep_model_data(f, spec("shared_core"), "fetal_death")
check(!any(c("bmi", "education4", "nativity", "prior_preterm", "prior_cesarean") %in% names(q$data)))
q <- prepare_sep_model_data(f, spec("shared_augmented"), "fetal_death")
check(all(c("bmi", "education4") %in% names(q$data)))
check(!any(c("source_type", "oe_weeks") %in% all.vars(q$spec$formula)))
check(fails(prepare_sep_model_data(f, s, "fetal_death")))
f$clinical_member[1] <- FALSE; check(fails(prepare_sep_model_data(f, spec("shared_core"), "fetal_death")))

# Strict membership identity, calendar lock and input validation.
e <- d; e$primary_member <- TRUE; check(prepare_sep_model_data(e, s, "natality")$n_selected == nrow(d))
e$primary_member[1] <- FALSE; check(fails(prepare_sep_model_data(e, s, "natality")))
e <- d; e$source_row[2] <- e$source_row[1]; e$year[2] <- e$year[1]
check(fails(prepare_sep_model_data(e, s, "natality")))
e <- d; e$year[1] <- 2018; check(fails(prepare_sep_model_data(e, s, "natality")))
e <- d; e$source_f_rf_phyper[1] <- "Z"; check(fails(prepare_sep_model_data(e, s, "natality")))
e <- d; e$age[1:2] <- c(14, 45); check(prepare_sep_model_data(e, s, "natality")$n_selected == nrow(d) - 2L)
e <- d; e$gh[1] <- NA; e$gh_status[1] <- "unknown_code"
check(prepare_sep_model_data(e, s, "natality")$n_selected == nrow(d) - 1L)
check(fails(prepare_sep_model_data(d, s, "livebirth")))
check(fails(lock_sep_model_specification("shared_core", "complementary", 2019:2024, c(20, 20, 36))))
check(fails(lock_sep_model_specification("natality_main", "complementary", 2019:2024, c(20, 28, 36))))
check(fails(lock_sep_model_specification("shared_core", "complementary", c(2024, 2019), c(20, 28, 36))))
check(fails(lock_sep_model_specification("shared_core", "complementary", 2019:2024, c(15, 28, 36))))

# Dynamic formula bases remain identical in one-row candidate evaluations.
p <- prepare_sep_model_data(d, s, "natality")
X <- stats::model.matrix(s$formula, p$data)
single <- do.call(rbind, lapply(seq_len(nrow(p$data)), function(i)
  stats::model.matrix(s$formula, p$data[i, , drop = FALSE])))
check(isTRUE(all.equal(unname(X), unname(single), tolerance = 1e-13, check.attributes = FALSE)))
check(identical(colnames(X), colnames(single)))
changed <- p$data; changed$bmi[4] <- changed$bmi[4] + 2
delta <- stats::model.matrix(s$formula, changed) - X
bmi_columns <- grepl("ns(bmi", colnames(X), fixed = TRUE)
check(any(abs(delta[4, bmi_columns]) > 1e-8)); check(all(delta[, !bmi_columns] == 0))
ready <- refresh_sep_model_bases(p$data, s)
check(all(s$fit_covariates %in% names(ready)))
check(max(abs(as.matrix(ready[s$bmi_spec$columns]) -
  splines::ns(p$data$bmi, knots = c(22, 27, 32), Boundary.knots = c(13, 69.9)))) < 1e-13)
changed <- ready; changed$bmi[4] <- 30
again <- refresh_sep_model_bases(changed, s)
check(any(abs(unlist(again[4, s$bmi_spec$columns]) - unlist(ready[4, s$bmi_spec$columns])) > 1e-8))
changed$bmi[3] <- NA; again <- refresh_sep_model_bases(changed, s)
check(all(is.na(again[3, s$bmi_spec$columns])))
check(!any(s$age_spec$columns %in% names(ready)))
check(sum(p$eligibility_ledger$n_excluded) == p$n_excluded_eligibility)
e <- d; e$prepregnancy_bmi <- NA; e$prepregnancy_bmi_status <- "not_reported"
q <- prepare_sep_model_data(e, s, "natality")
check(q$n_selected == nrow(d)); check(all(is.na(q$data$bmi)))
check(all(is.na(refresh_sep_model_bases(q$data, s)[s$bmi_spec$columns])))
e <- d; e$source_type <- "fetal_death"; check(fails(prepare_sep_model_data(e, s, "natality")))
check(identical(d, fixture()))

# Interoperate with the real pure adapters, using constructed source records only.
source("R/03_natality_adapter.R")
nr <- as.data.frame(setNames(rep(list(rep(1, 6)), length(natality_selected_columns(2024, TRUE))),
  natality_selected_columns(2024, TRUE)), stringsAsFactors = FALSE)
nr$dob_yy <- 2024; nr$mager <- 30; nr$oegest_comb <- 38
nr$imp_plur <- nr$mage_impflg <- ""
for (f in natality_provenance_columns()) nr[[f]] <- ""
for (f in c("rf_phype", "rf_ghype", "rf_pdiab", "rf_ppterm", "rf_cesar", "rf_inftr")) nr[[f]] <- "N"
nr$rf_artec <- "X"; nr$bmi <- 27.3; nr$priorlive <- nr$priordead <- 0
nr$cig_0 <- c(5, 98, 10, 99, 0, 0); nr$cig_1 <- c(5, 0, 99, 0, 0, 5)
nd <- natality_input_chunk(nr, 2024)$data
np <- prepare_sep_model_data(nd, s, "natality")
check(identical(np$identity$source_row, c(1, 2))); check(identical(np$data$A, c(1L, 0L)))
np0 <- prepare_sep_model_data(nd, spec(contrast = "prepregnancy_only"), "natality")
check(identical(np0$identity$source_row, c(1, 2, 3, 5, 6)))
source("R/09_fetal_clinical_adapter.R")
fs <- jsonlite::fromJSON("config/fetal_schema.json", simplifyVector = FALSE)
fs <- fetal_schema_with_provenance_r(fs)
fr <- as.data.frame(setNames(lapply(fs$fields, function(f) {
  z <- fs$domains[[f$kind]]
  rep(if (!is.null(z$allowed)) unlist(z$allowed)[1] else as.character(z$known_min), 6)
}), names(fs$fields)), stringsAsFactors = FALSE)
for (f in names(fr)[startsWith(names(fr), "F_")]) fr[[f]] <- "1"
fr$DOD_YY <- "2024"; fr$MAGER <- "30"; fr$OE_GEST <- "38"; fr$OE_TABFLG <- "2"
fr$PHYP <- fr$GHYP <- fr$PDIAB <- fr$INFTR <- "N"; fr$ART <- "X"; fr$BMI <- "27.3"
fr$CIG0 <- c("05", "98", "10", "99", "00", "00"); fr$CIG1 <- c("05", "00", "99", "00", "00", "05")
fd <- adapt_fetal_r(fr, fs, 2024)$data
fp <- prepare_sep_model_data(fd, spec("shared_augmented"), "fetal_death")
check(identical(fp$identity$source_row, c(1, 2))); check(identical(fp$data$A, c(1L, 0L)))
fp0 <- prepare_sep_model_data(fd, spec("shared_core", "prepregnancy_only"), "fetal_death")
check(identical(fp0$identity$source_row, c(1, 2, 3, 5, 6)))
# Pooling sources preserves source-specific identity without making source a regressor.
common <- union(names(nd), names(fd)); nn <- as.data.frame(nd); ff <- as.data.frame(fd)
for (f in setdiff(common, names(nn))) nn[[f]] <- NA
for (f in setdiff(common, names(ff))) ff[[f]] <- NA
# Only fetal rows have clinical_member metadata; natality NA is not FALSE.
pooled <- rbind(nn[names(ff)], ff)
pp <- prepare_sep_model_data(pooled, spec("shared_augmented"), rep(c("natality", "fetal_death"), each = 6))
check(pp$n_selected == 4L)
check(identical(as.character(pp$data$prepreg_dose5), rep(c("1-5", "41 or more"), 2)))
check(!"source_type" %in% names(pp$data))
cat("Passed", checks, "synthetic model-specification checks; no fitting or study data reads.\n")
