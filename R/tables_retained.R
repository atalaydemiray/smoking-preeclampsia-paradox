# Helpers used by the current October publication tables.

crossover_model_wording <- c(
  "Women who smoked before pregnancy: core, same records" =
    "Women who smoked before pregnancy: core adjustment, same records",
  "Women who smoked before pregnancy: BMI/education, same records" =
    "Women who smoked before pregnancy: BMI and education, same records",
  "Women who smoked before pregnancy: gestation >=28 weeks" =
    "Women who smoked before pregnancy: gestation 28 weeks or more",
  "Women who smoked before pregnancy: age by BMI nuisance" =
    "Women who smoked before pregnancy: age-by-BMI nuisance interaction")

table_matrix <- function(header, body) {
  rows <- rbind(header, do.call(rbind, body), deparse.level = 0)
  storage.mode(rows) <- "character"
  dimnames(rows) <- NULL
  rows
}

hc0_roots <- function() {
  roots <- read_result("main", "root_reference.csv")
  roots <- roots[roots$covariance == "HC0" & roots$age >= 20 & roots$age <= 40, , drop = FALSE]
  if (anyDuplicated(roots$model_id))
    stop("more than one displayed crossover root for a model in root_reference.csv")
  rownames(roots) <- roots$model_id
  roots
}

crossover_regions <- function() {
  summary <- read_result("main", "crossover_summary.csv")
  summary <- summary[summary$covariance == "HC0", , drop = FALSE]
  as_printed <- function(x) {
    x <- trimws(as.character(x))
    x[x == "" | x == "Empty"] <- "none"
    gsub(" U ", " and ", x, fixed = TRUE)
  }
  data.frame(
    null_set = as_printed(summary$full_95_fixed_null_age_set),
    negative = as_printed(summary$simultaneous_strict_negative_intervals),
    positive = as_printed(summary$simultaneous_strict_positive_intervals),
    reversal = as.character(summary$simultaneous_reversal),
    row.names = summary$model_id, stringsAsFactors = FALSE)
}

table_s1 <- function() {
  # The flow is recorded a year at a time, so each rule is the sum across years. The obstetric
  # estimate threshold of 20 weeks is the one the primary analysis uses.
  flow <- read_result("descriptive", "common_clinical_flow_by_year.csv")
  flow <- flow[flow$oe_threshold == 20, , drop = FALSE]
  stages <- c(
    source_records = "Source records",
    us_residents = "US residents",
    singleton = "Singleton",
    maternal_age_15_45 = "Age 15–45",
    known_oe_at_least_threshold = "Known obstetric estimate 20–47 weeks",
    known_no_prepregnancy_hypertension = "Chronic hypertension explicitly absent")

  header <- c("Sequential rule", "Entering", "Retained", "Excluded")
  body <- lapply(names(stages), function(stage) {
    part <- flow[flow$stage == stage, , drop = FALSE]
    c(stages[[stage]], fmt_n(sum(part$n_entering)), fmt_n(sum(part$n_retained)),
      fmt_n(sum(part$n_excluded)))
  })
  rows <- table_matrix(header, body)
  check_people_first(rows, "Table_S1")
  rows
}

age_specific_table <- function(contrast, where) {
  estimates <- read_result("main", "main_age_estimates.csv")
  estimates <- estimates[estimates$covariance == "HC0" & estimates$contrast == contrast, ,
                         drop = FALSE]
  if (nrow(estimates) != 31) stop("expected ages 15 to 45 for the ", contrast, " comparison")

  header <- c("Age", "Reference N", "A0 risk/1,000", "A1 risk/1,000", "RR (95% CI)",
              "RD/1,000 (95% CI)")
  body <- lapply(seq_len(nrow(estimates)), function(i) {
    row <- estimates[i, , drop = FALSE]
    c(as.character(row$age), fmt_n(row$target_n),
      fmt_num(row$risk0 * 1000, 2), fmt_num(row$risk1 * 1000, 2),
      fmt_ci(row, "rr", 4), fmt_ci(row, "rd", 2, 1000))
  })
  rows <- table_matrix(header, body)
  check_people_first(rows, where)
  rows
}

table_s5 <- function() {
  labels <- read_result("main", "crossover_labels.csv")
  roots <- hc0_roots()
  regions <- crossover_regions()

  display <- relabel(labels$display_label)
  spelled_out <- display %in% names(crossover_model_wording)
  display[spelled_out] <- unname(crossover_model_wording[display[spelled_out]])

  header <- c("Model", "Crossover age (local 95% CI)", "95% null-age confidence set",
              "Lower risk supported (ages)", "Higher risk supported (ages)",
              "Opposite directions supported")
  body <- lapply(seq_len(nrow(labels)), function(i) {
    id <- labels$model_id[i]
    c(display[i], fmt_crossover(roots[id, ], 2),
      regions[id, "null_set"], regions[id, "negative"], regions[id, "positive"],
      if (identical(regions[id, "reversal"], "TRUE")) "Yes" else "No")
  })
  rows <- table_matrix(header, body)
  check_people_first(rows, "Table_S5")
  rows
}
