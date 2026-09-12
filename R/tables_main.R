# Table 1, Table 2, Table 3 and Supplementary Tables S1 to S5 of the manuscript, built from the
# aggregate estimates in results/. Each function returns a character matrix whose first row is
# the header; none of these ten tables carries a footnote row.

# The three main comparisons, in the order the manuscript presents them. Table 2 and Table 3
# name the third comparison without the "irrespective of T1" qualifier that the supplementary
# tables keep, so the wording of these two is fixed here instead of read from the estimates.
main_comparisons <- c(
  primary = "Continued vs stopped smoking in T1, women who smoked before pregnancy",
  broad = "Smoking before pregnancy and in T1 vs neither period",
  prepregnancy = "Any prepregnancy smoking vs none")

main_crossover_models <- c(
  primary = "primary_main", broad = "broad_main", prepregnancy = "prepregnancy_main")

# Table 1 groups the covariates the way the manuscript reads them, which is not the order the
# models list them in. Only the grouping is named here; the levels inside each group keep the
# order they have in the estimates file.
table_1_variables <- c(
  age = "Maternal age, years",
  bmi = "Prepregnancy BMI, kg/m²",
  race_ethnicity = "Race/ethnicity",
  education4 = "Education",
  prior_living4 = "Prior living children",
  prepreg_diabetes = "Prepregnancy diabetes",
  prior_preterm = "Previous preterm birth",
  prior_cesarean = "Previous cesarean",
  nativity = "Nativity",
  prepreg_dose5 = "Prepregnancy cigarettes/day")

# Four specification names reach the estimates abbreviated. The revision spells them out.
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

# Fitted crossover ages, one row per model, keyed by model identifier. root_reference.csv holds
# every root the search returned; the manuscript displays the interior one, which is why the two
# fetal-source reporting-year models lose their second root out near age 43 here.
hc0_roots <- function() {
  roots <- read_result("main", "root_reference.csv")
  roots <- roots[roots$covariance == "HC0" & roots$age >= 20 & roots$age <= 40, , drop = FALSE]
  if (anyDuplicated(roots$model_id))
    stop("more than one displayed crossover root for a model in root_reference.csv")
  rownames(roots) <- roots$model_id
  roots
}

# The 95% null-age confidence set and the two simultaneous sign regions, keyed by model
# identifier. The assembler joins components with "U" and writes "Empty" for a region with no
# interval; in print those read "and" and "none".
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

table_1 <- function() {
  base <- read_result("descriptive", "baseline_all_three_contrasts.csv")
  base <- base[base$contrast == "primary" & base$variable != "year_factor", , drop = FALSE]
  overall <- read_result("main", "main_overall.csv")
  primary <- overall[overall$contrast == "primary", , drop = FALSE]

  body <- list()
  for (variable in names(table_1_variables)) {
    part <- base[base$variable == variable, , drop = FALSE]
    for (level in unique(part$level)) {
      cells <- part[part$level == level, , drop = FALSE]
      label <- table_1_variables[[variable]]
      if (nzchar(level)) label <- paste0(label, ": ", level)
      body[[length(body) + 1]] <- c(label,
        cells$display[cells$A == 0], cells$display[cells$A == 1])
    }
  }

  # The column headers carry the arm sizes, so they come from the same fit the rows describe.
  header <- c("Characteristic",
              paste0("T1 abstinence\nN=", fmt_n(primary$A0)),
              paste0("T1 smoking\nN=", fmt_n(primary$A1)))
  rows <- table_matrix(header, body)
  check_people_first(rows, "Table_1")
  rows
}

table_2 <- function() {
  overall <- read_result("main", "main_overall.csv")
  header <- c("Comparison", "Records / GH/PE events",
              "Standardized risk per 1,000, reference (95% CI)",
              "Standardized risk per 1,000, exposed (95% CI)",
              "Adjusted RR (95% CI)", "RD per 1,000 (95% CI)")
  body <- lapply(names(main_comparisons), function(key) {
    row <- overall[overall$contrast == key, , drop = FALSE]
    c(main_comparisons[[key]],
      paste0(fmt_n(row$n), " / ", fmt_n(row$events)),
      fmt_overall(row, "risk0", 1), fmt_overall(row, "risk1", 1),
      fmt_overall(row, "rr", 3), fmt_overall(row, "rd", 2))
  })
  rows <- table_matrix(header, body)
  check_people_first(rows, "Table_2")
  rows
}

table_3 <- function() {
  roots <- hc0_roots()
  regions <- crossover_regions()
  header <- c("Comparison", "Crossover age, years (local 95% CI)",
              "95% null-age confidence set, all components",
              "Ages with lower risk supported (simultaneous 95% band)",
              "Ages with higher risk supported (simultaneous 95% band)")
  body <- lapply(names(main_comparisons), function(key) {
    id <- main_crossover_models[[key]]
    c(main_comparisons[[key]], fmt_crossover(roots[id, ], 1),
      regions[id, "null_set"], regions[id, "negative"], regions[id, "positive"])
  })
  rows <- table_matrix(header, body)
  check_people_first(rows, "Table_3")
  rows
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

table_s2 <- function() {
  excluded <- read_result("descriptive", "numeric_cc_exclusions_overall.csv")
  overall <- read_result("main", "main_overall.csv")
  label <- relabel(overall$label)
  names(label) <- overall$contrast

  # The complete-case counts and the fitted models have to describe the same records.
  fitted <- overall[match(excluded$contrast, overall$contrast), , drop = FALSE]
  if (!all(excluded$included_n == fitted$n) || !all(excluded$included_events == fitted$events))
    stop("complete-case counts disagree with the fitted models in main_overall.csv")

  header <- c("Contrast", "Eligible", "Included", "Excluded", "Excluded, %",
              "GH/PE per 1,000: included", "GH/PE per 1,000: excluded")
  body <- lapply(seq_len(nrow(excluded)), function(i) {
    row <- excluded[i, , drop = FALSE]
    c(label[[row$contrast]], fmt_n(row$eligible_n), fmt_n(row$included_n), fmt_n(row$excluded_n),
      fmt_num(row$excluded_pct, 2), fmt_num(row$included_GH_risk_per1000, 2),
      fmt_num(row$excluded_GH_risk_per1000, 2))
  })
  rows <- table_matrix(header, body)
  check_people_first(rows, "Table_S2")
  rows
}

table_s3 <- function() {
  overall <- read_result("main", "main_overall.csv")
  header <- c("Contrast", "A0 risk per 1,000 (95% CI)", "A1 risk per 1,000 (95% CI)",
              "RR (95% CI)", "RD per 1,000 (95% CI)")
  body <- lapply(seq_len(nrow(overall)), function(i) {
    row <- overall[i, , drop = FALSE]
    c(relabel(row$label), fmt_overall(row, "risk0", 3), fmt_overall(row, "risk1", 3),
      fmt_overall(row, "rr", 3), fmt_overall(row, "rd", 3))
  })
  rows <- table_matrix(header, body)
  check_people_first(rows, "Table_S3")
  rows
}

# Tables S4a to S4c are the same display for the three comparisons: one row per year of age,
# with the risk in each arm on the per-1,000 scale the manuscript reports.
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

table_s4a <- function() age_specific_table("primary", "Table_S4a")

table_s4b <- function() age_specific_table("broad", "Table_S4b")

table_s4c <- function() age_specific_table("prepregnancy", "Table_S4c")

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
