# Supplementary tables S6a to S10: alternative age splines, adjustment and fitting-population
# sensitivities, the paired fetal-inclusive difference, the linked mortality endpoints, the
# known-truth simulation and the interaction models.

# Flags reach these tables either as logicals or as the text R read from the CSV, so both
# spellings are accepted here rather than at every call site.
spec_flag_true <- function(x) toupper(as.character(x)) %in% "TRUE"

# A coverage or detection rate with its Monte Carlo interval, as a percentage. A scenario with
# no applicable replication has no rate to report, which is different from a rate of zero.
spec_percent_ci <- function(row) {
  if (as.numeric(row[["applicable_n"]]) == 0) return("Not applicable")
  paste0(fmt_num(100 * as.numeric(row[["rate"]]), 1), "% (",
         fmt_num(100 * as.numeric(row[["wilson_lower"]]), 1), "%, ",
         fmt_num(100 * as.numeric(row[["wilson_upper"]]), 1), "%)")
}

# The fitted crossover of a model, and whether its simultaneous band supports both signs.
# Roots outside ages 20 to 40 are edge artefacts of the spline, not the crossover the paper
# reports, so they are dropped before a model is looked up.
spec_reference_roots <- function() {
  roots <- read_result("main", "root_reference.csv")
  roots <- roots[roots$covariance == "HC0" & roots$age >= 20 & roots$age <= 40, ]
  split(roots, roots$model_id)
}

table_s6a <- function() {
  roots <- spec_reference_roots()
  summary_hc0 <- read_result("main", "crossover_summary.csv")
  summary_hc0 <- summary_hc0[summary_hc0$covariance == "HC0", ]

  # Each specification changes only the age basis; every other covariate and the records are
  # the same, so the change column reads as the cost of the spline choice alone.
  specs <- c(
    primary_main                 = "Original age spline",
    age_spec_fewer_age_knots     = "Two interior age knots (22,32)",
    age_spec_shifted_age_knots   = "Shifted age knots (23,29,36)",
    age_spec_more_age_knots      = "Four age knots (20,25,30,36)",
    age_spec_age_by_bmi_nuisance = "Original age spline plus age-by-BMI nuisance interaction")

  reference_age <- as.numeric(roots[["primary_main"]]$age[1])
  body <- lapply(names(specs), function(id) {
    r <- roots[[id]]
    reversal <- summary_hc0$simultaneous_reversal[summary_hc0$model_id == id][1]
    c(specs[[id]],
      fmt_num(r$age[1]),
      fmt_num(as.numeric(r$age[1]) - reference_age),
      paste0(fmt_num(r$delta_lower[1]), "–", fmt_num(r$delta_upper[1])),
      if (spec_flag_true(reversal)) "Supported" else "Not established")
  })

  rows <- rbind(
    c("Specification", "Fitted null age", "Change, years", "Local 95% CI",
      "Simultaneous opposite signs"),
    do.call(rbind, body))
  check_people_first(rows, "Table_S6a")
  rows
}

table_s6b <- function() {
  # The reference row and four sensitivities. Fit N and reference N differ only where a
  # specification is fitted on more records than it standardizes to.
  specs <- c(
    primary_main      = "full main",
    core_common_cc    = "core common cc",
    augmented_common_cc = "augmented common cc",
    core_available_cc = "core available cc",
    main_2018_2024_cc = "main 2018 2024 cc")

  body <- lapply(names(specs), function(id) {
    counts <- read_result("models", paste0(id, "_counts.csv"))
    est <- read_result("models", paste0(id, "_HC0_overall_standardized.csv"))
    rr <- est[est$measure == "rr", ][1, ]
    rd <- est[est$measure == "rd", ][1, ]
    c(specs[[id]], fmt_n(counts$fit_n[1]), fmt_n(counts$reference_n[1]),
      fmt_estimate_ci(rr, 4), fmt_estimate_ci(rd, 3, 1000))
  })

  rows <- rbind(
    c("Model", "Fit N", "Reference N", "RR (95% CI)", "RD/1,000 (95% CI)"),
    do.call(rbind, body))
  check_people_first(rows, "Table_S6b")
  rows
}

table_s7 <- function() {
  paired <- read_result("main", "paired_GH_overall_differences.csv")
  paired <- paired[paired$measure == "difference_RD", ]

  # Each row pairs one fitting population with itself plus the fetal records it can match, so
  # the difference is within a shared standardization reference rather than across two cohorts.
  models <- c("all_years_shared_core", "all_years_shared_augmented",
              "reporting_years_shared_core", "reporting_years_shared_augmented")

  body <- lapply(models, function(id) {
    r <- paired[paired$model == id, ][1, ]
    c(if (startsWith(id, "all")) "2018–2024" else "2019–2021, 2024",
      if (endsWith(id, "_core")) "Core" else "Augmented",
      fmt_n(r$live_n), fmt_n(r$fetal_n), fmt_estimate_ci(r, 4, 1000))
  })

  rows <- rbind(
    c("Period", "Adjustment", "Live records", "Fetal records added",
      "Difference in RD/1,000 (95% CI)"),
    do.call(rbind, body))
  check_people_first(rows, "Table_S7")
  rows
}

table_s8 <- function() {
  endpoints <- read_result("main", "source_specific_endpoints.csv")
  endpoints <- endpoints[endpoints$covariance == "HC0" & endpoints$family == "linked", ]
  ratios <- endpoints[endpoints$measure == "rr", ]

  body <- lapply(seq_len(nrow(ratios)), function(i) {
    r <- ratios[i, ]
    rd <- endpoints[endpoints$scenario == r$scenario & endpoints$measure == "rd", ][1, ]
    c(gsub("_", " ", r$scenario),
      fmt_age_span(strsplit(r$years, ",")[[1]]),
      paste0(fmt_n(r$fit_n), " / ", fmt_n(r$events)),
      fmt_estimate_ci(r), fmt_estimate_ci(rd, 3, 1000))
  })

  rows <- rbind(
    c("Scenario", "Years", "Records / events", "RR (95% CI)", "RD/1,000 (95% CI)"),
    do.call(rbind, body))
  check_people_first(rows, "Table_S8")
  rows
}

table_s9 <- function() {
  sim <- read_result("main", "crossover_simulation_summary.csv")
  scenarios <- unique(sim$scenario)

  body <- lapply(scenarios, function(s) {
    block <- sim[sim$scenario == s, ]
    pick <- function(metric) block[block$metric == metric, ][1, ]
    pointwise <- pick("pointwise_fixed_root_covered")
    c(gsub("_", " ", s),
      fmt_n(pointwise$attempts),
      spec_percent_ci(pointwise),
      spec_percent_ci(pick("global_null_whole_domain_covered")),
      spec_percent_ci(pick("simultaneous_reversal_detected")))
  })

  rows <- rbind(
    c("Scenario", "Replications", "Fixed-null-age coverage (95% Monte Carlo CI)",
      "Whole-domain coverage under global null",
      "Simultaneous reversal detected (95% Monte Carlo CI)"),
    do.call(rbind, body))
  check_people_first(rows, "Table_S9")
  rows
}

table_s10 <- function() {
  age_rows <- read_result("main", "interaction_age_all_models.csv")
  overall <- read_result("main", "interaction_overall.csv")

  # These are the short names the stored estimates use, so they are lookup keys rather than wording a
  # reader sees. relabel() turns them into the people-first labels of the published table, and
  # check_people_first() refuses to write the table if any of them survives.
  contrasts <- c(primary = "Within smokers", broad = "Both early windows",
                 prepregnancy = "Prepregnancy only")
  # Only the comparison among women who smoked before pregnancy carries the two single-term
  # additions; the other two were fitted with the original model and with both additions together.
  models_for <- function(contrast) {
    if (contrast == "primary")
      c("original", "smoking_by_bmi", "smoking_and_age_by_year", "combined")
    else c("original", "combined")
  }
  # "bmi" is a variable name, not a word for a reader.
  spell_out <- c("smoking by bmi" = "smoking-by-BMI")

  body <- list()
  for (contrast in names(contrasts)) {
    for (model in models_for(contrast)) {
      a <- age_rows[age_rows$contrast == contrast & age_rows$model == model, ]
      a <- a[order(as.integer(a$age)), ]
      o <- overall[overall$contrast == contrast & overall$model == model, ]

      # The sign bracket is the pair of neighbouring integer ages the fitted risk difference
      # crosses zero between, restricted to the interior where a crossover is interpretable.
      left <- seq_len(nrow(a) - 1)
      crosses <- a$rd[left] < 0 & a$rd[left + 1] > 0 &
        as.integer(a$age[left]) >= 20 & as.integer(a$age[left]) < 40
      brackets <- paste(a$age[left][crosses], "to", a$age[left + 1][crosses])

      label <- paste0(contrasts[[contrast]], ": ", gsub("_", " ", model))
      label <- relabel(label)
      for (term in names(spell_out)) label <- gsub(term, spell_out[[term]], label, fixed = TRUE)

      body[[length(body) + 1]] <- c(
        label,
        fmt_estimate_ci(o[o$measure == "rr", ][1, ]),
        fmt_estimate_ci(o[o$measure == "rd", ][1, ], 3, 1000),
        if (length(brackets)) paste(brackets, collapse = "; ") else "Not established",
        fmt_age_span(a$age[spec_flag_true(a$negative_grid_supported)]),
        fmt_age_span(a$age[spec_flag_true(a$positive_grid_supported)]))
    }
  }

  rows <- rbind(
    c("Contrast and specification", "Overall RR (95% CI)", "Overall RD /1,000 (95% CI)",
      "Fitted sign bracket, years", "Negative ages supported", "Positive ages supported"),
    do.call(rbind, body))
  check_people_first(rows, "Table_S10")
  rows
}
