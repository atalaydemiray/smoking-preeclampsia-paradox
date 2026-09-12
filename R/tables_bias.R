# The five quantitative bias tables of the supplement: E-values by age, live-birth selection,
# exposure misreporting unrelated to GH/PE, exposure misreporting related to GH/PE, and outcome
# recording sensitivity. Each function returns one table, header row first and footnote row last.

# Wording of the three comparisons, in the order every bias table reports them. The names are how
# the bias outputs store a contrast, the values are what a reader sees.
bias_comparisons <- c(
  primary      = "Comparison 1: continued vs stopped in T1",
  broad        = "Comparison 2: both periods vs neither",
  prepregnancy = "Comparison 3: any prepregnancy smoking vs none")

bias_contrasts <- names(bias_comparisons)

# A blank cell in a bias output means the scenario has no value there, and the tables keep that
# cell blank, so an absent number must not become the words fmt_num() would give a reader.
bias_num <- function(x, digits = 2) {
  if (length(x) != 1) return("")
  x <- as.character(x)
  if (is.na(x) || !nzchar(trimws(x))) return("")
  fmt_num(x, digits)
}

# The overall row of an age-indexed output carries no age, which reading the file turns into NA.
age_key <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

# Scenario grids are matched on numbers read back from a file, so a level is picked with a
# tolerance rather than on exact equality.
same_level <- function(x, level) !is.na(x) & abs(as.numeric(x) - level) < 1e-9

# A scenario level is shown the way the grid states it, so 1 stays 1 rather than becoming 1.000.
level_label <- function(x) {
  format(as.numeric(x), trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
}

# Header, body and the footnote alone on the last row, which is how every submitted table is laid
# out and how the supplement typesets the note under the rule.
bias_table <- function(header, body, footnote) {
  rows <- rbind(header, body, c(footnote, rep("", length(header) - 1)), deparse.level = 0)
  dimnames(rows) <- NULL
  storage.mode(rows) <- "character"
  rows
}


# E-value for each comparison overall and at every age.
table_s11 <- function() {
  ev <- read_result("bias", "S_evalue_by_age_all_contrasts.csv")
  ev$key <- paste(ev$contrast, age_key(ev$age))

  header <- c("Age", unlist(lapply(bias_contrasts, function(ct)
    paste0(bias_comparisons[[ct]], ": ",
           c("RR (95% CI)", "E-value", "E-value, CI limit")))))

  body <- t(vapply(c("", as.character(15:45)), function(a) {
    cells <- unlist(lapply(bias_contrasts, function(ct) {
      r <- ev[ev$key == paste(ct, a), ]
      c(fmt_ci(r, "rr", 3), bias_num(r$evalue_point, 2), bias_num(r$evalue_ci_limit, 2))
    }))
    c(if (nzchar(a)) a else "Overall", cells)
  }, character(length(header))))

  rows <- bias_table(header, body, paste(
    "E-value: minimum risk ratio an unmeasured factor would need with both the exposure and GH/PE",
    "(or with both selection and GH/PE) to move the estimate to the null; 1 means the confidence",
    "interval already includes the null. Algebraic benchmark, not a validated causal bound."))
  check_people_first(rows, "Table S11")
  rows
}


# Live-birth selection: how much more pregnancy loss among women who smoked would be needed for
# selection alone to produce the observed inverse association.
table_s12 <- function() {
  sel <- read_result("bias", "S_selection_left_truncation_compact.csv")
  sel <- sel[same_level(sel$type_prevalence, 0.1), ]

  header <- c(
    "Comparison", "Assumed risk ratio of the hidden hypertension-prone group", "Age",
    "Baseline pregnancy loss (scenario)", "Observed RR",
    "Loss differential needed to reproduce the observed RR under a true null",
    "Implied smoking loss RR in other pregnancies",
    paste("RR predicted by the mechanism when calibrated at the age of strongest",
          "inverse association"))

  scenario_row <- function(r, comparison, level) {
    # A scenario ends in one of three ways: the observed ratio is already at or above 1 and needs
    # no differential, a differential reproduces it, or no differential is large enough.
    if (as.numeric(r$observed_rr) >= 1) {
      needed <- "Not needed (observed ratio at or above 1)"
      other <- ""
    } else if (!is.na(r$feasible) && as.numeric(r$feasible) == 1) {
      needed <- bias_num(r$required_delta, 1)
      other <- bias_num(r$loss_rr_other, 2)
    } else {
      needed <- paste0("Not reachable (minimum ", bias_num(r$min_reachable_rr, 3), ")")
      other <- ""
    }
    # Calibrating at the age of strongest inverse association leaves no predicted ratio when the
    # mechanism has already removed every pregnancy of the hidden group.
    predicted <- bias_num(r$predicted_rr_calibrated_at_min, 3)
    if (!nzchar(predicted)) {
      predicted <- if (nzchar(bias_num(r$calibrated_delta, 2)))
        "All type pregnancies lost" else "Calibration age not reachable by the mechanism"
    }
    c(comparison, level, as.character(r$age), bias_num(r$baseline_loss, 2),
      bias_num(r$observed_rr, 3), needed, other, predicted)
  }

  body <- NULL
  for (ct in c("primary", "broad")) {
    for (level in c(2, 3, 5)) {
      part <- sel[sel$contrast == ct & same_level(sel$type_gh_pe_rr, level), ]
      for (i in seq_len(nrow(part))) {
        body <- rbind(body, scenario_row(part[i, ], bias_comparisons[[ct]], level_label(level)))
      }
    }
  }

  rows <- bias_table(header, body, paste(
    "Type prevalence 10%; smoking loss RR 1.23 (Pineles 2014); age-graded loss curve anchored on",
    "Magnus 2019 (10% at 25-29, 53% at 45+; intermediate values are scenario inputs). A",
    "differential below 1 for other pregnancies means smoking would have to reduce loss there.",
    "Full grid in S_selection_left_truncation_scenarios.csv."))
  check_people_first(rows, "Table S12")
  rows
}


# Exposure misreporting unrelated to GH/PE: the crude age-specific ratios after correcting for a
# fixed sensitivity and specificity of the recorded smoking fields.
table_s13 <- function() {
  nd <- read_result("bias", "S_exposure_misclassification_crossover.csv")
  # Specificity 0.98 is omitted: a 2% false-positive rate approaches the recorded prevalence, so
  # the corrected tables leave the admissible range at most ages.
  nd <- nd[nd$contrast %in% c("broad", "prepregnancy") &
             (same_level(nd$specificity, 0.995) | same_level(nd$specificity, 1)), ]

  header <- c("Comparison", "Sensitivity of recorded smoking", "Specificity",
              "Corrected crude RR, minimum over ages", "Corrected crude RR, maximum over ages",
              "Age at which recorded crude RR crosses 1",
              "Age at which corrected crude RR crosses 1")

  body <- t(vapply(seq_len(nrow(nd)), function(i) {
    r <- nd[i, ]
    c(bias_comparisons[[as.character(r$contrast)]],
      level_label(r$sensitivity), level_label(r$specificity),
      bias_num(r$min_corrected_rr, 3), bias_num(r$max_corrected_rr, 3),
      bias_num(r$recorded_crossover, 2), bias_num(r$corrected_crossover, 2))
  }, character(length(header))))

  rows <- bias_table(header, body, paste(
    "Misreporting unrelated to GH/PE, corrected on the crude age-specific two-by-two tables",
    "(unadjusted; illustrative). Scenarios with specificity 0.98 leave the admissible range at",
    "most ages because a 2% false-positive rate approaches the recorded smoking prevalence, and",
    "are omitted. Certificate sensitivity anchors: 70.6% to 82.0% (PRAMS Working Group 1998)."))
  check_people_first(rows, "Table S13")
  rows
}


# Exposure misreporting related to GH/PE: the reporting difference between women with and without
# GH/PE that would make the association null at each age.
table_s14 <- function() {
  tip <- read_result("bias", "S_exposure_misclassification_differential_tipping.csv")
  tip$key <- paste(tip$contrast, age_key(tip$age))

  header <- c("Age",
              paste0(bias_comparisons, ": crude RR"),
              paste0(bias_comparisons, ": required sensitivity ratio"))

  body <- t(vapply(as.character(15:45), function(a) {
    at_age <- lapply(bias_contrasts, function(ct) tip[tip$key == paste(ct, a), ])
    c(a,
      vapply(at_age, function(r) bias_num(r$crude_rr, 3), ""),
      vapply(at_age, function(r) bias_num(r$required_sensitivity_ratio_cases_to_noncases, 3), ""))
  }, character(length(header))))

  rows <- bias_table(header, body, paste(
    "Required sensitivity ratio: reporting sensitivity among women with GH/PE divided by that",
    "among women without, with no false-positive reports, that would make the true association",
    "exactly null at that age. Values below 1 mean under-reporting among women with GH/PE would",
    "be needed; above 1, over-reporting."))
  check_people_first(rows, "Table S14")
  rows
}


# Outcome recording sensitivity: the GH/PE recording sensitivity among women who continued
# smoking, relative to women who stopped, that would equalize the latent risks.
table_s15 <- function() {
  ot <- read_result("bias", "S_outcome_sensitivity_tipping_by_age.csv")
  anchors <- sort(unique(as.character(ot$anchor)), method = "radix")
  ot$key <- paste(ot$anchor, age_key(ot$age))

  header <- c("Age", paste0("Anchor ", anchors,
                            ": required sensitivity ratio (continued / stopped)"))

  body <- t(vapply(c("", as.character(15:45)), function(a) {
    c(if (nzchar(a)) a else "Overall",
      vapply(anchors, function(k) {
        r <- ot[ot$key == paste(k, a), ]
        # An anchor whose implied sensitivity leaves the unit interval has no tipping value.
        if (toupper(as.character(r$valid)) %in% c("TRUE", "1"))
          bias_num(r$required_A1_to_A0_sensitivity_ratio, 3) else "Not feasible"
      }, ""))
  }, character(length(header))))

  rows <- bias_table(header, body, paste(
    "Comparison 1. Each external validation sample fixes GH/PE recording sensitivity and",
    "specificity for women who stopped; the table gives the recording sensitivity among women who",
    "continued smoking, relative to that value, that would equalize the latent risks. Illustrative",
    "tipping conditions; no smoking-stratified validation exists."))
  check_people_first(rows, "Table S15")
  rows
}
