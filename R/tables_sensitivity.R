# Supplementary tables S16 to S19, S21 and S22. S16 to S19 report the supplementary fits;
# S21 and S22 report the populations and the unknown-value counts behind them.

# The eight columns shared by every fit-based supplementary table. One header for all of them
# keeps the four tables on one reading scale, which is why S16 can merge two sets of fits.
sens_header <- c(
  "Comparison",
  "Records / events",
  "Exposed records",
  "Standardized risk per 1,000, reference / exposed",
  "RR (95% CI)",
  "RD per 1,000 (95% CI)",
  "Crossover age (local 95% CI)",
  "Ages with lower | higher recorded risk supported (simultaneous 95% band)")

# A fit either has a regular interior root, reported with its local interval, or it has none, in
# which case the ages at which the fitted contrast was null are reported instead.
sens_crossover <- function(row) {
  age <- row[["crossover_age"]]
  if (length(age) == 1 && !is.na(age) && nzchar(as.character(age))) {
    return(paste0(fmt_num(age, 1), " (", fmt_num(row[["delta_lower"]], 1), ", ",
                  fmt_num(row[["delta_upper"]], 1), ")"))
  }
  null_ages <- as.character(row[["fitted_null_ages"]])
  if (length(null_ages) != 1 || is.na(null_ages) || !nzchar(null_ages)) return("none")
  null_ages
}

# The fitting code writes an empty sign region as "Empty" and joins disjoint intervals with " U ".
# A reader sees "none" and the word "and" instead.
sens_region <- function(x) {
  x <- as.character(x)
  x <- if (length(x) == 1 && !is.na(x)) trimws(x) else ""
  if (x %in% c("", "Empty")) return("none")
  gsub(" U ", " and ", x, fixed = TRUE)
}

# One table row for one model. A model that was not fitted is shown as such rather than dropped,
# so a reader can see that the row was planned.
sens_row <- function(fits, model_id, label) {
  i <- match(model_id, fits$model_id)
  if (is.na(i)) return(c(label, "not run", rep("", length(sens_header) - 2)))
  row <- fits[i, , drop = FALSE]
  c(label,
    paste0(fmt_n(row[["fit_n"]]), " / ", fmt_n(row[["events"]])),
    fmt_n(row[["A1"]]),
    paste0(fmt_num(row[["risk0_per1000"]], 1), " / ", fmt_num(row[["risk1_per1000"]], 1)),
    fmt_overall(row, "rr", 3),
    fmt_overall(row, "rd", 2),
    sens_crossover(row),
    paste0(sens_region(row[["simultaneous_strict_negative"]]), " | ",
           sens_region(row[["simultaneous_strict_positive"]])))
}

# Header, body and the footnote row the manuscript expects as the final row of every table.
sens_assemble <- function(header, rows, footnote) {
  body <- do.call(rbind, rows)
  m <- rbind(header, body, c(footnote, rep("", length(header) - 1)))
  matrix(relabel(m), nrow = nrow(m), dimnames = NULL)
}

sens_fits <- function() read_result("sensitivity", "supp_summary.csv")

# Labels of the three comparisons, used wherever a supplementary row names one of them.
sens_comparison <- c(
  primary = "Comparison 1: continued vs stopped in T1",
  broad = "Comparison 2: both periods vs neither",
  prepregnancy = "Comparison 3: any prepregnancy smoking vs none")


# --------------------------------------------------------------------------- fit-based tables

# Three-level early smoking, then the stricter and clean-reference definitions. The two sets share
# the header, so they read as one table; the footnote explains that the first two rows and the last
# three rest on different reference groups.
table_s16 <- function() {
  fits <- sens_fits()
  rows <- list(
    sens_row(fits, "supp_3lvl_stopped_vs_none", "Stopped by T1 vs no early smoking"),
    sens_row(fits, "supp_3lvl_continued_vs_none", "Continued in T1 vs no early smoking"),
    sens_row(fits, "supp_strict_continued_vs_stopped",
             "Continued vs stopped with no later positive report"),
    sens_row(fits, "supp_clean_prepregnancy",
             "Comparison 3 with reference zero in every recorded window"),
    sens_row(fits, "supp_clean_broad",
             "Comparison 2 with reference zero in every recorded window"))
  out <- sens_assemble(sens_header, rows, paste0(
    "The first two rows are standardized to the covariate distribution of all three ",
    "early-smoking groups together at each age, so they share one absolute scale. In the last ",
    "three rows the later-trimester fields only narrow the reference group, and a missing later ",
    "field counts as not positive, so no group requires the pregnancy to reach a given week."))
  check_people_first(out, "Table_S16")
  out
}

# Dose response, in the first trimester and before pregnancy.
table_s17 <- function() {
  fits <- sens_fits()
  dose <- c("1_5" = "1-5", "6_10" = "6-10", "11_20" = "11-20", "21plus" = "21 or more")
  rows <- c(
    lapply(names(dose), function(k)
      sens_row(fits, paste0("supp_t1dose_", k),
               paste0("T1 ", dose[[k]],
                      " cigarettes/day vs 0, women who smoked before pregnancy"))),
    list(sens_row(fits, "supp_t1change_reduced", "Reduced dose in T1 vs stopped"),
         sens_row(fits, "supp_t1change_same_or_more", "Same or higher dose in T1 vs stopped")),
    lapply(names(dose), function(k)
      sens_row(fits, paste0("supp_pdose_", k),
               paste0("Prepregnancy ", dose[[k]], " cigarettes/day vs 0"))))
  out <- sens_assemble(sens_header, rows, paste0(
    "T1 dose rows adjust for prepregnancy dose and share one reference (all women who smoked ",
    "before pregnancy); prepregnancy dose rows share the prepregnancy population reference."))
  check_people_first(out, "Table_S17")
  out
}

# Definitions that condition on information recorded at or after disease onset. These rows are
# printed to show what such a definition does to an estimate, not as estimates of an association.
table_s18 <- function() {
  fits <- sens_fits()
  rows <- list(
    sens_row(fits, "supp_t3_through_vs_none",
             "Positive in all four windows vs zero in all four (requires a third-trimester value)"),
    sens_row(fits, "supp_timing_stopped_by_T1", "Stopped by T1 vs smoked throughout"),
    sens_row(fits, "supp_timing_stopped_in_T2", "Stopped in T2 vs smoked throughout"),
    sens_row(fits, "supp_timing_stopped_in_T3", "Stopped in T3 vs smoked throughout"),
    sens_row(fits, "supp_strict_relapse_vs_stopped", paste0(
      "T1 zero then a later positive report vs strict stopped ",
      "(exposed group defined by a later-trimester report)")))
  out <- sens_assemble(sens_header, rows, paste0(
    "Bias illustration, not association estimates. Every row conditions on information recorded ",
    "at or after GH/PE onset. The all-four-windows row requires an observed third-trimester ",
    "value. In the three stopping-time rows the comparator \"smoked throughout\" is a positive ",
    "report in all of the first, second and third trimesters, so the comparator itself is ",
    "unobtainable for a birth before 28 weeks: the reference arm, not only the exposed arms, is ",
    "conditioned on reaching the third trimester, which raises all three risk ratios ",
    "independently of any effect of smoking or of when it stopped. In the last row the exposed ",
    "group is defined by a positive later-trimester report, so a missing later field places a ",
    "woman in the reference arm and the reference is enriched with early deliveries, which carry ",
    "early-onset disease."))
  check_people_first(out, "Table_S18")
  out
}

# Preterm birth as a control outcome, then the same comparisons within race and ethnicity strata.
table_s19 <- function() {
  fits <- sens_fits()
  strata <- c(nhw = "non-Hispanic White", nhb = "non-Hispanic Black",
              hisp = "Hispanic", other = "other groups combined")
  rows <- c(
    lapply(names(sens_comparison), function(cc)
      sens_row(fits, paste0("supp_preterm_", cc),
               paste0("Control outcome preterm birth, ", sens_comparison[[cc]]))),
    unlist(lapply(c("primary", "broad"), function(cc)
      lapply(names(strata), function(k)
        sens_row(fits, paste0("supp_race_", cc, "_", k),
                 paste0(sens_comparison[[cc]], ", ", strata[[k]])))),
      recursive = FALSE))
  out <- sens_assemble(sens_header, rows, paste0(
    "Preterm birth: obstetric estimate below 37 weeks, on the same complete-case records as the ",
    "GH/PE models. Race and ethnicity strata omit the race covariate."))
  check_people_first(out, "Table_S19")
  out
}


# --------------------------------------------------------------------- descriptive reporting

# Field names as the manuscript writes them, and the order Table 1 uses, so Table 1, S21 and S22
# can be read side by side.
sens_field_label <- c(
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

sens_field_order <- names(sens_field_label)

# Characteristics of the populations of comparisons 2 and 3. Table 1 shows only comparison 1, so
# the two reference groups of about 28 million births were computed but never displayed.
table_s21 <- function() {
  base <- read_result("descriptive", "baseline_all_three_contrasts.csv")
  arm_label <- c(
    "broad.0" = "Comparison 2: no smoking in either period",
    "broad.1" = "Comparison 2: smoked before pregnancy and in T1",
    "prepregnancy.0" = "Comparison 3: no smoking before pregnancy",
    "prepregnancy.1" = "Comparison 3: smoked before pregnancy")
  columns <- names(arm_label)

  # Maternal age is measured on every record, so its denominator is the size of the arm.
  age_rows <- base[base$variable == "age", , drop = FALSE]
  denom <- setNames(age_rows$denominator, paste(age_rows$contrast, age_rows$A, sep = "."))

  shown <- base[base$variable %in% sens_field_order, , drop = FALSE]
  key <- paste(shown$variable, shown$level, sep = "\r")
  first <- key[!duplicated(key)]
  variable <- sub("\r.*$", "", first)
  level <- sub("^[^\r]*\r", "", first)
  ord <- order(match(variable, sens_field_order), seq_along(first))
  variable <- variable[ord]
  level <- level[ord]

  display <- setNames(as.character(shown$display),
                      paste(shown$contrast, shown$A, shown$variable, shown$level, sep = "\r"))

  header <- c("Characteristic",
              vapply(columns, function(cl)
                paste0(arm_label[[cl]], "\nN=", fmt_n(denom[[cl]])), ""))
  rows <- lapply(seq_along(variable), function(i) {
    label <- sens_field_label[[variable[i]]]
    if (nzchar(level[i])) label <- paste0(label, ": ", level[i])
    cells <- vapply(columns, function(cl) {
      cl <- sub(".", "\r", cl, fixed = TRUE)
      value <- display[paste(cl, variable[i], level[i], sep = "\r")]
      if (is.na(value)) "Not applicable" else unname(value)
    }, "")
    c(label, cells)
  })
  out <- sens_assemble(header, rows, paste0(
    "Values are mean (SD) or n (%), from the same complete-case records as the estimates for ",
    "each comparison; column denominators apply to every percentage in that column. The exposed ",
    "group of comparison 2 is the group of women who continued smoking in T1 in Table 1. ",
    "Populations overlap and must not be added. T1: first trimester; BMI: body mass index; ",
    "SD: standard deviation."))
  check_people_first(out, "Table_S21")
  out
}

# Records with an unknown value in each covariate field, by comparison. Table S2 gives only the
# joint complete-case exclusion, which is smaller because these counts overlap.
table_s22 <- function() {
  miss <- read_result("descriptive", "missingness_by_variable_year.csv")
  order_of <- c(primary = "Comparison 1", broad = "Comparison 2", prepregnancy = "Comparison 3")

  # Summing every reason over 2016 to 2024 reproduces the eligible count of the comparison, which
  # is the denominator each percentage belongs to. A field with nothing recorded as unknown has no
  # row at all, so an absent key means zero rather than a missing value.
  eligible <- tapply(miss$n, paste(miss$contrast, miss$field, sep = "\r"), sum)
  unknown_rows <- miss[miss$reason == "item_unknown", , drop = FALSE]
  unknown <- tapply(unknown_rows$n,
                    paste(unknown_rows$contrast, unknown_rows$field, sep = "\r"), sum)

  total <- vapply(names(order_of), function(cc) {
    sizes <- unique(eligible[startsWith(names(eligible), paste0(cc, "\r"))])
    if (length(sizes) != 1)
      stop("the fields of ", cc, " do not share one eligible count")
    sizes
  }, numeric(1))

  fields <- sort(unique(miss$field))
  fields <- fields[order(match(fields, sens_field_order), seq_along(fields))]

  header <- c("Covariate field",
              vapply(names(order_of), function(cc)
                paste0(order_of[[cc]], ": records with an unknown value (% of ",
                       fmt_n(total[[cc]]), " eligible)"), ""))
  rows <- lapply(fields, function(f) {
    cells <- vapply(names(order_of), function(cc) {
      d <- eligible[paste(cc, f, sep = "\r")]
      if (is.na(d)) return("Not used in this comparison")
      n <- unknown[paste(cc, f, sep = "\r")]
      if (is.na(n)) n <- 0
      paste0(fmt_n(n), " (", fmt_num(100 * n / d, 2), "%)")
    }, "")
    c(sens_field_label[[f]], cells)
  })
  out <- sens_assemble(header, rows, paste0(
    "Counts are records whose field was recorded as unknown, among the records that met the ",
    "clinical eligibility of each comparison, summed over 2016 to 2024. A record is excluded ",
    "from a model if any field that model adjusts for is unknown, so these counts overlap and ",
    "are larger than the joint complete-case exclusion in Table S2. Prepregnancy cigarettes per ",
    "day is a covariate of comparison 1 only. Maternal age and the smoking and outcome fields ",
    "are eligibility requirements, so they are known for every eligible record and are not ",
    "listed."))
  check_people_first(out, "Table_S22")
  out
}
