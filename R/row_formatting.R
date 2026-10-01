# Helpers used by the current October publication tables.

sens_header <- c(
  "Comparison",
  "Records / events",
  "Exposed records",
  "Standardized risk per 1,000, reference / exposed",
  "RR (95% CI)",
  "RD per 1,000 (95% CI)",
  "Crossover age (local 95% CI)",
  "Ages with lower | higher recorded risk supported (simultaneous 95% band)")

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

sens_region <- function(x) {
  x <- as.character(x)
  x <- if (length(x) == 1 && !is.na(x)) trimws(x) else ""
  if (x %in% c("", "Empty")) return("none")
  gsub(" U ", " and ", x, fixed = TRUE)
}

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
