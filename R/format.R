# Number and label formatting shared by every table.
#
# The manuscript reports risk ratios to three decimals, risks and risk differences per 1,000
# births to one or two decimals, and counts with thousands separators. These helpers are the
# single place those conventions are defined, so a table cannot drift from the text.

fmt_n <- function(x) formatC(as.integer(as.numeric(x)), format = "d", big.mark = ",")

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x) || identical(x, "")) return("Not available")
  formatC(as.numeric(x), format = "f", digits = digits)
}

# An estimate with its confidence interval, from columns named <key>, <key>_lower, <key>_upper.
fmt_ci <- function(row, key = "rr", digits = 3, scale = 1) {
  paste0(fmt_num(as.numeric(row[[key]]) * scale, digits), " (",
         fmt_num(as.numeric(row[[paste0(key, "_lower")]]) * scale, digits), ", ",
         fmt_num(as.numeric(row[[paste0(key, "_upper")]]) * scale, digits), ")")
}

# The same, for files that name the columns estimate, lower and upper.
fmt_estimate_ci <- function(row, digits = 3, scale = 1) {
  paste0(fmt_num(as.numeric(row[["estimate"]]) * scale, digits), " (",
         fmt_num(as.numeric(row[["lower"]]) * scale, digits), ", ",
         fmt_num(as.numeric(row[["upper"]]) * scale, digits), ")")
}

# Overall risk, risk ratio or risk difference from a main_overall.csv row.
fmt_overall <- function(row, measure, digits = 3) {
  if (measure == "rr") return(fmt_ci(row, "rr", digits))
  paste0(fmt_num(row[[paste0(measure, "_per1000")]], digits), " (",
         fmt_num(row[[paste0(measure, "_lower_per1000")]], digits), ", ",
         fmt_num(row[[paste0(measure, "_upper_per1000")]], digits), ")")
}

# Consecutive ages collapse to a range, so "15,16,17,20" reads as "15-17; 20".
fmt_age_span <- function(values) {
  values <- sort(unique(as.integer(values)))
  if (!length(values)) return("Not established")
  groups <- split(values, cumsum(c(1, diff(values) != 1)))
  paste(vapply(groups, function(g)
    if (length(g) == 1) as.character(g[1]) else paste0(g[1], "–", g[length(g)]), ""),
    collapse = "; ")
}

# A fitted crossover age with its local confidence interval.
fmt_crossover <- function(row, digits = 2) {
  paste0(fmt_num(row[["age"]], digits), " (", fmt_num(row[["delta_lower"]], digits), ", ",
         fmt_num(row[["delta_upper"]], digits), ")")
}
