# Table 1 by recorded outcome (protocol addendum G, 19 September 2026).
#
# Characteristics of the S vs N complete-case population (the broadest main population) by recorded
# GH/PE: births with the outcome, births without, and the total. Rows: exposure pattern coded by the
# before-pregnancy and first-trimester cigarette fields (SS, SN, NN, other), before-pregnancy smoking
# (S, N), maternal age, every model covariate, birth year.
#
# Descriptive only. No imputation (complete cases on the model covariates, exactly as the fits), no
# model, no estimate, no test statistic. Reads the receipted supplementary inputs and refuses any file
# whose hash does not match its receipt. Writes only to age45_revision/outputs/descriptive.
#
# From 01_Analysis: Rscript --vanilla age45_revision/scripts/32_table1_by_outcome.R
main <- function() {
  source("age45_revision/R/fit_helpers.R"); age45_initialize()
  suppressPackageStartupMessages(library(data.table))
  INP <- "age45_revision/derived/supplementary_inputs"
  out <- "age45_revision/outputs/descriptive"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
  AVOID <- c("smokers", "nonsmokers", "non-smokers", "quitters", "abstinence", "abstain")
  cells <- list(); crude <- list(); inputs <- list()
  for (y in 2016:2024) {
    stem <- file.path(INP, paste0(y, "_prepregnancy_ext"))
    r <- jsonlite::fromJSON(paste0(stem, "_receipt.json"))
    sha <- age45_sha(paste0(stem, ".rds"))
    stopifnot(r$status == "complete_prepared_supplementary", sha == r$output_sha256)
    inputs[[as.character(y)]] <- sha
    p <- readRDS(paste0(stem, ".rds"))
    stopifnot(nrow(p$data) == nrow(p$identity), !anyNA(p$identity$pre_smoking),
      all((p$data$A == 1) == p$identity$pre_smoking), all(p$data$age %in% 15:45))
    cc <- complete.cases(p$data[p$spec$covariates])
    d <- as.data.table(p$data[cc, , drop = FALSE])
    pre <- p$identity$pre_smoking[cc]; t1 <- p$identity$t1_smoking[cc]
    rm(p); gc(FALSE)
    # Exposure pattern: first letter before pregnancy, second first trimester. "other" is the small
    # set the three-group analysis excludes: no smoking before pregnancy but smoking in T1, or T1 unknown.
    d[, pattern := factor(fifelse(pre & t1 %in% TRUE, "SS",
                          fifelse(pre & t1 %in% FALSE, "SN",
                          fifelse(!pre & t1 %in% FALSE, "NN", "other"))),
                          levels = c("SS", "SN", "NN", "other"))]
    d[, pre_smoking := factor(fifelse(pre, "S", "N"), levels = c("S", "N"))]
    d[, age_group := cut(age, c(14, 19, 24, 29, 34, 39, 45),
                         labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-45"))]
    stopifnot(!anyNA(d$age_group), !anyNA(d$pattern))
    cat_vars <- c("pattern", "pre_smoking", "age_group", "race_ethnicity", "education4", "prepreg_diabetes",
                  "prior_living4", "prior_preterm", "prior_cesarean", "nativity", "year_factor")
    num_vars <- c("age", "bmi")
    for (v in cat_vars) {
      z <- d[, .N, by = .(Y, level = as.character(get(v)))]
      z[, `:=`(variable = v, kind = "categorical", sum = 0, sum_sq = 0)]
      setnames(z, "N", "n"); cells[[length(cells) + 1L]] <- z
    }
    for (v in num_vars) {
      z <- d[, .(n = .N, sum = sum(get(v)), sum_sq = sum(get(v)^2)), by = Y]
      z[, `:=`(variable = v, level = "", kind = "continuous")]; cells[[length(cells) + 1L]] <- z
    }
    # Crude recorded GH/PE per 1,000 within each pattern, for the text.
    crude[[as.character(y)]] <- d[, .(n = .N, events = sum(Y)), by = .(pattern)]
    message("Table 1 by outcome: ", y, " complete cases ", nrow(d)); rm(d); gc(FALSE)
  }
  z <- rbindlist(cells, use.names = TRUE)[, lapply(.SD, sum), by = .(Y, variable, level, kind), .SDcols = c("n", "sum", "sum_sq")]
  denom <- z[variable == "age", .(Y, denominator = n)]
  z <- merge(z, denom, by = "Y", sort = FALSE)
  z[, c("mean", "sd", "pct") := list(NA_real_, NA_real_, NA_real_)]
  z[kind == "continuous", `:=`(mean = sum / n, sd = sqrt((sum_sq - sum^2 / n) / (n - 1)))]
  z[kind == "categorical", pct := 100 * n / denominator]
  # Every categorical variable must partition each outcome column exactly.
  chk <- z[kind == "categorical", .(s = sum(n), d = unique(denominator)), by = .(Y, variable)]
  stopifnot(all(chk$s == chk$d))
  # Totals column: the two outcome columns summed.
  tot <- z[, .(n = sum(n), sum = sum(sum), sum_sq = sum(sum_sq)), by = .(variable, level, kind)]
  tot[, Y := 2L]; tot[, denominator := sum(denom$denominator)]
  tot[, c("mean", "sd", "pct") := list(NA_real_, NA_real_, NA_real_)]
  tot[kind == "continuous", `:=`(mean = sum / n, sd = sqrt((sum_sq - sum^2 / n) / (n - 1)))]
  tot[kind == "categorical", pct := 100 * n / denominator]
  z <- rbindlist(list(z, tot), use.names = TRUE)
  z[, column := factor(Y, levels = c(1L, 0L, 2L), labels = c("gh_pe", "no_gh_pe", "total"))]
  z[, display := ifelse(kind == "continuous", sprintf("%.1f (%.1f)", mean, sd),
                        paste0(format(n, big.mark = ",", trim = TRUE), " (", sprintf("%.1f", pct), "%)"))]
  setorder(z, column, variable, level)
  fwrite(z, file.path(out, "table1_by_outcome_long.csv"))
  cr <- rbindlist(crude)[, .(n = sum(n), events = sum(events)), by = pattern][, risk_per1000 := 1000 * events / n][]
  fwrite(cr, file.path(out, "crude_gh_pe_by_pattern.csv"))
  # People-first guard on every label that will reach the manuscript.
  labels <- unique(c(z$variable, z$level))
  bad <- labels[Reduce(`|`, lapply(AVOID, function(a) grepl(a, labels, ignore.case = TRUE)))]
  if (length(bad)) stop("people-first guard: ", paste(bad, collapse = ", "))
  n_by <- setNames(denom$denominator, denom$Y)
  age45_json(list(status = "complete_table1_by_outcome", protocol = "addendum G, 19 September 2026",
    population = "S vs N complete cases (prepregnancy_ext, complete on model covariates)",
    no_imputation = TRUE, no_model = TRUE, n_total = sum(denom$denominator),
    n_gh_pe = unname(n_by["1"]), n_no_gh_pe = unname(n_by["0"]),
    input_sha256 = inputs, script_sha256 = age45_sha("age45_revision/scripts/32_table1_by_outcome.R")),
    file.path(out, "table1_by_outcome_receipt.json"))
  message("Total complete cases ", sum(denom$denominator), "; with GH/PE ", n_by["1"], "; without ", n_by["0"])
  invisible(z)
}
if (sys.nframe() == 0L) main()
