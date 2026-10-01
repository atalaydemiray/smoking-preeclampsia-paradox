# Diagnostic fit (addendum I, 19 September 2026): SS vs SN on the released within-group population with the
# nine natality-main covariates and NO prepregnancy dose. Separates the two differences between the joint
# model's SS vs SN and the released primary_main (dose adjustment; covariate function shared with NN).
# NOT a reported result. Writes joint_3group/diagnostics/within_s_no_dose/.
# From 01_Analysis: Rscript --vanilla age45_revision/scripts/36_diagnostic_within_s_no_dose.R
main <- function() {
  source("age45_revision/R/fit_helpers.R"); age45_initialize()
  suppressPackageStartupMessages(library(data.table))
  INP <- "age45_revision/derived/supplementary_inputs"; years <- 2016:2024; ages <- 15:45
  JOINT <- "age45_revision/outputs/supplementary/joint_3group"
  OUT <- file.path(JOINT, "diagnostics/within_s_no_dose"); dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
  t0 <- Sys.time(); stage <- function(msg) message(sprintf("[%5.1f min] %s", as.numeric(difftime(Sys.time(), t0, units = "mins")), msg))
  parts <- list(); spec <- NULL
  for (y in years) {
    stem <- file.path(INP, paste0(y, "_prepregnancy_ext")); rr <- jsonlite::fromJSON(paste0(stem, "_receipt.json"))
    stopifnot(rr$status == "complete_prepared_supplementary", age45_sha(paste0(stem, ".rds")) == rr$output_sha256)
    p <- readRDS(paste0(stem, ".rds")); spec <- p$spec
    cc <- rep(TRUE, nrow(p$data)); for (v in spec$covariates) cc <- cc & !is.na(p$data[[v]])
    pre <- p$identity$pre_smoking; t1 <- p$identity$t1_smoking
    keep <- which(cc & pre & t1 %in% c(TRUE, FALSE))              # SS and SN records only
    d <- p$data[keep, c("Y", "A", "age", spec$covariates), drop = FALSE]
    d$A <- as.integer(t1[keep])                                     # A = 1 for SS, 0 for SN
    parts[[as.character(y)]] <- d; rm(p, d, cc, pre, t1, keep); gc(FALSE)
  }
  d <- as.data.frame(rbindlist(parts, use.names = TRUE)); rm(parts); gc(FALSE)
  d$year_factor <- factor(as.character(d$year_factor), levels = years)
  stopifnot(!anyNA(d), nrow(d) == 1980559L, sum(d$A) == 1480543L, sum(d$A == 0L) == 500016L)   # = primary_main and the joint SS/SN
  stopifnot(!"prepreg_dose5" %in% spec$covariates)
  stage(sprintf("within-group population rebuilt: N=%d (SS %d, SN %d); nine covariates, no dose", nrow(d), sum(d$A), sum(d$A == 0L)))
  fit <- fit_streaming_logit(d, spec$formula, "Y", "A", "age", spec$age_spec, "HC0", 25000L)
  stopifnot(fit$converged, max(abs(fit$final_score)) / nrow(d) < 1e-12)
  saveRDS(fit, file.path(OUT, "model.rds")); stage(sprintf("converged in %d iterations", fit$iterations))
  age45_save_standardization(fit, d, OUT, root_allowed = TRUE); stage("standardized to the within-group population; crossover written")
  # ---- decomposition ---------------------------------------------------------------------------------
  main_root <- function(dir) { z <- readRDS(file.path(dir, "HC0_crossover.rds")); r <- as.data.table(z$roots)[kind == "crossing" & age >= 20 & age <= 40]
    if (nrow(r) > 1) r <- r[which.max(regular_delta)]; list(age = r$age, lo = r$delta_lower, hi = r$delta_upper, rev = isTRUE(z$simultaneous$both_signs_demonstrated)) }
  ov <- function(dir) { o <- fread(file.path(dir, "HC0_overall_standardized.csv")); list(rr = o[measure == "rr", estimate], rd = o[measure == "rd", estimate_per1000]) }
  rows <- rbindlist(lapply(list(
    list(id = "primary_main", label = "released within-group fit: S covariate function, prepregnancy dose adjusted", dir = "age45_revision/outputs/models/primary_main"),
    list(id = "within_s_no_dose", label = "diagnostic: S covariate function, no dose", dir = OUT),
    list(id = "joint_ss_vs_sn", label = "joint three-group model: pooled covariate function, no dose", dir = file.path(JOINT, "ss_vs_sn"))),
    function(x) { r <- main_root(x$dir); o <- ov(x$dir)
      data.table(model = x$id, description = x$label, crossover = r$age, ci_lower = r$lo, ci_upper = r$hi, simultaneous_reversal = r$rev, overall_rr = o$rr, overall_rd_per1000 = o$rd) }))
  rows[, crossover_minus_primary_main := crossover - crossover[model == "primary_main"]]
  dose_effect <- rows[model == "within_s_no_dose", crossover] - rows[model == "primary_main", crossover]
  shared_effect <- rows[model == "joint_ss_vs_sn", crossover] - rows[model == "within_s_no_dose", crossover]
  fwrite(rows, file.path(OUT, "decomposition.csv")); print(rows)
  stage(sprintf("DECOMPOSITION of the %.2f-year movement: dropping the dose adjustment %+.2f years; sharing the covariate function with NN %+.2f years",
    rows[model == "joint_ss_vs_sn", crossover] - rows[model == "primary_main", crossover], dose_effect, shared_effect))
  files <- list.files(OUT, full.names = TRUE); files <- files[basename(files) != "receipt.json"]
  age45_json(list(status = "complete_diagnostic_within_s_no_dose", protocol = "addendum I, 19 September 2026; diagnostic only, not a reported result",
    fit_n = nrow(d), A1 = sum(d$A), A0 = sum(d$A == 0L), events = sum(d$Y), formula = paste(deparse(spec$formula, width.cutoff = 500), collapse = " "),
    covariates = spec$covariates, iterations = fit$iterations, dose_effect_years = dose_effect, shared_covariate_function_effect_years = shared_effect,
    code = rbind(age45_code_manifest(), data.frame(path = "age45_revision/scripts/36_diagnostic_within_s_no_dose.R", sha256 = age45_sha("age45_revision/scripts/36_diagnostic_within_s_no_dose.R"))),
    elapsed_seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")), outputs = data.frame(path = files, sha256 = vapply(files, age45_sha, ""))), file.path(OUT, "receipt.json"))
  stage("complete"); invisible(NULL)
}
if (sys.nframe() == 0L) main()
