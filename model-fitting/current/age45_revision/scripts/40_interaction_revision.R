# Dated protocol: age45_revision/protocol/INTERACTION_AMENDMENT_2026-10-01.md
# Execute separately and serially, from 01_Analysis:
# Rscript --vanilla age45_revision/scripts/40_interaction_revision.R joint
# Rscript --vanilla age45_revision/scripts/40_interaction_revision.R prepregnancy
# No writes to released full fits or prior submission staging.
main <- function(population) {
  stopifnot(population %in% c("joint", "prepregnancy"))
  source("age45_revision/R/fit_helpers.R"); age45_initialize()
  suppressPackageStartupMessages(library(data.table))
  Sys.setenv(VECLIB_MAXIMUM_THREADS = "1", OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
  ROOT <- "age45_revision/outputs/supplementary/interaction_revision_20261001"
  OUT <- file.path(ROOT, population); dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
  SELF <- "age45_revision/scripts/40_interaction_revision.R"
  PROTOCOL <- "age45_revision/protocol/INTERACTION_AMENDMENT_2026-10-01.md"
  log_path <- file.path(OUT, "execution.log")
  started <- Sys.time()
  stage <- function(x) {
    line <- sprintf("[%s | %.2f min] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      as.numeric(difftime(Sys.time(), started, units = "mins")), x)
    message(line); cat(line, "\n", file = log_path, append = TRUE)
  }
  check <- function(ok, label) { if (!isTRUE(ok)) stop(label); invisible(TRUE) }
  hash_frame <- function(x) digest::digest(list(names = names(x), n = nrow(x),
    columns = vapply(x, function(col) digest::digest(col, algo = "sha256"), "")), algo = "sha256")
  FULL <- if (population == "joint") "age45_revision/outputs/supplementary/joint_3group" else "age45_revision/outputs/models/prepregnancy_main"
  receipt_path <- file.path(FULL, "receipt.json"); receipt <- jsonlite::fromJSON(receipt_path)
  for (i in seq_len(nrow(receipt$code))) check(age45_sha(receipt$code$path[i]) == receipt$code$sha256[i], paste("Frozen code hash mismatch", receipt$code$path[i]))
  model_path <- file.path(FULL, "model.rds")
  mi <- match(model_path, receipt$outputs$path); check(!is.na(mi), "Model absent from frozen receipt")
  check(age45_sha(model_path) == receipt$outputs$sha256[mi], "Frozen full model hash mismatch")
  full <- readRDS(model_path); check(full$converged, "Frozen full fit not converged")
  code <- rbind(age45_code_manifest(), data.frame(path = c(SELF, PROTOCOL), sha256 = vapply(c(SELF, PROTOCOL), age45_sha, "")))
  self_sha <- age45_sha(SELF)
  if (file.exists(file.path(OUT, "receipt.json"))) {
    old <- jsonlite::fromJSON(file.path(OUT, "receipt.json"))
    check(old$status == "complete_validated_interaction_revision" && old$self_sha256 == self_sha,
      "Existing result belongs to another code version; do not overwrite")
    for (i in seq_len(nrow(old$outputs))) check(age45_sha(old$outputs$path[i]) == old$outputs$sha256[i], "Existing result hash mismatch")
    stage("Verified completed interaction revision reused"); return(invisible(NULL))
  }
  stage(paste("Starting", population, "with verified frozen model and code"))
  age45_json(list(status = "running", population = population, started = as.character(started),
    self_sha256 = self_sha, protocol_sha256 = age45_sha(PROTOCOL)), file.path(OUT, "running.json"))
  years <- 2016:2024; parts <- ids <- missing <- counts <- inputs <- list()
  for (year in years) {
    stem <- if (population == "joint") file.path("age45_revision/derived/supplementary_inputs", paste0(year, "_prepregnancy_ext")) else
      file.path("age45_revision/derived/model_inputs", paste0(year, "_prepregnancy"))
    ip <- paste0(stem, ".rds"); rp <- paste0(stem, "_receipt.json"); rr <- jsonlite::fromJSON(rp)
    ih <- age45_sha(ip); check(ih == rr$output_sha256, paste("Annual input hash mismatch", year))
    inputs[[as.character(year)]] <- data.table(year = year, path = ip, sha256 = ih, receipt_path = rp, receipt_sha256 = age45_sha(rp))
    p <- readRDS(ip); spec <- p$spec
    check(nrow(p$data) == nrow(p$identity), "Annual data/identity length mismatch")
    if (population == "joint") {
      check(!anyNA(p$identity$pre_smoking) && all((p$data$A == 1) == p$identity$pre_smoking), "Exposure identity mismatch")
      pre <- p$identity$pre_smoking; t1 <- p$identity$t1_smoking
      group <- ifelse(!pre & t1 %in% FALSE, "NN", ifelse(pre & t1 %in% FALSE, "SN", ifelse(pre & t1 %in% TRUE, "SS", NA)))
      eligible <- !is.na(group)
    } else { group <- ifelse(p$data$A == 1, "S", "N"); eligible <- rep(TRUE, nrow(p$data)) }
    cc <- rep(TRUE, nrow(p$data))
    for (v in spec$covariates) {
      unknown <- is.na(p$data[[v]]); cc <- cc & !unknown
      missing[[length(missing) + 1L]] <- data.table(population = population, year = year, variable = v,
        eligible_n = sum(eligible), unknown_n = sum(eligible & unknown))
    }
    keep <- which(eligible & cc)
    counts[[as.character(year)]] <- data.table(population = population, year = year,
      prepregnancy_eligible_n = nrow(p$data), eligible_n = sum(eligible), excluded_for_covariates_n = sum(eligible & !cc),
      complete_case_n = length(keep), events = sum(p$data$Y[keep]))
    d <- p$data[keep, c("Y", "A", "age", spec$covariates), drop = FALSE]
    if (population == "joint") {
      d$A <- as.integer(group[keep] == "SS"); d$SN <- as.integer(group[keep] == "SN")
      ident <- p$identity[keep, c("source_type", "year", "source_row"), drop = FALSE]
    } else ident <- p$identity[keep, , drop = FALSE]
    parts[[as.character(year)]] <- d; ids[[as.character(year)]] <- ident
    stage(sprintf("%d loaded: eligible %d; complete cases %d", year, sum(eligible), length(keep)))
    rm(p, d, ident, group, eligible, cc, keep); gc(FALSE)
  }
  d <- as.data.frame(rbindlist(parts, use.names = TRUE)); ident <- as.data.frame(rbindlist(ids, use.names = TRUE)); rm(parts, ids); gc(FALSE)
  d$year_factor <- factor(as.character(d$year_factor), levels = years); spec$years <- as.integer(years)
  stage("Annual complete cases combined; checking unique source-record keys")
  check(!anyNA(d) && nrow(d) == full$nobs && !anyDuplicated(as.data.table(ident[c("source_type", "year", "source_row")])), "Cohort integrity failure")
  stage("Unique source-record keys verified; computing columnwise fingerprints")
  data_hash <- hash_frame(d); identity_hash <- hash_frame(ident)
  if (population == "joint") {
    manifest <- rbind(age45_code_manifest(), data.frame(path = c("age45_revision/scripts/20_prepare_supplementary_inputs.R", "age45_revision/scripts/33_fit_joint_three_group.R"),
      sha256 = unname(vapply(c("age45_revision/scripts/20_prepare_supplementary_inputs.R", "age45_revision/scripts/33_fit_joint_three_group.R"), age45_sha, ""))))
    formula <- as.formula(paste0(paste(deparse(spec$formula, width.cutoff = 500), collapse = " "), " + SN + SN:", .sep_spec_ns_term("age", spec$age_spec)), env = baseenv())
    signature <- digest::digest(list(data = data_hash, identity = identity_hash, formula = deparse(formula), age = spec$age_spec, code = manifest), algo = "sha256")
  } else {
    formula <- spec$formula
    # Released large-cohort fits were generated by 14, not the earlier generic driver.
    manifest <- rbind(age45_code_manifest(), data.frame(path = "age45_revision/scripts/14_large_cohort_refits.R",
      sha256 = age45_sha("age45_revision/scripts/14_large_cohort_refits.R")))
    check(identical(data_hash, receipt$data_column_fingerprint) && identical(identity_hash, receipt$identity_column_fingerprint),
      "Binary population columns differ from released large-cohort fingerprints")
    signature <- digest::digest(list(data_columns = data_hash, identity_columns = identity_hash, reference_columns = data_hash,
      formula = deparse(spec$formula), age = spec$age_spec, code = manifest), algo = "sha256")
  }
  check(identical(signature, receipt$input_signature), "Reconstructed cohort/formula signature does not match the frozen fit")
  rm(ident); gc(FALSE)
  stage(sprintf("Exact original input signature reproduced; N=%d, events=%d", nrow(d), sum(d$Y)))
  miss <- rbindlist(missing); count <- rbindlist(counts); rm(missing, counts)
  fwrite(miss, file.path(OUT, "missingness_by_year.csv")); fwrite(count, file.path(OUT, "population_by_year.csv"))
  ms <- miss[, .(eligible_n = sum(eligible_n), unknown_n = sum(unknown_n)), by = .(population, variable)]
  ms[, unknown_percent := 100 * unknown_n / eligible_n]
  fwrite(ms, file.path(OUT, "missingness_overall.csv"))
  fwrite(count[, lapply(.SD, sum), .SDcols = setdiff(names(count), c("population", "year"))], file.path(OUT, "population_overall.csv"))
  fwrite(rbindlist(inputs), file.path(OUT, "annual_input_manifest.csv"))
  group <- if (population == "joint") ifelse(d$A == 1, "SS", ifelse(d$SN == 1, "SN", "NN")) else ifelse(d$A == 1, "S", "N")
  dt <- as.data.table(d); dt[, group := group]
  fwrite(dt[, .(n = .N, events = sum(Y), non_events = sum(1 - Y), observed_risk_per1000 = 1000 * mean(Y)), by = group], file.path(OUT, "group_counts.csv"))
  desc <- lapply(c("age", "bmi"), function(v) dt[, .(variable = v, n = .N, mean = mean(get(v)), sd = sd(get(v)),
    median = median(get(v)), q1 = unname(quantile(get(v), .25)), q3 = unname(quantile(get(v), .75))), by = group])
  fwrite(rbindlist(desc), file.path(OUT, "descriptive_continuous.csv"))
  catdesc <- lapply(setdiff(spec$covariates, "bmi"), function(v) dt[, .(n = .N), by = .(group, level = as.character(get(v)))][, variable := v])
  cd <- rbindlist(catdesc, use.names = TRUE); cd[, percent := 100 * n / sum(n), by = .(group, variable)]
  fwrite(cd, file.path(OUT, "descriptive_categorical.csv")); rm(dt, group, desc, catdesc, cd); gc(FALSE)

  # Remove age interactions only; retain every main effect and nuisance term.
  labels <- attr(stats::terms(formula), "term.labels")
  is_interaction <- grepl("splines::ns(age,", labels, fixed = TRUE) &
    (grepl("^A:|:A$", labels) | grepl("^SN:|:SN$", labels))
  check(sum(is_interaction) == if (population == "joint") 2L else 1L, "Unexpected interaction term structure")
  reduced_formula <- stats::reformulate(labels[!is_interaction], response = "Y", env = baseenv())
  nms <- names(full$beta)
  a_int <- which(startsWith(nms, "A:splines::ns(age,"))
  sn_int <- which(grepl("^SN:splines::ns\\(age,.*\\)[0-9]+$|^splines::ns\\(age,.*\\)[0-9]+:SN$", nms))
  at <- c(a_int, sn_int); expected_df <- if (population == "joint") 8L else 4L
  check(length(a_int) == 4L && length(at) == expected_df, "Unexpected interaction degrees of freedom")
  wald <- function(beta, covariance, L) {
    b <- drop(L %*% beta); V <- L %*% covariance %*% t(L); V <- (V + t(V)) / 2
    drop(crossprod(b, solve(V, b)))
  }
  contrast_matrix <- function(indices, dimension) diag(dimension)[indices, , drop = FALSE]
  L <- contrast_matrix(at, length(full$beta))
  original_pass <- get("streaming_logistic_pass", envir = .GlobalEnv)
  assign("streaming_logistic_pass", function(...) {
    value <- original_pass(...)
    stage(sprintf("Completed unchanged solver pass; deviance=%.10f", value$deviance))
    value
  }, envir = .GlobalEnv)
  # Small, deterministic validation against the standard R implementation.
  subrows <- unique(as.integer(round(seq(1, nrow(d), length.out = min(120000L, nrow(d))))))
  small <- d[subrows, , drop = FALSE]
  stage(sprintf("Validating streaming and glm on %d deterministic records", nrow(small)))
  sf <- fit_streaming_logit(small, formula, "Y", "A", "age", spec$age_spec, "HC0", 10000L)
  sr <- fit_streaming_logit(small, reduced_formula, "Y", "A", "age", spec$age_spec, "HC0", 10000L)
  gf <- glm(formula, data = small, family = binomial(), control = glm.control(epsilon = 1e-12, maxit = 100), x = TRUE)
  gr <- glm(reduced_formula, data = small, family = binomial(), control = glm.control(epsilon = 1e-12, maxit = 100), x = TRUE)
  check(gf$converged && gr$converged && identical(names(coef(gf)), names(sf$beta)), "glm validation did not converge or design mismatch")
  bread <- solve(crossprod(gf$x, gf$x * (fitted(gf) * (1 - fitted(gf)))))
  hc0 <- bread %*% crossprod(gf$x, gf$x * (small$Y - fitted(gf))^2) %*% bread
  checks <- data.table(check = c("subset_full_coefficients", "subset_reduced_coefficients", "subset_full_deviance", "subset_reduced_deviance", "subset_LRT", "subset_HC0_covariance", "subset_HC0_Wald"),
    absolute_difference = c(max(abs(sf$beta - coef(gf))), max(abs(sr$beta - coef(gr))), abs(sf$deviance - deviance(gf)),
      abs(sr$deviance - deviance(gr)), abs((sr$deviance - sf$deviance) - (deviance(gr) - deviance(gf))),
      max(abs(sf$vcov_HC0 - hc0)), abs(wald(sf$beta, sf$vcov_HC0, L) - wald(coef(gf), hc0, L))),
    tolerance = c(1e-7, 1e-7, 1e-6, 1e-6, 1e-6, 1e-7, 1e-6))
  checks[, passed := absolute_difference <= tolerance]; fwrite(checks, file.path(OUT, "validation.csv"))
  check(all(checks$passed), "Deterministic glm validation failed")
  saveRDS(list(rows = subrows, full = sf, reduced = sr), file.path(OUT, "subset_validation_fits.rds"))
  rm(small, sf, sr, gf, gr, bread, hc0); gc(FALSE)
  stage("Subset validation passed; reevaluating frozen full likelihood, score, and both covariances on every record")
  full_pass <- streaming_logistic_pass(d, full$design_spec, full$beta, full$chunk_size, with_meat = TRUE)
  inv <- streaming_information_inverse(full_pass$H, 1e-12)$inverse
  V <- inv %*% full_pass$meat %*% inv; V <- (V + t(V)) / 2
  full_checks <- data.table(check = c("full_record_deviance", "full_record_score_vs_saved", "full_record_max_score_per_record", "full_record_model_covariance", "full_record_HC0_covariance"),
    absolute_difference = c(abs(full_pass$deviance - full$deviance), max(abs(full_pass$score - full$final_score)),
      max(abs(full_pass$score)) / nrow(d), max(abs(inv - full$vcov_model)), max(abs(V - full$vcov_HC0))),
    tolerance = c(1e-5, 1e-6, 1e-12, 1e-10, 1e-10))
  full_checks[, passed := absolute_difference <= tolerance]; checks <- rbind(checks, full_checks)
  fwrite(checks, file.path(OUT, "validation.csv")); check(all(checks$passed), "Full-record frozen fit verification failed")
  stage("Full-record verification passed; fitting the reduced main-effects model")
  reduced <- fit_streaming_logit(d, reduced_formula, "Y", "A", "age", spec$age_spec, "HC0", full$chunk_size)
  # Preserve a converged fit before downstream validation/reporting; it is not a completed release.
  saveRDS(reduced, file.path(OUT, "reduced_model.rds"))
  age45_json(list(status = "converged_pending_final_validation", input_signature = signature,
    self_sha256 = self_sha, model_sha256 = age45_sha(file.path(OUT, "reduced_model.rds"))),
    file.path(OUT, "reduced_checkpoint.json"))
  check(reduced$converged && max(abs(reduced$final_score)) / nrow(d) < 1e-12, "Reduced fit convergence failure")
  check(identical(names(reduced$beta), nms[-at]) && full$rank - reduced$rank == expected_df, "Reduced model not the intended nested design")
  # Direct design comparison on spread-out rows excludes term-reordering or basis drift.
  full_X <- streaming_design(d, subrows[seq_len(min(10000L, length(subrows)))], full$design_spec)$X
  reduced_X <- streaming_design(d, subrows[seq_len(min(10000L, length(subrows)))], reduced$design_spec)$X
  check(identical(colnames(full_X)[-at], colnames(reduced_X)), "Nested design column names/order changed")
  # Subsetting model.matrix drops incidental assign/contrast attributes, whereas rebuilding keeps them.
  # Their term indexes need not match; the ordered names and every numeric entry must match exactly.
  check(identical(as.vector(full_X[, -at, drop = FALSE]), as.vector(reduced_X)), "Nested numeric design columns changed")
  lr <- 2 * (full$log_likelihood - reduced$log_likelihood); check(lr >= 0 && is.finite(lr), "Invalid likelihood ratio statistic")
  result_row <- function(label, type, statistic, df) data.table(population = population, hypothesis = label, test = type,
    n = nrow(d), events = sum(d$Y), full_parameters = full$rank, reduced_parameters = reduced$rank,
    statistic = statistic, df = df, p_value = pchisq(statistic, df, lower.tail = FALSE),
    log_p_value = pchisq(statistic, df, lower.tail = FALSE, log.p = TRUE),
    p_display = if (pchisq(statistic, df, lower.tail = FALSE) < .001) "<0.001" else sprintf("%.3f", pchisq(statistic, df, lower.tail = FALSE)),
    p_numeric_underflow = pchisq(statistic, df, lower.tail = FALSE) == 0,
    full_log_likelihood = full$log_likelihood, reduced_log_likelihood = reduced$log_likelihood)
  tests <- rbind(result_row("All smoking-by-age spline interactions equal zero", "Likelihood ratio (model based)", lr, expected_df),
    result_row("All smoking-by-age spline interactions equal zero", "Omnibus Wald (HC0)", wald(full$beta, full$vcov_HC0, L), expected_df))
  if (population == "joint") {
    LA <- contrast_matrix(a_int, length(full$beta)); LS <- contrast_matrix(sn_int, length(full$beta))
    for (nm in c("SN versus SS", "NN versus SS", "SN versus NN")) {
      C <- switch(nm, "SN versus SS" = LS - LA, "NN versus SS" = -LA, "SN versus NN" = LS)
      tests <- rbind(tests, result_row(paste(nm, "age-interaction coefficients equal zero"), "Contrast Wald (HC0; exploratory)", wald(full$beta, full$vcov_HC0, C), 4L))
    }
  }
  fwrite(tests, file.path(OUT, "interaction_tests.csv")); saveRDS(reduced, file.path(OUT, "reduced_model.rds"))
  fwrite(reduced$trace, file.path(OUT, "reduced_fit_trace.csv"))
  writeLines(c("Full:", paste(deparse(formula, width.cutoff = 500), collapse = " "), "Reduced:",
    paste(deparse(reduced_formula, width.cutoff = 500), collapse = " "), "Removed coefficient columns:", nms[at]), file.path(OUT, "model_specifications.txt"))
  writeLines(c(paste("Rscript --vanilla", SELF, population), "Working directory: 01_Analysis", "No full model refitted; only the nested reduced model was newly fitted."), file.path(OUT, "run_command.txt"))
  writeLines(capture.output(sessionInfo()), file.path(OUT, "sessionInfo.txt"))
  age45_json(list(status = "converged_and_validated", input_signature = signature,
    self_sha256 = self_sha, model_sha256 = age45_sha(file.path(OUT, "reduced_model.rds"))),
    file.path(OUT, "reduced_checkpoint.json"))
  check(age45_sha(SELF) == self_sha && age45_sha(model_path) == receipt$outputs$sha256[mi], "Code or frozen full model changed during run")
  stage(sprintf("COMPLETE: LRT chi-square %.8f, df %d, P %s; HC0 omnibus %.8f", lr, expected_df, tests$p_display[1], tests$statistic[2]))
  files <- list.files(OUT, full.names = TRUE); files <- files[!basename(files) %in% c("receipt.json", "running.json", "execution.log")]
  age45_json(list(status = "complete_validated_interaction_revision", population = population, started = as.character(started),
    finished = as.character(Sys.time()), elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    self_sha256 = self_sha, protocol_sha256 = age45_sha(PROTOCOL), no_imputation = TRUE,
    full_model_path = model_path, full_model_sha256 = receipt$outputs$sha256[mi], full_receipt_sha256 = age45_sha(receipt_path),
    reconstructed_original_input_signature = signature, data_sha256 = data_hash, identity_sha256 = identity_hash,
    eligible_n = sum(count$eligible_n), fit_n = nrow(d), events = sum(d$Y),
    full_parameters = full$rank, reduced_parameters = reduced$rank, interaction_df = expected_df,
    full_fit_reused = TRUE, full_record_likelihood_score_covariance_verified = TRUE, validation_passed = all(checks$passed),
    code = code, inputs = rbindlist(inputs),
    outputs = data.frame(path = files, sha256 = vapply(files, age45_sha, ""))), file.path(OUT, "receipt.json"))
  invisible(tests)
}
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) stop("Supply exactly one population: joint or prepregnancy")
  main(args[[1]])
}
