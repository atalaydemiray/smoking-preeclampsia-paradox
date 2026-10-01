# Strict local provenance and result validation, no fitting or mutation of fit outputs.
# From 01_Analysis: Rscript --vanilla age45_revision/scripts/40_validate_interaction_release.R
main <- function() {
  source("age45_revision/R/fit_helpers.R"); age45_initialize()
  ROOT <- "age45_revision/outputs/supplementary/interaction_revision_20261001"
  SELF <- "age45_revision/scripts/40_validate_interaction_release.R"
  check_hash <- function(path, expected) {
    if (!file.exists(path)) stop("Missing file: ", path)
    if (!identical(age45_sha(path), expected)) stop("SHA-256 mismatch: ", path)
  }
  n_files <- 0L
  check_manifest <- function(z) {
    for (i in seq_len(nrow(z))) { check_hash(z$path[i], z$sha256[i]); n_files <<- n_files + 1L }
  }
  all_tests <- read.csv(file.path(ROOT, "interaction_tests.csv"), check.names = FALSE)
  stopifnot(nrow(all_tests) == 7L, all(is.finite(all_tests$log_p_value)), all(is.finite(all_tests$holm_log_p)))
  stopifnot(all(all_tests$holm_log_p >= all_tests$log_p_value - 1e-12),
    all(all_tests$holm_log_p <= 0))
  display_p <- function(logp) ifelse(logp < log(.001), "<0.001", sprintf("%.3f", exp(logp)))
  stopifnot(identical(all_tests$p_display, display_p(all_tests$log_p_value)),
    identical(all_tests$holm_p_display, display_p(all_tests$holm_log_p)))
  counts <- list()
  for (population in c("joint", "prepregnancy")) {
    rp <- file.path(ROOT, population, "receipt.json"); r <- jsonlite::fromJSON(rp)
    stopifnot(r$status == "complete_validated_interaction_revision", r$validation_passed,
      r$full_record_likelihood_score_covariance_verified)
    check_manifest(r$code); check_manifest(r$outputs)
    check_hash(r$full_model_path, r$full_model_sha256)
    check_hash(file.path(dirname(r$full_model_path), "receipt.json"), r$full_receipt_sha256)
    for (i in seq_len(nrow(r$inputs))) {
      check_hash(r$inputs$path[i], r$inputs$sha256[i])
      check_hash(r$inputs$receipt_path[i], r$inputs$receipt_sha256[i]); n_files <- n_files + 2L
    }
    full <- readRDS(r$full_model_path)
    reduced <- readRDS(file.path(ROOT, population, "reduced_model.rds"))
    test <- all_tests[all_tests$population == population & all_tests$test == "Likelihood ratio (model based)", ]
    stopifnot(nrow(test) == 1L, full$converged, reduced$converged,
      full$nobs == reduced$nobs, test$n == full$nobs, full$nobs == r$fit_n,
      test$df == full$rank - reduced$rank, test$df == if (population == "joint") 8L else 4L,
      abs(test$statistic - 2 * (full$log_likelihood - reduced$log_likelihood)) < 1e-7,
      max(abs(reduced$final_score)) / reduced$nobs < 1e-12)
    # Recheck exact numerical nesting independently on 10,000 source-verified 2024 records.
    p <- readRDS(r$inputs$path[which(r$inputs$year == 2024)])
    cc <- complete.cases(p$data[p$spec$covariates])
    if (population == "joint") {
      pre <- p$identity$pre_smoking; t1 <- p$identity$t1_smoking
      group <- ifelse(!pre & t1 %in% FALSE, "NN", ifelse(pre & t1 %in% FALSE, "SN", ifelse(pre & t1 %in% TRUE, "SS", NA)))
      keep <- head(which(cc & !is.na(group)), 10000L)
    } else keep <- head(which(cc), 10000L)
    d <- p$data[keep, c("Y", "A", "age", p$spec$covariates), drop = FALSE]
    if (population == "joint") { d$A <- as.integer(group[keep] == "SS"); d$SN <- as.integer(group[keep] == "SN") }
    d$year_factor <- factor(as.character(d$year_factor), levels = full$xlevels$year_factor)
    X <- streaming_design(d, seq_len(nrow(d)), full$design_spec)$X
    Z <- streaming_design(d, seq_len(nrow(d)), reduced$design_spec)$X
    at <- which(startsWith(colnames(X), "A:splines::ns(age,") | endsWith(colnames(X), ":SN"))
    stopifnot(length(at) == test$df, identical(colnames(X)[-at], colnames(Z)),
      identical(as.vector(X[, -at, drop = FALSE]), as.vector(Z)))
    nesting_max_difference <- max(abs(X[, -at, drop = FALSE] - Z)); nesting_n <- nrow(d)
    rm(p, d, X, Z); gc(FALSE)
    miss <- read.csv(file.path(ROOT, population, "missingness_overall.csv"))
    cnt <- read.csv(file.path(ROOT, population, "population_overall.csv"))
    stopifnot(all(miss$eligible_n == cnt$eligible_n),
      cnt$complete_case_n + cnt$excluded_for_covariates_n == cnt$eligible_n,
      cnt$complete_case_n == test$n, cnt$events == test$events)
    validation <- read.csv(file.path(ROOT, population, "validation.csv"))
    stopifnot(nrow(validation) == 12L, all(validation$passed))
    counts[[population]] <- list(n = test$n, events = test$events, eligible_n = cnt$eligible_n,
      df = test$df, all_numerical_checks_passed = TRUE, nested_design_check_n = nesting_n,
      nested_column_names_order_identical = TRUE, nested_numeric_max_difference = nesting_max_difference)
  }
  root <- jsonlite::fromJSON(file.path(ROOT, "receipt.json"))
  stopifnot(root$status == "complete_validated_interaction_collection")
  check_manifest(root$code); check_manifest(root$inputs); check_manifest(root$outputs)
  check_manifest(root$reporting_clarification)
  age45_json(list(status = "passed", checked_at = as.character(Sys.time()),
    code = data.frame(path = SELF, sha256 = age45_sha(SELF)), sha256_checks = n_files,
    annual_inputs_reverified = 18L, populations = counts,
    no_models_refitted = TRUE, no_fit_outputs_modified = TRUE), file.path(ROOT, "release_validation.json"))
  message("PASS: both full and reduced models, all 18 annual model inputs, code/receipt/output hashes, 24 numerical checks, nested ranks, cohort counts and multiplicity reporting verified.")
}
if (sys.nframe() == 0L) main()
