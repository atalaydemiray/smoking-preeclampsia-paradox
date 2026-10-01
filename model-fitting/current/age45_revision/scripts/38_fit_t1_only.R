# First-trimester-marginal model (protocol addendum J, 19 September 2026): smoking in the first trimester
# versus none in the first trimester, irrespective of before-pregnancy smoking.
#
#   logit P(Y = 1 | A, a, X) = g(a, X) + A h(a),   A = 1 if any cigarettes reported in T1
#
# Same engine, age basis, nine covariates, HC0 covariance, age-specific empirical standardization and
# crossover procedure as prepregnancy_main. Before-pregnancy smoking is NOT adjusted for, by symmetry with
# the before-pregnancy model (addendum J). Population: eligible complete cases with a KNOWN T1 field, that
# is SS + NS + SN + NN; records with an unknown T1 value are excluded because the exposure is undefined.
#
# Writes age45_revision/outputs/supplementary/t1_only/ (its own folder; the assembler scans
# outputs/supplementary/models/, so this model is not picked up there). Refits nothing else and touches no
# released result. One fit per R process; expect about 20 to 30 minutes and roughly 13 GB.
#
# From 01_Analysis:
#   Rscript --vanilla age45_revision/scripts/38_fit_t1_only.R test 2024   # code test, one year
#   Rscript --vanilla age45_revision/scripts/38_fit_t1_only.R             # the fit
main <- function(mode = "full", test_year = NULL) {
  source("age45_revision/R/fit_helpers.R"); age45_initialize()
  suppressPackageStartupMessages(library(data.table))
  INP <- "age45_revision/derived/supplementary_inputs"
  test <- identical(mode, "test")
  years <- if (test) as.integer(test_year) else 2016:2024
  ROOT <- if (test) file.path("age45_revision/outputs/supplementary/_t1_only_test", paste(years, collapse = "_"))
          else "age45_revision/outputs/supplementary/t1_only"
  dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
  SELF <- "age45_revision/scripts/38_fit_t1_only.R"
  manifest <- rbind(age45_code_manifest(), data.frame(
    path = c("age45_revision/scripts/20_prepare_supplementary_inputs.R", SELF),
    sha256 = c(age45_sha("age45_revision/scripts/20_prepare_supplementary_inputs.R"), age45_sha(SELF))))
  hash_frame <- function(x) digest::digest(list(names = names(x), n = nrow(x),
    columns = vapply(x, function(col) digest::digest(col, algo = "sha256"), "")), algo = "sha256")
  keys <- c("source_type", "year", "source_row"); ages <- 15:45
  t0 <- Sys.time(); stage <- function(m) message(sprintf("[%5.1f min] %s", as.numeric(difftime(Sys.time(), t0, units = "mins")), m))
  # ---- population: eligible complete cases with a known first-trimester field -------------------------
  parts <- ids <- list(); spec <- NULL; eligible <- 0L; t1_unknown <- 0L
  grp_counts <- list()
  for (y in years) {
    stem <- file.path(INP, paste0(y, "_prepregnancy_ext")); r <- jsonlite::fromJSON(paste0(stem, "_receipt.json"))
    stopifnot(r$status == "complete_prepared_supplementary", age45_sha(paste0(stem, ".rds")) == r$output_sha256)
    p <- readRDS(paste0(stem, ".rds")); spec <- p$spec; eligible <- eligible + nrow(p$data)
    stopifnot(nrow(p$data) == nrow(p$identity), !anyNA(p$identity$pre_smoking),
              all((p$data$A == 1) == p$identity$pre_smoking), all(p$data$age %in% ages))
    cc <- rep(TRUE, nrow(p$data)); for (v in spec$covariates) cc <- cc & !is.na(p$data[[v]])
    pre <- p$identity$pre_smoking; t1 <- p$identity$t1_smoking
    known <- t1 %in% c(TRUE, FALSE)
    t1_unknown <- t1_unknown + sum(cc & !known)
    keep <- which(cc & known)
    d <- p$data[keep, c("Y", "A", "age", spec$covariates), drop = FALSE]
    d$A <- as.integer(t1[keep])                       # the exposure is the first-trimester field
    pat <- ifelse(pre[keep], ifelse(t1[keep], "SS", "SN"), ifelse(t1[keep], "NS", "NN"))
    grp_counts[[as.character(y)]] <- data.table(pattern = pat, Y = d$Y)[, .(n = .N, events = sum(Y)), by = pattern]
    parts[[as.character(y)]] <- d; ids[[as.character(y)]] <- p$identity[keep, keys, drop = FALSE]
    stage(paste0("loaded ", y, ": kept ", length(keep), " of ", nrow(p$data), " (T1 unknown among complete cases: ", sum(cc & !known), ")"))
    rm(p, d, cc, pre, t1, known, keep, pat); gc(FALSE)
  }
  d <- as.data.frame(rbindlist(parts, use.names = TRUE)); ident <- as.data.frame(rbindlist(ids, use.names = TRUE))
  rm(parts, ids); gc(FALSE)
  d$year_factor <- factor(as.character(d$year_factor), levels = years)
  if (test) {                                        # one year has a single level of year_factor
    spec$covariates <- setdiff(spec$covariates, "year_factor")
    f0 <- paste(deparse(spec$formula, width.cutoff = 500), collapse = " ")
    spec$formula <- as.formula(gsub("\\s*\\+\\s*year_factor", "", f0), env = baseenv()); d$year_factor <- NULL
  }
  stopifnot(nrow(d) == nrow(ident), !anyNA(d), all(d$A %in% 0:1), sum(d$A) > 0, sum(d$A == 0) > 0, !anyDuplicated(ident))
  gc <- rbindlist(grp_counts)[, .(n = sum(n), events = sum(events)), by = pattern][order(-n)]
  gc[, crude_per1000 := round(1000 * events / n, 2)]
  fwrite(gc, file.path(ROOT, "arm_composition.csv")); print(gc)
  stopifnot(setequal(gc$pattern, c("SS", "SN", "NN", "NS")))
  signature <- digest::digest(list(data = hash_frame(d), identity = hash_frame(ident),
    formula = deparse(spec$formula), age = spec$age_spec, code = manifest), algo = "sha256")
  rp <- file.path(ROOT, "receipt.json")
  if (file.exists(rp)) {
    old <- jsonlite::fromJSON(rp)
    if (identical(old$status, "complete_t1_only_fit") && identical(old$input_signature, signature)) {
      for (i in seq_len(nrow(old$outputs))) if (!grepl("_log\\.txt$", old$outputs$path[i]))
        stopifnot(age45_sha(old$outputs$path[i]) == old$outputs$sha256[i])
      message("t1_only: verified completed fit reused"); return(invisible(NULL))
    }
  }
  before <- Sys.time()
  age45_json(list(status = "running", model = "t1_only", fit_n = nrow(d), A1 = sum(d$A), A0 = sum(d$A == 0),
    input_signature = signature, started = as.character(before)), file.path(ROOT, "running.json"))
  stage(sprintf("fitting N=%d (T1 positive %d, T1 negative %d); events %d", nrow(d), sum(d$A), sum(d$A == 0), sum(d$Y)))
  fit <- fit_streaming_logit(d, spec$formula, "Y", "A", "age", spec$age_spec, "HC0", 25000L)
  stopifnot(fit$converged, max(abs(fit$final_score)) / nrow(d) < 1e-12)
  saveRDS(fit, file.path(ROOT, "model.rds")); stopifnot(identical(readRDS(file.path(ROOT, "model.rds"))$beta, fit$beta))
  stage(sprintf("converged in %d iterations", fit$iterations))
  support <- data.table(age = d$age, A = d$A, Y = d$Y)[, .(n = .N, events = sum(Y), non_events = sum(1 - Y)), by = .(age, A)]
  support <- merge(CJ(age = ages, A = 0:1), support, by = c("age", "A"), all.x = TRUE)
  support[is.na(n), c("n", "events", "non_events") := list(0L, 0L, 0L)]
  support[, sparse := events < 20 | non_events < 20]
  fwrite(support, file.path(ROOT, "age_arm_support.csv")); stopifnot(all(support$n > 0))
  age45_save_standardization(fit, d, ROOT, root_allowed = TRUE)
  stage("standardized and crossover written")
  # ---- pre-specified comparison with SS vs NN of the joint model (addendum J) -------------------------
  flagged <- NA
  if (!test) {
    J <- "age45_revision/outputs/supplementary/joint_3group/ss_vs_nn"
    if (file.exists(file.path(J, "HC0_overall_standardized.csv"))) {
      main_root <- function(co) { z <- as.data.table(co$roots)[kind == "crossing" & age >= 20 & age <= 40]
                                  if (nrow(z) > 1) z <- z[which.max(regular_delta)]; z }
      m_ov <- fread(file.path(ROOT, "HC0_overall_standardized.csv")); j_ov <- fread(file.path(J, "HC0_overall_standardized.csv"))
      m_co <- readRDS(file.path(ROOT, "HC0_crossover.rds")); j_co <- readRDS(file.path(J, "HC0_crossover.rds"))
      mr <- main_root(m_co); jr <- main_root(j_co)
      rows <- list(); add <- function(q, a, b, tol) rows[[length(rows) + 1L]] <<- data.table(
        quantity = q, t1_only = a, ss_vs_nn = b, difference = a - b, expected_within = tol, within = abs(a - b) <= tol)
      add("overall RR", m_ov[measure == "rr", estimate], j_ov[measure == "rr", estimate], .02)
      add("overall RD per 1,000", m_ov[measure == "rd", estimate_per1000], j_ov[measure == "rd", estimate_per1000], 2)
      add("standardized risk, reference per 1,000", m_ov[measure == "risk0", estimate_per1000], j_ov[measure == "risk0", estimate_per1000], NA_real_)
      add("standardized risk, exposed per 1,000", m_ov[measure == "risk1", estimate_per1000], j_ov[measure == "risk1", estimate_per1000], NA_real_)
      add("crossover age", mr$age, jr$age, 1)
      add("crossover local CI lower", mr$delta_lower, jr$delta_lower, 1)
      add("crossover local CI upper", mr$delta_upper, jr$delta_upper, 1)
      add("simultaneous reversal (1 = yes)", as.numeric(isTRUE(m_co$simultaneous$both_signs_demonstrated)),
          as.numeric(isTRUE(j_co$simultaneous$both_signs_demonstrated)), 0)
      val <- rbindlist(rows); val[is.na(expected_within), within := NA]
      fwrite(val, file.path(ROOT, "comparison_with_ss_vs_nn.csv"))
      flagged <- any(val$within %in% FALSE)
      print(val[, .(quantity, t1_only = signif(t1_only, 6), ss_vs_nn = signif(ss_vs_nn, 6),
                    difference = signif(difference, 3), expected_within, within)])
      stage(if (flagged) "FLAG: the first-trimester margin diverges from SS vs NN by more than addendum J expected; report before use"
            else "within the range addendum J expected of SS vs NN")
    } else message("joint SS vs NN outputs not found; comparison skipped")
  }
  files <- list.files(ROOT, full.names = TRUE, recursive = TRUE)
  files <- files[!basename(files) %in% c("receipt.json", "running.json")]
  age45_json(list(status = "complete_t1_only_fit", model = "t1_only", protocol = "addendum J, 19 September 2026",
    mode = mode, years = years, eligible_n = eligible, fit_n = nrow(d), events = sum(d$Y),
    A1 = sum(d$A), A0 = sum(d$A == 0), t1_unknown_excluded = t1_unknown,
    arm_composition = as.list(setNames(gc$n, gc$pattern)),
    reference = "own complete cases (fit rows)", adjusts_for_prepregnancy_smoking = FALSE,
    input_signature = signature,
    input_signature_method = "columnwise SHA256 of fit data and identity keys; formula; age basis; code manifest",
    age_domain = c(15, 45), formula = paste(deparse(spec$formula, width.cutoff = 500), collapse = " "),
    age_knots = spec$age_spec$knots, age_boundaries = spec$age_spec$boundaries, covariates = spec$covariates,
    no_imputation = TRUE, iterations = fit$iterations, score_max = max(abs(fit$final_score)),
    flagged_against_ss_vs_nn = flagged, code = manifest,
    elapsed_seconds = as.numeric(difftime(Sys.time(), before, units = "secs")),
    outputs = data.frame(path = files, sha256 = vapply(files, age45_sha, ""))), rp)
  unlink(file.path(ROOT, "running.json")); stage("complete"); invisible(NULL)
}
if (sys.nframe() == 0L) { a <- commandArgs(trailingOnly = TRUE); if (length(a) >= 2 && a[1] == "test") main("test", a[2]) else main("full") }
