# Joint three-group model, SS / SN / NN with NN the reference (protocol addendum H, 19 September 2026).
#
#   logit P(Y = 1 | G, a, X) = g(a, X) + 1[G = SS] h_SS(a) + 1[G = SN] h_SN(a)
#
# One streaming logistic fit on the pooled SS, SN and NN complete cases (the common reference of the
# existing two-arm pair). The validated engine is unchanged: A = 1[SS] as the engine requires, and the
# SN block enters as the formula terms SN + SN:ns(age) on the locked age basis. HC0 covariance.
#
# Standardization to the pooled population at each age: SS vs NN through the engine's own routine with
# the target's SN fixed at 0 (toggling A gives NN and SS); SN vs NN through a two-pattern g-computation
# that reuses the engine's design builder and delta method and toggles SN with A fixed at 0. The
# two-pattern routine must first reproduce the engine's SS vs NN output (stopifnot) before its SN vs NN
# output is written. Crossovers: analyze_age_crossover on each contrast's coefficient block.
#
# Pre-specified validation (addendum H): agreement with supp_3lvl_continued_vs_none (SS vs NN) and
# supp_3lvl_stopped_vs_none (SN vs NN) within RR 0.003, RD 0.3 per 1,000, age-specific risk 0.2 per
# 1,000, crossover 0.2 years, identical simultaneous-reversal flag. Written to validation_against_pair.csv
# and to the receipt; a failure is reported, not hidden.
#
# Outputs: age45_revision/outputs/supplementary/joint_3group/ (own folder; not scanned by the assembler).
# One fit per R process. 21_fit_supplementary.R is not edited. No released result is touched.
#
# From 01_Analysis:
#   Rscript --vanilla age45_revision/scripts/33_fit_joint_three_group.R test 2024   # code test, one year
#   Rscript --vanilla age45_revision/scripts/33_fit_joint_three_group.R              # the fit
main <- function(mode = "full", test_year = NULL) {
  source("age45_revision/R/fit_helpers.R"); age45_initialize()
  suppressPackageStartupMessages(library(data.table))
  INP <- "age45_revision/derived/supplementary_inputs"; PAIR <- "age45_revision/outputs/supplementary/models"
  test <- identical(mode, "test")
  years <- if (test) as.integer(test_year) else 2016:2024
  ROOT <- if (test) file.path("age45_revision/outputs/supplementary/_joint_3group_test", paste(years, collapse = "_"))
          else "age45_revision/outputs/supplementary/joint_3group"
  dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
  SELF <- "age45_revision/scripts/33_fit_joint_three_group.R"
  manifest <- rbind(age45_code_manifest(), data.frame(path = c("age45_revision/scripts/20_prepare_supplementary_inputs.R", SELF),
    sha256 = c(age45_sha("age45_revision/scripts/20_prepare_supplementary_inputs.R"), age45_sha(SELF))))
  hash_frame <- function(x) digest::digest(list(names = names(x), n = nrow(x),
    columns = vapply(x, function(col) digest::digest(col, algo = "sha256"), "")), algo = "sha256")
  keys <- c("source_type", "year", "source_row"); ages <- 15:45
  t0 <- Sys.time(); stage <- function(msg) message(sprintf("[%5.1f min] %s", as.numeric(difftime(Sys.time(), t0, units = "mins")), msg))
  # ---- pooled SS, SN, NN complete cases ---------------------------------------------------------
  parts <- ids <- list(); spec <- NULL; eligible <- 0L
  for (y in years) {
    stem <- file.path(INP, paste0(y, "_prepregnancy_ext")); r <- jsonlite::fromJSON(paste0(stem, "_receipt.json"))
    stopifnot(r$status == "complete_prepared_supplementary", age45_sha(paste0(stem, ".rds")) == r$output_sha256)
    p <- readRDS(paste0(stem, ".rds")); spec <- p$spec; eligible <- eligible + nrow(p$data)
    stopifnot(nrow(p$data) == nrow(p$identity), !anyNA(p$identity$pre_smoking), all((p$data$A == 1) == p$identity$pre_smoking))
    cc <- rep(TRUE, nrow(p$data)); for (v in spec$covariates) cc <- cc & !is.na(p$data[[v]])
    pre <- p$identity$pre_smoking; t1 <- p$identity$t1_smoking
    grp <- ifelse(!pre & t1 %in% FALSE, "NN", ifelse(pre & t1 %in% FALSE, "SN", ifelse(pre & t1 %in% TRUE, "SS", NA)))
    keep <- which(cc & !is.na(grp))
    d <- p$data[keep, c("Y", "A", "age", spec$covariates), drop = FALSE]
    d$A <- as.integer(grp[keep] == "SS"); d$SN <- as.integer(grp[keep] == "SN")
    parts[[as.character(y)]] <- d; ids[[as.character(y)]] <- p$identity[keep, keys, drop = FALSE]
    stage(paste0("loaded ", y, ": kept ", length(keep), " of ", nrow(p$data))); rm(p, d, cc, pre, t1, grp, keep); gc(FALSE)
  }
  d <- as.data.frame(rbindlist(parts, use.names = TRUE)); ident <- as.data.frame(rbindlist(ids, use.names = TRUE)); rm(parts, ids); gc(FALSE)
  d$year_factor <- factor(as.character(d$year_factor), levels = years)
  if (test) {   # one year has a single level of year_factor; drop it for the code test only
    spec$covariates <- setdiff(spec$covariates, "year_factor")
    f0 <- paste(deparse(spec$formula, width.cutoff = 500), collapse = " ")
    spec$formula <- as.formula(gsub("\\s*\\+\\s*year_factor", "", f0), env = baseenv()); d$year_factor <- NULL
  }
  stopifnot(nrow(d) == nrow(ident), !anyNA(d), all(d$A + d$SN <= 1L), sum(d$A) > 0, sum(d$SN) > 0,
    sum(d$A == 0L & d$SN == 0L) > 0, !anyDuplicated(ident), all(d$age %in% ages))
  is_nn <- d$A == 0L & d$SN == 0L
  n_by <- c(NN = sum(is_nn), SN = sum(d$SN), SS = sum(d$A)); ev_by <- c(NN = sum(d$Y[is_nn]), SN = sum(d$Y[d$SN == 1L]), SS = sum(d$Y[d$A == 1L]))
  # ---- formula: the locked specification plus the SN block on the same locked age basis -------------
  ns_term <- .sep_spec_ns_term("age", spec$age_spec)
  f0 <- paste(deparse(spec$formula, width.cutoff = 500), collapse = " ")
  formula <- as.formula(paste0(f0, " + SN + SN:", ns_term), env = baseenv())
  signature <- digest::digest(list(data = hash_frame(d), identity = hash_frame(ident), formula = deparse(formula),
    age = spec$age_spec, code = manifest), algo = "sha256")
  rp <- file.path(ROOT, "receipt.json")
  if (file.exists(rp)) {
    old <- jsonlite::fromJSON(rp)
    if (identical(old$status, "complete_joint_three_group_fit") && identical(old$input_signature, signature)) {
      for (i in seq_len(nrow(old$outputs))) stopifnot(age45_sha(old$outputs$path[i]) == old$outputs$sha256[i])
      message("joint_3group: verified completed fit reused"); return(invisible(NULL))
    }
  }
  before <- Sys.time()
  age45_json(list(status = "running", model = "joint_3group", fit_n = nrow(d), n_by_group = as.list(n_by),
    input_signature = signature, started = as.character(before)), file.path(ROOT, "running.json"))
  stage(sprintf("fitting N=%d (NN %d, SN %d, SS %d); events %d", nrow(d), n_by["NN"], n_by["SN"], n_by["SS"], sum(d$Y)))
  fit <- fit_streaming_logit(d, formula, "Y", "A", "age", spec$age_spec, "HC0", 25000L)
  stopifnot(fit$converged, max(abs(fit$final_score)) / nrow(d) < 1e-12)
  saveRDS(fit, file.path(ROOT, "model.rds")); stopifnot(identical(readRDS(file.path(ROOT, "model.rds"))$beta, fit$beta))
  stage(sprintf("converged in %d iterations", fit$iterations))
  # ---- coefficient blocks --------------------------------------------------------------------------
  n <- names(fit$beta); K <- length(spec$age_spec$knots) + 2L
  a_at <- which(n == "A" | startsWith(n, "A:splines::ns(age,"))
  # model.matrix labels the interaction by variable order, so the columns are either
  # "SN:splines::ns(age,...)k" or "splines::ns(age,...)k:SN" with k the basis index.
  sn_int <- which(grepl("^SN:splines::ns\\(age,.*\\)[0-9]+$", n) | grepl("^splines::ns\\(age,.*\\)[0-9]+:SN$", n))
  sn_at <- c(which(n == "SN"), sn_int)
  stopifnot(length(a_at) == K, length(sn_at) == K, !length(intersect(a_at, sn_at)))
  basis_index <- as.integer(sub("^.*[^0-9]([0-9]+)(:SN)?$", "\\1", n[sn_int])); stopifnot(identical(basis_index, seq_len(K - 1L)))
  writeLines(c(paste("A block:", paste(n[a_at], collapse = " | ")), paste("SN block:", paste(n[sn_at], collapse = " | "))), file.path(ROOT, "coefficient_blocks.txt"))
  # ---- support by age and group --------------------------------------------------------------------
  grp <- factor(ifelse(d$A == 1L, "SS", ifelse(d$SN == 1L, "SN", "NN")), levels = c("NN", "SN", "SS"))
  support <- data.table(age = d$age, group = grp, Y = d$Y)[, .(n = .N, events = sum(Y), non_events = sum(1 - Y)), by = .(age, group)]
  support <- merge(CJ(age = ages, group = factor(c("NN", "SN", "SS"), levels = c("NN", "SN", "SS"))), support, by = c("age", "group"), all.x = TRUE)
  support[is.na(n), c("n", "events", "non_events") := list(0L, 0L, 0L)]; support[, sparse := events < 20 | non_events < 20]
  fwrite(support, file.path(ROOT, "age_group_support.csv")); stopifnot(all(support$n > 0)); rm(grp)
  # ---- (i) SS vs NN through the engine: target = pooled population with SN fixed at 0 ---------------
  d$SN <- 0L
  out_ss <- file.path(ROOT, "ss_vs_nn"); dir.create(out_ss, showWarnings = FALSE)
  age45_save_standardization(fit, d, out_ss, root_allowed = TRUE)
  stage("SS vs NN standardized and crossover written (engine routine)")
  # ---- (ii) two-pattern g-computation: engine design builder and delta method, SN toggled ----------
  accumulate <- function(target, pat0, pat1) {
    spec_d <- fit$design_spec; P <- length(fit$beta); chunk <- fit$chunk_size
    S <- matrix(0, 2, length(ages)); G <- array(0, c(2, length(ages), P)); N <- integer(length(ages))
    for (k in 1:2) {
      pat <- if (k == 1L) pat0 else pat1
      target$SN <- rep(as.integer(pat$SN), nrow(target))
      for (i in seq_along(ages)) {
        rows <- which(target[[fit$age]] == ages[i]); n_i <- length(rows); stopifnot(n_i > 0); N[i] <- n_i
        for (first in seq.int(1L, n_i, by = chunk)) {
          at <- rows[seq.int(first, min(n_i, first + chunk - 1L))]
          X <- streaming_design(target, at, spec_d, TRUE, ages[i], as.integer(pat$A))$X
          p <- stats::plogis(drop(X %*% fit$beta)); if (any(!is.finite(p))) stop("Nonfinite target predictions.")
          S[k, i] <- S[k, i] + sum(p); G[k, i, ] <- G[k, i, ] + colSums(X * (p * (1 - p)))
        }
      }
    }
    list(S = S, G = G, N = N)
  }
  finish <- function(acc, vcov) {
    z <- stats::qnorm(.975); tab <- vals <- grads <- meta <- vector("list", length(ages))
    for (i in seq_along(ages)) {
      n_i <- acc$N[i]; r <- acc$S[, i] / n_i; g <- acc$G[, i, ] / n_i
      if (any(r <= 0 | r >= 1)) stop("Boundary standardized risk; no clipping.")
      log_rr <- log(r[2]) - log(r[1])
      gradient <- rbind(risk0 = g[1, ], risk1 = g[2, ], rd = g[2, ] - g[1, ], log_rr = g[2, ] / r[2] - g[1, ] / r[1]); colnames(gradient) <- names(fit$beta)
      estimate <- c(risk0 = r[1], risk1 = r[2], rd = r[2] - r[1], log_rr = log_rr)
      variance <- gradient %*% vcov %*% t(gradient); if (any(diag(variance) < -1e-12)) stop("Negative target delta variance.")
      se <- sqrt(pmax(diag(variance), 0)); ci <- function(p, s) stats::plogis(stats::qlogis(p) + c(-1, 1) * z * s / (p * (1 - p)))
      ci0 <- ci(r[1], se[1]); ci1 <- ci(r[2], se[2]); rd <- unname(estimate["rd"])
      tab[[i]] <- data.frame(age = ages[i], target_n = n_i, risk0 = r[1], risk1 = r[2], rd = rd, rr = exp(log_rr),
        se_risk0 = se[1], se_risk1 = se[2], se_rd = se[3], se_log_rr = se[4], risk0_lower = ci0[1], risk0_upper = ci0[2],
        risk1_lower = ci1[1], risk1_upper = ci1[2], rd_lower = rd - z * se[3], rd_upper = rd + z * se[3],
        rr_lower = exp(log_rr - z * se[4]), rr_upper = exp(log_rr + z * se[4]), row.names = NULL)
      labels <- paste0(names(estimate), "@age=", format(ages[i], scientific = FALSE, trim = TRUE, digits = 15))
      names(estimate) <- rownames(gradient) <- labels; vals[[i]] <- estimate; grads[[i]] <- gradient
      meta[[i]] <- data.frame(estimand = labels, age = ages[i], target_n = n_i, measure = c("risk0", "risk1", "rd", "rr"),
        scale = c("identity", "identity", "identity", "log"), row.names = NULL)
    }
    estimate <- do.call(c, vals); gradient <- do.call(rbind, grads)
    covariance <- gradient %*% vcov %*% t(gradient); covariance <- (covariance + t(covariance)) / 2
    obj <- fit; obj$vcov <- vcov
    structure(list(summary = do.call(rbind, tab), estimate = estimate, gradient = gradient, covariance = covariance,
      metadata = do.call(rbind, meta), object = obj, target = "age_specific_empirical", level = .95,
      largest_target_design_rows = fit$chunk_size, stored_target_designs = FALSE, uncertainty_target = fit$uncertainty_target),
      class = "sep_streaming_standardized_risks")
  }
  # self-check: the two-pattern routine must reproduce the engine's SS vs NN output
  acc_ss <- accumulate(d, list(A = 0L, SN = 0L), list(A = 1L, SN = 0L))
  mine <- finish(acc_ss, fit$vcov_HC0)$summary
  eng <- as.data.frame(fread(file.path(out_ss, "HC0_age_standardized.csv")))
  cols <- names(mine); dev <- max(abs(as.matrix(mine[cols]) - as.matrix(eng[cols])) / pmax(1, abs(as.matrix(eng[cols]))))
  stopifnot(dev < 1e-9); rm(acc_ss, mine, eng)
  stage(sprintf("self-check passed: two-pattern routine reproduces the engine for SS vs NN (max relative deviation %.1e)", dev))
  # SN vs NN: risks and gradients once; both covariance estimators applied
  acc_sn <- accumulate(d, list(A = 0L, SN = 0L), list(A = 0L, SN = 1L))
  out_sn <- file.path(ROOT, "sn_vs_nn"); dir.create(out_sn, showWarnings = FALSE)
  for (cv in c("HC0", "model")) {
    V <- fit[[paste0("vcov_", cv)]]; st <- finish(acc_sn, V)
    ov <- overall_streaming_risks(st, as.numeric(nrow(d))); tab <- st$summary
    stopifnot(identical(as.numeric(tab$age), as.numeric(ages)), all(is.finite(tab$rr)), all(tab$risk0 > 0 & tab$risk0 < 1), all(tab$risk1 > 0 & tab$risk1 < 1))
    for (nm in c("risk0", "risk1", "rd", "risk0_lower", "risk0_upper", "risk1_lower", "risk1_upper", "rd_lower", "rd_upper")) tab[[paste0(nm, "_per1000")]] <- 1000 * tab[[nm]]
    crit <- stats::qnorm(1 - .05 / (2 * length(ages)))
    tab$rd_bonferroni_lower_per1000 <- 1000 * (tab$rd - crit * tab$se_rd); tab$rd_bonferroni_upper_per1000 <- 1000 * (tab$rd + crit * tab$se_rd)
    for (nm in c("estimate", "lower", "upper")) ov$summary[[paste0(nm, "_per1000")]] <- ifelse(ov$summary$measure == "rr", NA, 1000 * ov$summary[[nm]])
    fwrite(tab, file.path(out_sn, paste0(cv, "_age_standardized.csv"))); fwrite(ov$summary, file.path(out_sn, paste0(cv, "_overall_standardized.csv")))
    saveRDS(list(age = st, overall = ov), file.path(out_sn, paste0(cv, "_joint_standardization.rds")))
    co <- analyze_age_crossover(fit$beta[sn_at], V[sn_at, sn_at, drop = FALSE], spec$age_spec$knots, spec$age_spec$boundaries)
    ev <- evaluate_age_crossover(co, ages); stopifnot(all(sign(ev$h) == sign(tab$rd)))
    saveRDS(co, file.path(out_sn, paste0(cv, "_crossover.rds"))); fwrite(co$roots, file.path(out_sn, paste0(cv, "_roots.csv")))
    for (method in c("pointwise", "simultaneous")) fwrite(co[[method]]$confidence_set, file.path(out_sn, paste0(cv, "_", method, "_null_age_set.csv")))
  }
  rm(acc_sn); stage("SN vs NN standardized and crossover written")
  # ---- the three curves on one file, for Figure 2 and Table 2 --------------------------------------
  ss <- fread(file.path(out_ss, "HC0_age_standardized.csv")); sn <- fread(file.path(out_sn, "HC0_age_standardized.csv"))
  stopifnot(isTRUE(all.equal(ss$risk0, sn$risk0, tolerance = 1e-12)))   # one NN curve, by construction
  curves <- rbind(
    data.table(age = ss$age, group = "NN", risk_per1000 = ss$risk0_per1000, lower_per1000 = ss$risk0_lower_per1000, upper_per1000 = ss$risk0_upper_per1000),
    data.table(age = sn$age, group = "SN", risk_per1000 = sn$risk1_per1000, lower_per1000 = sn$risk1_lower_per1000, upper_per1000 = sn$risk1_upper_per1000),
    data.table(age = ss$age, group = "SS", risk_per1000 = ss$risk1_per1000, lower_per1000 = ss$risk1_lower_per1000, upper_per1000 = ss$risk1_upper_per1000))
  fwrite(curves, file.path(ROOT, "HC0_three_group_curves.csv"))
  # ---- pre-specified validation against the two-arm pair (full run only) ----------------------------
  validated <- NA
  if (!test) {
    main_root <- function(co) { r <- as.data.table(co$roots)[kind == "crossing"]; r <- r[age >= 20 & age <= 40]; if (nrow(r) > 1) r <- r[which.max(regular_delta)]; r }
    rows <- list()
    cmp <- function(label, contrast, pair_id, out, tol_rr = .003, tol_rd = .3, tol_risk = .2, tol_age = .2) {
      m_ov <- fread(file.path(out, "HC0_overall_standardized.csv")); p_ov <- fread(file.path(PAIR, pair_id, "HC0_overall_standardized.csv"))
      m_age <- fread(file.path(out, "HC0_age_standardized.csv")); p_age <- fread(file.path(PAIR, pair_id, "HC0_age_standardized.csv"))
      m_co <- readRDS(file.path(out, "HC0_crossover.rds")); p_co <- readRDS(file.path(PAIR, pair_id, "HC0_crossover.rds"))
      mr <- main_root(m_co); pr <- main_root(p_co)
      add <- function(q, mine, pair, tol) rows[[length(rows) + 1L]] <<- data.table(contrast = label, quantity = q, joint = mine, pair = pair,
        difference = mine - pair, tolerance = tol, within = abs(mine - pair) <= tol)
      add("overall RR", m_ov[measure == "rr", estimate], p_ov[measure == "rr", estimate], tol_rr)
      add("overall RD per 1,000", m_ov[measure == "rd", estimate_per1000], p_ov[measure == "rd", estimate_per1000], tol_rd)
      add("max |risk NN| per 1,000 over ages", max(abs(m_age$risk0_per1000 - p_age$risk0_per1000)), 0, tol_risk)
      add(paste0("max |risk ", contrast, "| per 1,000 over ages"), max(abs(m_age$risk1_per1000 - p_age$risk1_per1000)), 0, tol_risk)
      add("crossover age", mr$age, pr$age, tol_age); add("crossover local CI lower", mr$delta_lower, pr$delta_lower, tol_age); add("crossover local CI upper", mr$delta_upper, pr$delta_upper, tol_age)
      add("simultaneous reversal (1 = yes)", as.numeric(isTRUE(m_co$simultaneous$both_signs_demonstrated)), as.numeric(isTRUE(p_co$simultaneous$both_signs_demonstrated)), 0)
    }
    cmp("SS vs NN", "SS", "supp_3lvl_continued_vs_none", out_ss); cmp("SN vs NN", "SN", "supp_3lvl_stopped_vs_none", out_sn)
    pr_ss <- jsonlite::fromJSON(file.path(PAIR, "supp_3lvl_continued_vs_none", "receipt.json")); pr_sn <- jsonlite::fromJSON(file.path(PAIR, "supp_3lvl_stopped_vs_none", "receipt.json"))
    rows[[length(rows) + 1L]] <- data.table(contrast = "records", quantity = "NN records", joint = n_by[["NN"]], pair = pr_ss$A0, difference = n_by[["NN"]] - pr_ss$A0, tolerance = 0, within = n_by[["NN"]] == pr_ss$A0)
    rows[[length(rows) + 1L]] <- data.table(contrast = "records", quantity = "SS records", joint = n_by[["SS"]], pair = pr_ss$A1, difference = n_by[["SS"]] - pr_ss$A1, tolerance = 0, within = n_by[["SS"]] == pr_ss$A1)
    rows[[length(rows) + 1L]] <- data.table(contrast = "records", quantity = "SN records", joint = n_by[["SN"]], pair = pr_sn$A1, difference = n_by[["SN"]] - pr_sn$A1, tolerance = 0, within = n_by[["SN"]] == pr_sn$A1)
    val <- rbindlist(rows); fwrite(val, file.path(ROOT, "validation_against_pair.csv"))
    validated <- all(val$within); print(val[, .(contrast, quantity, joint = signif(joint, 6), pair = signif(pair, 6), difference = signif(difference, 3), tolerance, within)])
    stage(if (validated) "VALIDATION PASSED: joint model agrees with the two-arm pair within every pre-specified tolerance"
          else "VALIDATION FLAG: at least one quantity moved more than pre-specified; review validation_against_pair.csv before use")
  }
  files <- list.files(ROOT, full.names = TRUE, recursive = TRUE); files <- files[!basename(files) %in% c("receipt.json", "running.json")]
  age45_json(list(status = "complete_joint_three_group_fit", model = "joint_3group", protocol = "addendum H, 19 September 2026",
    mode = mode, years = years, eligible_n = eligible, fit_n = nrow(d), events = sum(d$Y), n_by_group = as.list(n_by), events_by_group = as.list(ev_by),
    reference = "pooled SS, SN, NN complete cases (fit rows)", input_signature = signature,
    input_signature_method = "columnwise SHA256 of fit data and identity keys; formula; age basis; code manifest",
    age_domain = c(15, 45), formula = paste(deparse(formula, width.cutoff = 500), collapse = " "),
    age_knots = spec$age_spec$knots, age_boundaries = spec$age_spec$boundaries, covariates = spec$covariates,
    coefficient_blocks = list(A = n[a_at], SN = n[sn_at]), no_imputation = TRUE, iterations = fit$iterations,
    score_max = max(abs(fit$final_score)), two_pattern_self_check_max_relative_deviation = dev,
    validated_against_pair = validated, code = manifest,
    elapsed_seconds = as.numeric(difftime(Sys.time(), before, units = "secs")),
    outputs = data.frame(path = files, sha256 = vapply(files, age45_sha, ""))), rp)
  unlink(file.path(ROOT, "running.json")); stage("complete"); invisible(NULL)
}
if (sys.nframe() == 0L) { a <- commandArgs(trailingOnly = TRUE); if (length(a) >= 2 && a[1] == "test") main("test", a[2]) else main("full") }
