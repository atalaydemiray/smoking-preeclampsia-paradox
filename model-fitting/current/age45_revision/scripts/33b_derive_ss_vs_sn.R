# SS vs SN from the joint three-group model (addendum H as amended 19 September 2026, about 01:00).
#
# Reads model.rds written by 33_fit_joint_three_group.R, rebuilds the identical pooled population with
# the same loader (the recomputed input signature must equal the fit receipt's), and derives the contrast
# between the two smoking groups: standardized SS and SN risks over the pooled population by the same
# two-pattern routine 33 validated against the engine (machine precision), RR and RD with the delta
# method, and the crossover from the difference of the two coefficient blocks,
#     h_SS(a) - h_SN(a),  covariance V_AA + V_SS - V_AS - V_SA,
# through analyze_age_crossover. No refit. Writes joint_3group/ss_vs_sn/ in the engine's layout plus a
# comparison with the released within-group fit primary_main (SS vs SN standardized to women who smoked
# before pregnancy), with the flag thresholds of addendum H (RR 0.02, RD 2 per 1,000, crossover 1.0 y).
#
# From 01_Analysis:
#   Rscript --vanilla age45_revision/scripts/33b_derive_ss_vs_sn.R test 2024
#   Rscript --vanilla age45_revision/scripts/33b_derive_ss_vs_sn.R
main <- function(mode = "full", test_year = NULL) {
  source("age45_revision/R/fit_helpers.R"); age45_initialize()
  suppressPackageStartupMessages(library(data.table))
  INP <- "age45_revision/derived/supplementary_inputs"
  test <- identical(mode, "test")
  years <- if (test) as.integer(test_year) else 2016:2024
  ROOT <- if (test) file.path("age45_revision/outputs/supplementary/_joint_3group_test", paste(years, collapse = "_"))
          else "age45_revision/outputs/supplementary/joint_3group"
  FIT <- "age45_revision/scripts/33_fit_joint_three_group.R"; SELF <- "age45_revision/scripts/33b_derive_ss_vs_sn.R"
  manifest <- rbind(age45_code_manifest(), data.frame(path = c("age45_revision/scripts/20_prepare_supplementary_inputs.R", FIT),
    sha256 = c(age45_sha("age45_revision/scripts/20_prepare_supplementary_inputs.R"), age45_sha(FIT))))
  hash_frame <- function(x) digest::digest(list(names = names(x), n = nrow(x),
    columns = vapply(x, function(col) digest::digest(col, algo = "sha256"), "")), algo = "sha256")
  keys <- c("source_type", "year", "source_row"); ages <- 15:45
  t0 <- Sys.time(); stage <- function(msg) message(sprintf("[%5.1f min] %s", as.numeric(difftime(Sys.time(), t0, units = "mins")), msg))
  # ---- the fit and its receipt ---------------------------------------------------------------------
  r <- jsonlite::fromJSON(file.path(ROOT, "receipt.json")); stopifnot(identical(r$status, "complete_joint_three_group_fit"))
  # The fit's stdout log lives in its folder and received its final line after the receipt hashed it;
  # every model and result file must match, log files are skipped.
  for (i in seq_len(nrow(r$outputs))) if (!grepl("_log\\.txt$", r$outputs$path[i]))
    stopifnot(age45_sha(r$outputs$path[i]) == r$outputs$sha256[i])
  fit <- readRDS(file.path(ROOT, "model.rds"))
  # ---- the identical pooled population --------------------------------------------------------------
  parts <- ids <- list(); spec <- NULL
  for (y in years) {
    stem <- file.path(INP, paste0(y, "_prepregnancy_ext")); rr <- jsonlite::fromJSON(paste0(stem, "_receipt.json"))
    stopifnot(rr$status == "complete_prepared_supplementary", age45_sha(paste0(stem, ".rds")) == rr$output_sha256)
    p <- readRDS(paste0(stem, ".rds")); spec <- p$spec
    cc <- rep(TRUE, nrow(p$data)); for (v in spec$covariates) cc <- cc & !is.na(p$data[[v]])
    pre <- p$identity$pre_smoking; t1 <- p$identity$t1_smoking
    grp <- ifelse(!pre & t1 %in% FALSE, "NN", ifelse(pre & t1 %in% FALSE, "SN", ifelse(pre & t1 %in% TRUE, "SS", NA)))
    keep <- which(cc & !is.na(grp))
    d <- p$data[keep, c("Y", "A", "age", spec$covariates), drop = FALSE]
    d$A <- as.integer(grp[keep] == "SS"); d$SN <- as.integer(grp[keep] == "SN")
    parts[[as.character(y)]] <- d; ids[[as.character(y)]] <- p$identity[keep, keys, drop = FALSE]
    rm(p, d, cc, pre, t1, grp, keep); gc(FALSE)
  }
  d <- as.data.frame(rbindlist(parts, use.names = TRUE)); ident <- as.data.frame(rbindlist(ids, use.names = TRUE)); rm(parts, ids); gc(FALSE)
  d$year_factor <- factor(as.character(d$year_factor), levels = years)
  if (test) { spec$covariates <- setdiff(spec$covariates, "year_factor")
    f0 <- paste(deparse(spec$formula, width.cutoff = 500), collapse = " ")
    spec$formula <- as.formula(gsub("\\s*\\+\\s*year_factor", "", f0), env = baseenv()); d$year_factor <- NULL }
  ns_term <- .sep_spec_ns_term("age", spec$age_spec)
  f0 <- paste(deparse(spec$formula, width.cutoff = 500), collapse = " ")
  formula <- as.formula(paste0(f0, " + SN + SN:", ns_term), env = baseenv())
  signature <- digest::digest(list(data = hash_frame(d), identity = hash_frame(ident), formula = deparse(formula),
    age = spec$age_spec, code = manifest), algo = "sha256")
  stopifnot(identical(signature, r$input_signature))     # the same records, formula, basis and code as the fit
  stage(sprintf("population rebuilt and matches the fit receipt (N=%d)", nrow(d)))
  n <- names(fit$beta); a_at <- match(r$coefficient_blocks$A, n); sn_at <- match(r$coefficient_blocks$SN, n)
  stopifnot(!anyNA(a_at), !anyNA(sn_at), length(a_at) == length(sn_at))
  # ---- two-pattern standardization (identical to 33; validated there to 4.6e-15 against the engine) ----
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
      n_i <- acc$N[i]; rk <- acc$S[, i] / n_i; g <- acc$G[, i, ] / n_i
      if (any(rk <= 0 | rk >= 1)) stop("Boundary standardized risk; no clipping.")
      log_rr <- log(rk[2]) - log(rk[1])
      gradient <- rbind(risk0 = g[1, ], risk1 = g[2, ], rd = g[2, ] - g[1, ], log_rr = g[2, ] / rk[2] - g[1, ] / rk[1]); colnames(gradient) <- names(fit$beta)
      estimate <- c(risk0 = rk[1], risk1 = rk[2], rd = rk[2] - rk[1], log_rr = log_rr)
      variance <- gradient %*% vcov %*% t(gradient); if (any(diag(variance) < -1e-12)) stop("Negative target delta variance.")
      se <- sqrt(pmax(diag(variance), 0)); ci <- function(p, s) stats::plogis(stats::qlogis(p) + c(-1, 1) * z * s / (p * (1 - p)))
      ci0 <- ci(rk[1], se[1]); ci1 <- ci(rk[2], se[2]); rd <- unname(estimate["rd"])
      tab[[i]] <- data.frame(age = ages[i], target_n = n_i, risk0 = rk[1], risk1 = rk[2], rd = rd, rr = exp(log_rr),
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
  # pattern 0 = SN (A = 0, SN = 1), pattern 1 = SS (A = 1, SN = 0): risk0 is SN, risk1 is SS
  acc <- accumulate(d, list(A = 0L, SN = 1L), list(A = 1L, SN = 0L))
  out <- file.path(ROOT, "ss_vs_sn"); dir.create(out, showWarnings = FALSE)
  for (cv in c("HC0", "model")) {
    V <- fit[[paste0("vcov_", cv)]]; st <- finish(acc, V)
    ov <- overall_streaming_risks(st, as.numeric(nrow(d))); tab <- st$summary
    stopifnot(identical(as.numeric(tab$age), as.numeric(ages)), all(is.finite(tab$rr)), all(tab$risk0 > 0 & tab$risk0 < 1), all(tab$risk1 > 0 & tab$risk1 < 1))
    for (nm in c("risk0", "risk1", "rd", "risk0_lower", "risk0_upper", "risk1_lower", "risk1_upper", "rd_lower", "rd_upper")) tab[[paste0(nm, "_per1000")]] <- 1000 * tab[[nm]]
    crit <- stats::qnorm(1 - .05 / (2 * length(ages)))
    tab$rd_bonferroni_lower_per1000 <- 1000 * (tab$rd - crit * tab$se_rd); tab$rd_bonferroni_upper_per1000 <- 1000 * (tab$rd + crit * tab$se_rd)
    for (nm in c("estimate", "lower", "upper")) ov$summary[[paste0(nm, "_per1000")]] <- ifelse(ov$summary$measure == "rr", NA, 1000 * ov$summary[[nm]])
    fwrite(tab, file.path(out, paste0(cv, "_age_standardized.csv"))); fwrite(ov$summary, file.path(out, paste0(cv, "_overall_standardized.csv")))
    saveRDS(list(age = st, overall = ov), file.path(out, paste0(cv, "_joint_standardization.rds")))
    # crossover of the derived contrast: difference of the two coefficient blocks on the same basis
    beta_d <- fit$beta[a_at] - fit$beta[sn_at]
    V_d <- V[a_at, a_at, drop = FALSE] + V[sn_at, sn_at, drop = FALSE] - V[a_at, sn_at, drop = FALSE] - V[sn_at, a_at, drop = FALSE]
    V_d <- (V_d + t(V_d)) / 2; dimnames(V_d) <- list(names(beta_d), names(beta_d))
    co <- analyze_age_crossover(beta_d, V_d, spec$age_spec$knots, spec$age_spec$boundaries)
    ev <- evaluate_age_crossover(co, ages); stopifnot(all(sign(ev$h) == sign(tab$rd)))
    saveRDS(co, file.path(out, paste0(cv, "_crossover.rds"))); fwrite(co$roots, file.path(out, paste0(cv, "_roots.csv")))
    for (method in c("pointwise", "simultaneous")) fwrite(co[[method]]$confidence_set, file.path(out, paste0(cv, "_", method, "_null_age_set.csv")))
  }
  # support: SN is the reference arm (A = 0), SS the exposed arm (A = 1)
  g <- fread(file.path(ROOT, "age_group_support.csv"))[group %in% c("SN", "SS")]; g[, A := as.integer(group == "SS")]
  fwrite(g[order(age, A), .(age, A, n, events, non_events, sparse)], file.path(out, "age_arm_support.csv"))
  stage("SS vs SN standardized (pooled population) and crossover written")
  # ---- comparison with the released within-group fit (expected to differ; see addendum H) ------------
  flagged <- NA
  if (!test) {
    pm <- "age45_revision/outputs/models/primary_main"
    main_root <- function(co) { z <- as.data.table(co$roots)[kind == "crossing" & age >= 20 & age <= 40]; if (nrow(z) > 1) z <- z[which.max(regular_delta)]; z }
    m_ov <- fread(file.path(out, "HC0_overall_standardized.csv")); p_ov <- fread(file.path(pm, "HC0_overall_standardized.csv"))
    m_co <- readRDS(file.path(out, "HC0_crossover.rds")); p_co <- readRDS(file.path(pm, "HC0_crossover.rds")); mr <- main_root(m_co); pr <- main_root(p_co)
    rows <- list(); add <- function(q, mine, pair, tol) rows[[length(rows) + 1L]] <<- data.table(quantity = q, joint = mine, primary_main = pair,
      difference = mine - pair, flag_threshold = tol, within = abs(mine - pair) <= tol)
    add("overall RR", m_ov[measure == "rr", estimate], p_ov[measure == "rr", estimate], .02)
    add("overall RD per 1,000", m_ov[measure == "rd", estimate_per1000], p_ov[measure == "rd", estimate_per1000], 2)
    add("standardized risk SN per 1,000", m_ov[measure == "risk0", estimate_per1000], p_ov[measure == "risk0", estimate_per1000], NA_real_)
    add("standardized risk SS per 1,000", m_ov[measure == "risk1", estimate_per1000], p_ov[measure == "risk1", estimate_per1000], NA_real_)
    add("crossover age", mr$age, pr$age, 1); add("crossover local CI lower", mr$delta_lower, pr$delta_lower, 1); add("crossover local CI upper", mr$delta_upper, pr$delta_upper, 1)
    add("simultaneous reversal (1 = yes)", as.numeric(isTRUE(m_co$simultaneous$both_signs_demonstrated)), as.numeric(isTRUE(p_co$simultaneous$both_signs_demonstrated)), 0)
    val <- rbindlist(rows); val[is.na(flag_threshold), within := NA]
    fwrite(val, file.path(out, "comparison_with_primary_main.csv"))
    flagged <- any(val$within %in% FALSE)
    print(val[, .(quantity, joint = signif(joint, 6), primary_main = signif(primary_main, 6), difference = signif(difference, 3), flag_threshold, within)])
    stage(if (flagged) "FLAG: SS vs SN moved more than the addendum H thresholds relative to primary_main; review before use"
          else "SS vs SN within the addendum H thresholds of primary_main (differences reflect the standardization population and the common covariate function)")
  }
  files <- list.files(out, full.names = TRUE); files <- files[basename(files) != "receipt.json"]
  age45_json(list(status = "complete_joint_ss_vs_sn_derived", model = "joint_3group", contrast = "SS vs SN", protocol = "addendum H as amended 19 September 2026",
    fit_receipt_signature = r$input_signature, model_rds_sha256 = age45_sha(file.path(ROOT, "model.rds")), fit_n = nrow(d),
    reference_pattern = "SN (A = 0, SN = 1)", exposed_pattern = "SS (A = 1, SN = 0)", standardization = "pooled SS, SN, NN complete cases at each age",
    crossover = "analyze_age_crossover on beta_A - beta_SN with V_AA + V_SS - V_AS - V_SA", flagged_against_primary_main = flagged,
    code = rbind(manifest, data.frame(path = SELF, sha256 = age45_sha(SELF))),
    elapsed_seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    outputs = data.frame(path = files, sha256 = vapply(files, age45_sha, ""))), file.path(out, "receipt.json"))
  stage("complete"); invisible(NULL)
}
if (sys.nframe() == 0L) { a <- commandArgs(trailingOnly = TRUE); if (length(a) >= 2 && a[1] == "test") main("test", a[2]) else main("full") }
