# Aggregate-only quantitative bias analyses for the supplement
# (supplementary amendment, 11 September 2026, group S-H). Reads completed main
# model outputs and the external anchors; writes CSV tables and a receipt to
# outputs/supplementary/bias. No fitting, no data access, no priors,
# no causal correction: every number is a fixed-assumption scenario or an algebraic bound.
# From model-fitting/: Rscript --vanilla scripts/15_supplementary_bias.R
main <- function() {
  suppressMessages(library(data.table))
  sha <- function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE)
  out <- "outputs/supplementary/bias"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
  anchors <- jsonlite::fromJSON("config/bias_anchors.json", simplifyVector = TRUE)
  contrasts <- c(primary = "primary_main", broad = "broad_main", prepregnancy = "prepregnancy_main")
  labels <- c(primary = "T1 smoking vs abstinence among women who smoked before pregnancy",
    broad = "Smoking before pregnancy and in T1 vs neither", prepregnancy = "Prepregnancy smoking vs none")
  inputs <- character()
  load_contrast <- function(ct) {
    base <- file.path("outputs/models", contrasts[[ct]])
    r <- jsonlite::fromJSON(file.path(base, "receipt.json"))
    stopifnot(r$status == "complete_validated_age15_45_fit")
    files <- file.path(base, c("HC0_age_standardized.csv", "HC0_overall_standardized.csv", "age_arm_support.csv"))
    for (f in files) { e <- r$outputs[basename(r$outputs$path) == basename(f), ]; stopifnot(nrow(e) == 1L, sha(f) == e$sha256) }
    inputs[files] <<- vapply(files, sha, "")
    age <- fread(files[1]); ov <- fread(files[2]); sup <- fread(files[3])
    list(age = age, overall = ov, support = sup)
  }
  # ---- (1) E-values / minimum bias strength, all three contrasts, by age ---------------
  evalue <- function(rr) { r <- ifelse(rr < 1, 1 / rr, rr); r + sqrt(r * (r - 1)) }
  ev_ci <- function(rr, lo, hi) { # nearest CI limit to the null; 1 if the CI covers the null
    if (lo <= 1 && hi >= 1) return(1)
    evalue(if (rr < 1) hi else lo)
  }
  ev <- list()
  for (ct in names(contrasts)) {
    z <- load_contrast(ct); o <- z$overall
    rows <- rbind(data.table(age = NA_integer_, target_n = o[measure == "rr", target_n], rr = o[measure == "rr", estimate],
      rr_lower = o[measure == "rr", lower], rr_upper = o[measure == "rr", upper]),
      z$age[, .(age, target_n, rr, rr_lower, rr_upper)])
    rows[, `:=`(contrast = ct, label = labels[[ct]], evalue_point = evalue(rr),
      evalue_ci_limit = mapply(ev_ci, rr, rr_lower, rr_upper),
      direction = ifelse(rr < 1, "inverse", "positive"))]
    ev[[ct]] <- rows
  }
  ev <- rbindlist(ev)
  ev[, interpretation := "Minimum strength, on the risk-ratio scale, that an unmeasured factor would need with both exposure and outcome (confounding) or with both selection and outcome (selection; Smith and VanderWeele 2019) to move the estimate to the null. Algebraic benchmark, not a validated causal bound."]
  fwrite(ev, file.path(out, "S_evalue_by_age_all_contrasts.csv"))
  # ---- (2) Live-birth selection: left-truncation mechanism by age ---------------------
  # Latent hypertension-prone type U (prevalence pi, GH/PE risk ratio RRuy). Losses:
  # non-smoking pregnancies lose a fraction m at every type; smoking multiplies loss by
  # lambda_N (other pregnancies) and lambda_U (U-type), with marginal smoking loss RR fixed at
  # lambda = pi*lambda_U + (1-pi)*lambda_N. Under a true null within type, the live-birth
  # risk ratio is [1 + piS*(RRuy-1)] / [1 + pi*(RRuy-1)] with piS the live-birth U share among
  # women who smoked. We solve the type-differential delta = lambda_U/lambda_N that reproduces each
  # observed age-specific RR, then hold delta fixed and predict the RR the mechanism implies
  # at every other age under each loss curve.
  loss_curve <- function(name, ages) switch(name,
    flat_12 = rep(0.12, length(ages)), flat_30 = rep(0.30, length(ages)),
    age_graded = { g <- c(0.16, 0.11, 0.10, 0.12, 0.17, 0.33, 0.53)
      g[findInterval(ages, c(15, 20, 25, 30, 35, 40, 45))] },
    steep = ifelse(ages <= 29, 0.10, 0.10 + (0.53 - 0.10) * (ages - 29) / (45 - 29)))
  implied_rr <- function(delta, m, lambda, pi, rruy) {
    lambda_N <- lambda / (pi * delta + (1 - pi)); lambda_U <- delta * lambda_N
    if (m * lambda_U >= 1 || m * lambda_N >= 1) return(NA_real_)
    piS <- pi * (1 - m * lambda_U) / (pi * (1 - m * lambda_U) + (1 - pi) * (1 - m * lambda_N))
    (1 + piS * (rruy - 1)) / (1 + pi * (rruy - 1))
  }
  solve_delta <- function(target, m, lambda, pi, rruy) {
    # Feasible differentials keep every loss probability below 1: m*lambda_U < 1.
    # lambda_U(delta) = delta*lambda/(pi*delta+1-pi) rises to lambda/pi as delta grows.
    if (target >= 1) return(c(delta = NA_real_, feasible = NA, min_reachable_rr = NA_real_))
    delta_max <- if (lambda / pi < 1 / m) 1e6 else {
      # solve m*lambda_U(delta) = 1 - 1e-9 for delta
      g <- function(ld) m * exp(ld) * lambda / (pi * exp(ld) + 1 - pi) - (1 - 1e-9)
      if (g(0) >= 0) return(c(delta = NA_real_, feasible = FALSE, min_reachable_rr = NA_real_))
      exp(uniroot(g, c(0, log(1e12)), tol = 1e-12)$root) * (1 - 1e-9)
    }
    f <- function(ld) implied_rr(exp(ld), m, lambda, pi, rruy) - target
    lo <- f(0); hi <- f(log(delta_max))
    if (is.na(hi) || is.na(lo)) return(c(delta = NA_real_, feasible = FALSE, min_reachable_rr = NA_real_))
    min_rr <- implied_rr(delta_max, m, lambda, pi, rruy)
    if (sign(lo) == sign(hi)) return(c(delta = NA_real_, feasible = FALSE, min_reachable_rr = min_rr))
    c(delta = exp(uniroot(f, c(0, log(delta_max)), tol = 1e-10)$root), feasible = TRUE, min_reachable_rr = min_rr)
  }
  primary <- load_contrast("primary")$age
  broad <- load_contrast("broad")$age
  sel <- list()
  for (ct in c("primary", "broad")) {
    a <- if (ct == "primary") primary else broad
    for (curve in c("flat_12", "flat_30", "age_graded", "steep")) for (lambda in anchors$smoking_and_miscarriage$scenario_values)
      for (pi in anchors$latent_type_scenarios$prevalence_of_hypertension_prone_type)
        for (rruy in anchors$latent_type_scenarios$gh_pe_risk_ratio_for_type) {
          m <- loss_curve(curve, a$age)
          req <- t(mapply(function(rr, mm) solve_delta(rr, mm, lambda, pi, rruy), a$rr, m))
          # Calibrate delta at the strongest inverse association the mechanism can reach, then predict elsewhere.
          feas_idx <- which(req[, "feasible"] %in% TRUE)
          k <- if (length(feas_idx)) feas_idx[which.min(a$rr[feas_idx])] else which.min(a$rr)
          cal <- if (length(feas_idx)) c(delta = unname(req[k, "delta"])) else c(delta = NA_real_)
          pred <- if (is.na(cal["delta"])) rep(NA_real_, nrow(a)) else vapply(m, function(mm) implied_rr(cal[["delta"]], mm, lambda, pi, rruy), 0)
          lambda_N <- lambda / (pi * req[, "delta"] + (1 - pi))
          sel[[length(sel) + 1L]] <- data.table(contrast = ct, loss_curve = curve, smoking_loss_rr = lambda,
            type_prevalence = pi, type_gh_pe_rr = rruy, age = a$age, baseline_loss = m, observed_rr = a$rr,
            required_type_differential_delta = req[, "delta"], feasible = req[, "feasible"],
            min_reachable_rr_under_mechanism = req[, "min_reachable_rr"],
            required_loss_rr_other_pregnancies = lambda_N,
            required_loss_rr_type_pregnancies = req[, "delta"] * lambda_N,
            smoking_would_have_to_reduce_loss_in_other_pregnancies = lambda_N < 1,
            calibration_age = a$age[k], calibrated_delta = unname(cal["delta"]),
            predicted_rr_under_calibrated_mechanism = pred)
        }
  }
  sel <- rbindlist(sel)
  sel[, interpretation := "Fixed-assumption left-truncation scenario under a true null within latent type; not an estimate of selection. A larger baseline loss makes the same mechanism produce a stronger inverse association, so under any loss curve that rises with age the mechanism predicts more, not less, apparent protection at older ages."]
  fwrite(sel, file.path(out, "S_selection_left_truncation_scenarios.csv"))
  # Compact view: calibrated prediction vs observed at selected ages, age_graded curve, lambda 1.23.
  compact <- sel[loss_curve == "age_graded" & smoking_loss_rr == 1.23 & age %in% c(18, 21, 25, 29, 32, 35, 40, 45),
    .(contrast, type_prevalence, type_gh_pe_rr, age, baseline_loss, observed_rr = round(observed_rr, 3),
      required_delta = round(required_type_differential_delta, 2), feasible,
      min_reachable_rr = round(min_reachable_rr_under_mechanism, 3),
      loss_rr_other = round(required_loss_rr_other_pregnancies, 3),
      predicted_rr_calibrated_at_min = round(predicted_rr_under_calibrated_mechanism, 3), calibration_age, calibrated_delta = round(calibrated_delta, 2))]
  fwrite(compact, file.path(out, "S_selection_left_truncation_compact.csv"))
  # ---- (3) Exposure misclassification, by age, from the age-specific 2x2 tables ----------
  # (3a) Non-differential within age: matrix correction of recorded exposure with Se/Sp
  #      applied equally among cases and non-cases; corrected crude RR by age.
  # (3b) Differential tipping: with specificity 1, the ratio Se(cases)/Se(non-cases) that
  #      makes the true RR exactly 1 equals the observed exposure prevalence ratio, cases vs
  #      non-cases, at that age (closed form; independent of the absolute sensitivity).
  mis <- tip <- list()
  for (ct in names(contrasts)) {
    s <- load_contrast(ct)$support
    w <- dcast(s, age ~ A, value.var = c("n", "events"))
    setnames(w, c("age", "n0", "n1", "e0", "e1"))
    w[, `:=`(C = e0 + e1, D = (n0 - e0) + (n1 - e1), Xc = e1, Xn = n1 - e1)]
    w[, `:=`(crude_rr = (e1 / n1) / (e0 / n0),
      exposure_prevalence_cases = Xc / C, exposure_prevalence_noncases = Xn / D)]
    w[, required_sensitivity_ratio_cases_to_noncases := exposure_prevalence_cases / exposure_prevalence_noncases]
    w[, contrast := ct]
    tip[[ct]] <- w[, .(contrast, age, cases = C, noncases = D, recorded_exposed_cases = Xc, recorded_exposed_noncases = Xn,
      crude_rr = round(crude_rr, 4), exposure_prevalence_cases = round(exposure_prevalence_cases, 5),
      exposure_prevalence_noncases = round(exposure_prevalence_noncases, 5),
      required_sensitivity_ratio_cases_to_noncases = round(required_sensitivity_ratio_cases_to_noncases, 4))]
    for (se in anchors$birth_certificate_smoking_ascertainment$sensitivity_scenarios)
      for (sp in anchors$birth_certificate_smoking_ascertainment$specificity_scenarios) {
        if (se + sp <= 1) next
        corr <- function(X, N) (X - (1 - sp) * N) / (se + sp - 1) # true exposed count
        Ec <- corr(w$Xc, w$C); En <- corr(w$Xn, w$D)
        valid <- Ec >= 0 & En >= 0 & Ec <= w$C & En <= w$D
        rr <- ifelse(valid, (Ec / (Ec + En)) / ((w$C - Ec) / ((w$C - Ec) + (w$D - En))), NA_real_)
        mis[[length(mis) + 1L]] <- data.table(contrast = ct, sensitivity = se, specificity = sp, age = w$age,
          crude_rr_recorded = w$crude_rr, corrected_crude_rr = rr, valid = valid,
          sign_preserved = ifelse(valid, sign(rr - 1) == sign(w$crude_rr - 1), NA))
      }
  }
  mis <- rbindlist(mis); tip <- rbindlist(tip)
  mis[, interpretation := "Non-differential recorded-exposure error corrected on the crude age-specific 2x2 table; the crude table is used because the correction is illustrative. Non-differential error cannot change the direction of an association; corrected estimates move away from the null in both directions."]
  tip[, interpretation := "With no false-positive smoking reports, the cases-to-non-cases sensitivity ratio that would make the true association exactly null equals the recorded exposure prevalence ratio. Values below 1 at younger ages and above 1 at older ages mean reporting error would have to change direction with maternal age to produce the observed pattern."]
  fwrite(mis, file.path(out, "S_exposure_misclassification_nondifferential.csv"))
  fwrite(tip, file.path(out, "S_exposure_misclassification_differential_tipping.csv"))
  # crossover of corrected crude RR under each scenario (first age at which corrected RR crosses 1 going upward)
  cross <- mis[valid == TRUE, .(recorded_crossover = { r <- crude_rr_recorded; a <- age; i <- which(diff(sign(r - 1)) > 0); if (length(i)) a[i[1]] + (1 - r[i[1]]) / (r[i[1] + 1] - r[i[1]]) else NA_real_ },
    corrected_crossover = { r <- corrected_crude_rr; a <- age; i <- which(diff(sign(r - 1)) > 0); if (length(i)) a[i[1]] + (1 - r[i[1]]) / (r[i[1] + 1] - r[i[1]]) else NA_real_ },
    min_corrected_rr = min(corrected_crude_rr), max_corrected_rr = max(corrected_crude_rr)),
    by = .(contrast, sensitivity, specificity)]
  fwrite(cross, file.path(out, "S_exposure_misclassification_crossover.csv"))
  # ---- (4) Reformat the existing outcome-misclassification tipping for tabulation --------
  prior <- "outputs/bias/age45"
  ot <- fread(file.path(prior, "outcome_sensitivity_null_tipping.csv")); inputs[file.path(prior, "outcome_sensitivity_null_tipping.csv")] <- sha(file.path(prior, "outcome_sensitivity_null_tipping.csv"))
  ot[, age := suppressWarnings(ifelse(target == "overall", NA_integer_, as.integer(sub("age_", "", target))))]
  fwrite(ot[, .(target, age, anchor, assumed_sensitivity_A0, assumed_common_specificity, required_sensitivity_A1,
    required_A1_to_A0_sensitivity_ratio, valid)], file.path(out, "S_outcome_sensitivity_tipping_by_age.csv"))
  cb <- fread(file.path(prior, "confounding_algebraic_benchmarks.csv")); inputs[file.path(prior, "confounding_algebraic_benchmarks.csv")] <- sha(file.path(prior, "confounding_algebraic_benchmarks.csv"))
  fwrite(cb, file.path(out, "S_confounding_benchmarks_primary.csv"))
  paths <- list.files(out, full.names = TRUE)
  jsonlite::write_json(list(status = "complete_fixed_assumption_supplementary_bias_analyses_not_causal",
    amendment = "SUPPLEMENTARY_AMENDMENT_2026-09-11", anchors = "config/bias_anchors.json",
    anchors_sha256 = sha("config/bias_anchors.json"),
    script_sha256 = sha("scripts/15_supplementary_bias.R"), inputs = as.list(inputs),
    outputs = data.frame(path = paths, sha256 = vapply(paths, sha, ""))),
    file.path(out, "receipt.json"), pretty = TRUE, auto_unbox = TRUE, digits = 15)
  message("Supplementary bias analyses written to ", out)
  invisible(TRUE)
}
if (sys.nframe() == 0L) main()
