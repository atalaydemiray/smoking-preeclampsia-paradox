# Deterministic sensitivity algebra only. No model/data reading, priors, sampling,
# automatic causal correction, OR reporting, or multiplication of separate biases.

pb_scalar <- function(x, label, lower = -Inf, upper = Inf, open_lower = FALSE,
                      open_upper = FALSE) {
  if (!is.numeric(x) || !is.null(dim(x)) || length(x) != 1L || !is.finite(x) ||
      x < lower || x > upper || (open_lower && x == lower) ||
      (open_upper && x == upper)) stop("Invalid ", label, "; supply one finite supported number.")
  unname(x)
}

pb_target <- function(target_id) {
  if (!is.character(target_id) || length(target_id) != 1L || is.na(target_id) ||
      !nzchar(trimws(target_id))) stop("Explicit fixed target_id required.")
  target_id
}

pb_rr_scope <- function(scope) {
  # Classic bounds are conditional on C. Applying their numeric transformation
  # to our marginal standardized RR is a BENCHMARK, not a proven marginal bound.
  if (length(scope) != 1L || !is.character(scope) || is.na(scope) ||
      !scope %in% c("conditional_stratum", "marginal_rr_benchmark"))
    stop("Specify conditional_stratum or marginal_rr_benchmark explicitly.")
  scope
}

pb_evalue_above_one <- function(r) {
  r <- pb_scalar(r, "oriented RR", 1)
  ans <- r + sqrt(r) * sqrt(r - 1)
  if (!is.finite(ans)) stop("E-value exceeds finite numerical support; no truncation.")
  ans
}

primary_evalue_rr <- function(rr, scope, target_id, lower = NULL, upper = NULL) {
  rr <- pb_scalar(rr, "risk ratio", 0, open_lower = TRUE)
  scope <- pb_rr_scope(scope); target_id <- pb_target(target_id)
  if (xor(is.null(lower), is.null(upper))) stop("Supply both confidence limits or neither.")
  ci_value <- closest <- NA_real_
  if (!is.null(lower)) {
    lower <- pb_scalar(lower, "lower RR limit", 0, open_lower = TRUE)
    upper <- pb_scalar(upper, "upper RR limit", 0, open_lower = TRUE)
    if (lower > rr || upper < rr || lower > upper) stop("RR confidence limits do not contain the estimate.")
    closest <- if (lower <= 1 && upper >= 1) 1 else if (rr < 1) upper else lower
    ci_value <- pb_evalue_above_one(if (closest < 1) 1 / closest else closest)
  }
  oriented <- if (rr < 1) 1 / rr else rr
  list(estimate_rr = rr, rr_for_null_benchmark = oriented,
    evalue_point = pb_evalue_above_one(oriented), evalue_ci_limit = ci_value,
    ci_limit_nearest_null = closest, exposure_reversed = rr < 1,
    confounder_prevalence_ratio_orientation = if (rr < 1)
      "max_u P(U=u|A=0,C)/P(U=u|A=1,C)" else "max_u P(U=u|A=1,C)/P(U=u|A=0,C)",
    scope = scope, target_id = target_id,
    is_validated_marginal_causal_bound = FALSE,
    interpretation = if (scope == "conditional_stratum")
      "Conditional-stratum equal-strength unmeasured-confounding threshold under the usual causal identification assumptions apart from U; not evidence those assumptions hold." else
      "Algebraic RR robustness benchmark only; not a validated bound for the empirical marginal causal target.",
    limitations = c("No measurement, selection, model-misspecification or CIG0/CHTN-membership correction.",
      "The CI-limit E-value is a threshold, not a confidence interval for the E-value.",
      "Uses genuine risk ratios without rare-outcome or OR-to-RR approximations."))
}

primary_confounding_bound <- function(rr, rr_au, rr_uy, scope, target_id) {
  ev <- primary_evalue_rr(rr, scope, target_id)
  a <- pb_scalar(rr_au, "exposure-confounder prevalence ratio", 1)
  b <- pb_scalar(rr_uy, "maximum confounder-outcome risk ratio", 1)
  # Equivalent to a*b/(a+b-1), without overflowing the product.
  factor <- 1 / (1/a + 1/b - (1/a)/b)
  bound <- if (ev$exposure_reversed) rr * factor else rr / factor
  if (!is.finite(factor) || !is.finite(bound) || bound <= 0)
    stop("Confounding-bound result exceeds numerical support.")
  list(rr = rr, rr_au = a, rr_uy = b, bounding_factor = factor,
    directional_rr_bound_or_benchmark = bound,
    direction = if (ev$exposure_reversed) "upper" else "lower",
    null_not_excluded_by_this_bound = factor >= ev$rr_for_null_benchmark,
    exposure_reversed = ev$exposure_reversed, scope = ev$scope, target_id = ev$target_id,
    interpretation = ev$interpretation,
    parameter_definition = "rr_uy is the maximum risk contrast across U within either exposure arm and fixed measured C; rr_au follows the displayed exposure orientation.",
    limitations = c(ev$limitations, "Worst-case bound, not an estimated bias factor or point correction; parameters are not validated by this function."))
}

primary_confounding_tipping <- function(rr, rr_au, scope, target_id) {
  ev <- primary_evalue_rr(rr, scope, target_id)
  a <- pb_scalar(rr_au, "exposure-confounder prevalence ratio", 1)
  r <- ev$rr_for_null_benchmark
  if (r == 1) return(list(status = "already_at_null", required_rr_uy = 1,
    target_id = ev$target_id, scope = ev$scope))
  if (a <= r) return(list(status = "no_finite_rr_uy_reaches_null", required_rr_uy = NA_real_,
    target_id = ev$target_id, scope = ev$scope))
  required <- r * ((a - 1) / (a - r))
  if (!is.finite(required)) stop("Tipping strength exceeds finite numerical support.")
  list(status = "finite_threshold", required_rr_uy = required,
    target_id = ev$target_id, scope = ev$scope,
    interpretation = "Threshold for the worst-case bound, not a demonstrated confounder.")
}

pb_selection_contract <- function(selection_scope, target_id, outcome_defined_for_nonselected,
                                  risk_scope) {
  if (length(selection_scope) != 1L || !is.character(selection_scope) || is.na(selection_scope) ||
      !selection_scope %in% c("baseline_complete_case", "eligible_record_capture"))
    stop("Selection scope must be baseline_complete_case or eligible_record_capture; early pregnancy loss is not implemented.")
  if (!identical(outcome_defined_for_nonselected, TRUE))
    stop("The SAME binary outcome must be defined in selected and nonselected target records.")
  if (!identical(risk_scope, "single_covariate_stratum"))
    stop("Selection algebra requires stratum-specific risks; do not insert already-standardized risks.")
  list(selection_scope = selection_scope, target_id = pb_target(target_id), risk_scope = risk_scope)
}

pb_selection_probabilities <- function(selection) {
  states <- c("A0_Y0", "A0_Y1", "A1_Y0", "A1_Y1")
  if (!is.numeric(selection) || !is.null(dim(selection)) || length(selection) != 4L ||
      !identical(names(selection), states) || any(!is.finite(selection)) ||
      any(selection <= 0 | selection > 1))
    stop("Require P(S=1|A,Y,C) in exact A0_Y0,A0_Y1,A1_Y0,A1_Y1 order, each in (0,1].")
  selection
}

primary_selection_scenario <- function(risk0_selected, risk1_selected, selection,
    selection_scope, target_id, outcome_defined_for_nonselected, risk_scope,
    observed_selection_share = NULL) {
  contract <- pb_selection_contract(selection_scope, target_id, outcome_defined_for_nonselected, risk_scope)
  q <- c(A0 = pb_scalar(risk0_selected, "selected risk0", 0, 1, TRUE, TRUE),
         A1 = pb_scalar(risk1_selected, "selected risk1", 0, 1, TRUE, TRUE))
  s <- pb_selection_probabilities(selection)
  s0 <- unname(s[c(1,3)]); s1 <- unname(s[c(2,4)])
  # Bayes: odds(Y=1|A,S=1,C) = odds(Y=1|A,C)*s1/s0.
  # Log calculation is stable; no rare-outcome approximation and no clipping.
  p <- stats::plogis(stats::qlogis(q) - log(s1) + log(s0))
  if (any(!is.finite(p) | p <= 0 | p >= 1)) stop("Selection scenario gives numerically unresolved boundary risks.")
  selected_share <- (1-p)*s0 + p*s1
  if (!is.null(observed_selection_share)) {
    if (!is.numeric(observed_selection_share) || !is.null(dim(observed_selection_share)) ||
        !identical(names(observed_selection_share), c("A0","A1")) ||
        any(!is.finite(observed_selection_share)) || any(observed_selection_share <= 0 | observed_selection_share > 1))
      stop("Observed selection shares must be named A0,A1 probabilities in (0,1].")
    if (any(abs(observed_selection_share-selected_share) > 1e-10))
      stop("Scenario is incompatible with the supplied arm-specific selection denominators.")
  }
  list(status = "valid_assumption_dependent_scenario", risk = setNames(p,c("risk0","risk1")),
    rd = unname(p[2]-p[1]), rr = unname(p[2]/p[1]),
    selected_risk = q, implied_selection_share = selected_share,
    case_to_noncase_selection_ratio = setNames(s1/s0,c("A0","A1")),
    selection = s, contract = contract, empirical_selection_share_checked = !is.null(observed_selection_share),
    interpretation = "Preselection-target stratum association (S unrestricted) under the supplied selection mechanism; not a causal correction.",
    risk_denominator = "P(Y=1|A,C) in all eligible target records, not P(Y=1|A,S=0,C) among excluded records.",
    limitations = c("No confounding or measurement correction; recorded CIG0/CHTN target membership is unchanged.",
      "Selection probabilities and the distribution of C in a target are not identified by this formula.",
      "Standardize stratum results using an explicit target distribution; averaging or inverting already-standardized risks is not justified.",
      "Not applicable to future GH/PE after an early loss; no conception-to-observation weights are estimated."))
}

primary_selection_tipping <- function(risk0_selected, risk1_selected,
    selection_A0_Y0, selection_A0_Y1, selection_A1_Y0,
    selection_scope, target_id, outcome_defined_for_nonselected, risk_scope,
    observed_selection_share = NULL) {
  pb_selection_contract(selection_scope, target_id, outcome_defined_for_nonselected, risk_scope)
  q0 <- pb_scalar(risk0_selected, "selected risk0", 0, 1, TRUE, TRUE)
  q1 <- pb_scalar(risk1_selected, "selected risk1", 0, 1, TRUE, TRUE)
  s00 <- pb_scalar(selection_A0_Y0, "selection_A0_Y0", 0, 1, TRUE)
  s01 <- pb_scalar(selection_A0_Y1, "selection_A0_Y1", 0, 1, TRUE)
  s10 <- pb_scalar(selection_A1_Y0, "selection_A1_Y0", 0, 1, TRUE)
  # At equal unselected risks, the between-arm ratio of case/noncase selection
  # ratios equals exp(logit(q1)-logit(q0)). It is NOT generally the observed RR.
  log_ratio <- stats::qlogis(q1)-stats::qlogis(q0)
  s11 <- exp(log(s10)+log(s01)-log(s00)+log_ratio)
  if (!is.finite(s11) || s11 <= 0 || s11 > 1)
    stop("No feasible null-tipping probability for the supplied other selection probabilities.")
  s <- setNames(c(s00,s01,s10,s11),c("A0_Y0","A0_Y1","A1_Y0","A1_Y1"))
  ans <- primary_selection_scenario(q0,q1,s,selection_scope,target_id,
    outcome_defined_for_nonselected,risk_scope,observed_selection_share)
  if (abs(ans$rd) > 1e-12) stop("Null tipping failed numerical verification.")
  ans$required_selection_A1_Y1 <- s11
  ans$required_between_arm_selection_ratio <- exp(log_ratio)
  ans
}

primary_outcome_measurement_scenario <- function(risk0_recorded, risk1_recorded,
    sensitivity, specificity, target_id, constant_error_within_each_arm,
    recorded_exposure_treated_as_true) {
  pb_target(target_id)
  if (!identical(constant_error_within_each_arm, TRUE) ||
      !identical(recorded_exposure_treated_as_true, TRUE))
    stop("Explicit constant-within-arm outcome error and error-free A scenario required; otherwise use a conditional joint model.")
  if (!exists("correct_outcome_risk", mode="function")) stop("Source frozen R/15_bias_helpers.R first.")
  for (x in list(sensitivity,specificity))
    if (!is.numeric(x) || !is.null(dim(x)) || !identical(names(x),c("A0","A1")) ||
        any(!is.finite(x)) || any(x<0 | x>1)) stop("Sensitivity/specificity require exact names A0,A1 and probability support.")
  q <- c(pb_scalar(risk0_recorded,"risk0_recorded",0,1),pb_scalar(risk1_recorded,"risk1_recorded",0,1))
  p <- correct_outcome_risk(q,sensitivity,specificity) # Incompatibility throws; no clipping.
  list(status=if(p[1]==0) "valid_reference_risk_zero" else "valid_assumption_dependent_scenario",
    risk=setNames(p,c("risk0","risk1")),rd=unname(p[2]-p[1]),rr=if(p[1]>0) unname(p[2]/p[1]) else NA_real_,
    target_id=target_id, interpretation="Outcome-measurement scenario association in the same recorded-eligibility target, not a causal correction.",
    limitations=c("Constant Se/Sp within each exposure arm across the entire target is assumed, not validated.",
      "Under this homogeneity assumption the linear outcome inversion commutes with a common target standardization.",
      "No CIG0, T1, chronic-hypertension membership, confounding or selection correction; no uncertainty interval generated."))
}

primary_validation_anchors <- function(evidence) {
  if (!is.list(evidence) || !identical(evidence$status,"candidate_external_evidence_not_locked_bias_parameters") ||
      !is.list(evidence$samples) || !length(evidence$samples)) stop("Reviewed candidate evidence object required.")
  rows <- lapply(evidence$samples,function(z) {
    counts <- c(TP=z$TP,FN=z$FN,FP=z$FP,TN=z$TN)
    if (length(counts)!=4L || !is.numeric(counts) || any(!is.finite(counts)) ||
        any(counts<0 | counts!=floor(counts)) || sum(counts)!=z$valid_n ||
        sum(counts[c("TP","FN")])<=0 || sum(counts[c("FP","TN")])<=0)
      stop("Invalid validation count denominator.")
    data.frame(id=z$id,birth_period=z$birth_period,valid_n=z$valid_n,
      TP=z$TP,FN=z$FN,FP=z$FP,TN=z$TN,
      sensitivity=z$TP/(z$TP+z$FN),specificity=z$TN/(z$TN+z$FP),
      source_url=z$source_url,table=z$table,sampling=z$sampling,
      evidence_status="Separate selected-hospital empirical anchor; not a national prior or plausible-range endpoint.",
      stringsAsFactors=FALSE)
  })
  out <- do.call(rbind,rows)
  if(anyDuplicated(out$id))stop("Duplicate validation sample identifiers.")
  out
}
