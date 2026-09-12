# Outcome-misclassification helpers. No clipping, fitting, or inferred parameters.
# Apply source/covariate-specific corrections BEFORE standardization if errors
# vary within the population being standardized. Homogeneous correction of an
# already-standardized risk is an explicit scenario assumption, not validation.

.qba_probability <- function(x, label) {
  if (!is.numeric(x) || !is.null(dim(x)) || anyNA(x) ||
      any(!is.finite(x)) || any(x < 0 | x > 1)) {
    stop(label, " must contain finite probabilities in [0, 1] with no missing values.",
         call. = FALSE)
  }
  x
}

.qba_expand <- function(x, n, label) {
  if (length(x) == n) return(x)
  if (length(x) == 1L) return(rep(x, n))
  stop(label, " must have length 1 or match the number of observed risks.",
       call. = FALSE)
}

correct_outcome_risk <- function(p_observed, sensitivity, specificity) {
  p <- .qba_probability(p_observed, "p_observed")
  se <- .qba_expand(.qba_probability(sensitivity, "sensitivity"), length(p), "sensitivity")
  sp <- .qba_expand(.qba_probability(specificity, "specificity"), length(p), "specificity")
  denominator <- se + sp - 1
  if (any(denominator <= 0)) {
    stop("Outcome correction requires sensitivity + specificity > 1.", call. = FALSE)
  }
  corrected <- (p + sp - 1) / denominator
  if (any(!is.finite(corrected) | corrected < 0 | corrected > 1)) {
    bad <- which(!is.finite(corrected) | corrected < 0 | corrected > 1)
    stop("Incompatible observed risk and error parameters at rows ",
         paste(head(bad, 8L), collapse = ", "), "; corrected probabilities leave [0, 1].",
         call. = FALSE)
  }
  corrected
}

qba_marginal_contrasts <- function(p_exposed, p_reference,
                                  sensitivity_exposed, specificity_exposed,
                                  sensitivity_reference, specificity_reference) {
  if (length(p_exposed) != length(p_reference)) {
    stop("Exposed and reference standardized risks must have equal lengths.", call. = FALSE)
  }
  pe <- correct_outcome_risk(p_exposed, sensitivity_exposed, specificity_exposed)
  pr <- correct_outcome_risk(p_reference, sensitivity_reference, specificity_reference)
  if (any(pe <= 0 | pe >= 1 | pr <= 0 | pr >= 1)) {
    stop("Marginal OR reporting requires corrected risks strictly between 0 and 1; no clipping is used.",
         call. = FALSE)
  }
  data.frame(corrected_risk_exposed = pe, corrected_risk_reference = pr,
             marginal_risk_difference = pe - pr,
             marginal_risk_ratio = pe / pr,
             marginal_odds_ratio = (pe / (1 - pe)) / (pr / (1 - pr)),
             estimand = rep("marginal contrasts from standardized risks", length(pe)),
             stringsAsFactors = FALSE)
}
