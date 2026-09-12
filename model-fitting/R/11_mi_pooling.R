# MI pooling algebra only: no imputation, national data reads, or model execution.
# Source R/09_standardization.R before using the standardized-risk adapters.

mi_validate_covariance <- function(v, estimands, label) {
  if (!is.matrix(v) || !is.numeric(v) || any(!is.finite(v)) ||
      !identical(dim(v), c(length(estimands), length(estimands))) ||
      !identical(rownames(v), estimands) || !identical(colnames(v), estimands))
    stop(label, ": covariance must match the explicit estimand names/order.", call. = FALSE)
  if (any(diag(v) < 0)) stop(label, ": negative marginal variances are invalid.", call. = FALSE)
  scale <- max(abs(v), .Machine$double.xmin)
  if (max(abs(v - t(v))) > 1e-10 * scale)
    stop(label, ": covariance must be symmetric.", call. = FALSE)
  if (min(eigen((v + t(v)) / 2, symmetric = TRUE, only.values = TRUE)$values) < -1e-10 * scale)
    stop(label, ": covariance must be positive semidefinite.", call. = FALSE)
  invisible(TRUE)
}

pool_mi_joint <- function(estimates, covariances, df_complete = Inf, level = .95) {
  # Rows = independent proper imputations; columns = same estimands, already on
  # pooling scales. Singular covariance is permitted (e.g. RD = risk1 - risk0).
  if (!is.matrix(estimates) || !is.numeric(estimates) || any(!is.finite(estimates)) ||
      nrow(estimates) < 2L || ncol(estimates) < 1L || is.null(colnames(estimates)) ||
      anyDuplicated(colnames(estimates)) || any(!nzchar(colnames(estimates))))
    stop("Require a finite M-by-P estimate matrix, M >= 2, with unique estimand names.", call. = FALSE)
  if (!is.list(covariances) || length(covariances) != nrow(estimates))
    stop("Exactly one covariance matrix per imputation is required.", call. = FALSE)
  if (!is.numeric(df_complete) || length(df_complete) != 1L || is.na(df_complete) ||
      df_complete <= 0 || (!is.finite(df_complete) && df_complete != Inf))
    stop("df_complete must be a positive scalar or Inf, justified by the complete-data analysis.")
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1)
    stop("level must be strictly inside (0,1).")
  m <- nrow(estimates); estimands <- colnames(estimates)
  for (i in seq_len(m)) mi_validate_covariance(covariances[[i]], estimands, paste("Imputation", i))
  within <- Reduce(`+`, covariances) / m
  # Symmetrize machine precision only, after rejecting substantive asymmetry.
  within <- (within + t(within)) / 2
  if (any(diag(within) <= 0))
    stop("Positive average within-imputation variance required; degenerate targets need a separate rule.")
  pooled <- colMeans(estimates)
  centered <- sweep(estimates, 2L, pooled, "-")
  between <- crossprod(centered) / (m - 1L)
  total <- within + (1 + 1 / m) * between
  mi_validate_covariance(total, estimands, "Pooled total")
  lambda <- (1 + 1 / m) * diag(between) / diag(total)
  # Positive within variance implies lambda < 1 mathematically. Do not truncate
  # a numerically unresolved limit, negative covariance, or other invalid input.
  if (any(!is.finite(lambda) | lambda < 0 | lambda >= 1))
    stop("Numerically unresolved missing-information boundary; no truncation.")
  df_old <- ifelse(lambda == 0, Inf, (m - 1) / lambda^2)
  if (is.infinite(df_complete)) {
    df <- df_old
  } else {
    df_observed <- ((df_complete + 1) / (df_complete + 3)) * df_complete * (1 - lambda)
    df <- 1 / (1 / df_old + 1 / df_observed)
  }
  if (any(is.na(df) | df <= 0)) stop("Invalid pooled degrees of freedom.")
  se <- sqrt(diag(total)); critical <- stats::qt((1 + level) / 2, df = df)
  if (any(!is.finite(critical))) stop("Pointwise interval quantile is numerically unbounded.")
  relative_increase <- (1 + 1 / m) * diag(between) / diag(within)
  result <- data.frame(estimand = estimands, estimate = unname(pooled), se = unname(se),
    df = unname(df), lower = unname(pooled - critical * se), upper = unname(pooled + critical * se),
    within_variance = unname(diag(within)), between_variance = unname(diag(between)),
    lambda = unname(lambda), relative_increase = unname(relative_increase),
    fmi = unname((relative_increase + 2 / (df + 3)) / (relative_increase + 1)),
    monte_carlo_se_estimate = unname(sqrt(diag(between) / m)), row.names = NULL)
  structure(list(summary = result, pooled = pooled, within = within, between = between, total = total,
    estimates = estimates, imputations = m, level = level, df_complete = df_complete,
    uncertainty = "Rubin within/between covariance conditional on valid complete-data variances and proper, compatible MI.",
    interval_family = "Pointwise t intervals only; no simultaneous bands or multivariate test.",
    mcse_caveat = "sqrt(B/M) approximates simulation error of the pooled point estimate only for independent proper imputations."),
    class = "sep_mi_joint")
}

standardized_mi_components <- function(standardized, target_reference_id) {
  if (!inherits(standardized, "sep_standardized_risks")) stop("A standardized-risk object is required.")
  if (!is.character(target_reference_id) || length(target_reference_id) != 1L ||
      is.na(target_reference_id) || !nzchar(target_reference_id))
    stop("A nonempty target_reference_id identifying fixed cohort membership is required.")
  if (!exists("logit_risk_contrasts", mode = "function")) stop("Source R/09_standardization.R first.")
  tab <- standardized$summary; object <- standardized$object
  if (!is.data.frame(tab) || !nrow(tab) || !is.numeric(tab$age) || any(!is.finite(tab$age)) ||
      is.unsorted(tab$age, strictly = TRUE) || length(standardized$matrices) != nrow(tab))
    stop("Invalid age grid or stored prediction matrices.")
  if (!standardized$target %in% c("age_specific_empirical", "common_empirical"))
    stop("Unknown empirical standardization target.")
  if (!is.numeric(tab$target_n) || any(!is.finite(tab$target_n) | tab$target_n <= 0 |
                                    tab$target_n != as.integer(tab$target_n)))
    stop("Target counts must be positive integers.")
  if (standardized$target == "common_empirical" && length(unique(tab$target_n)) != 1L)
    stop("Common empirical target must have the same row count at every age.")
  if (!identical(standardized$uncertainty_target, object$uncertainty_target) ||
      !identical(object$uncertainty_target, "coefficient uncertainty conditional on empirical target covariates"))
    stop("Unsupported or inconsistent within-imputation uncertainty target.")
  scales <- c("risk0", "risk1", "rd", "log_rr")
  values <- gradients <- metadata <- vector("list", nrow(tab))
  for (i in seq_len(nrow(tab))) {
    mat <- standardized$matrices[[i]]
    if (!is.matrix(mat$X0) || !is.matrix(mat$X1)) stop("Stored target designs must be matrices.")
    if (nrow(mat$X0) != tab$target_n[i] || nrow(mat$X1) != tab$target_n[i])
      stop("Target counts differ from stored prediction matrices.")
    part <- logit_risk_contrasts(object$beta, object$vcov, mat$X0, mat$X1)
    basis <- object$age_spec$columns
    if (!all(c(basis, object$exposure) %in% colnames(mat$X0)))
      stop("Age basis or exposure main effect absent from the stored design.")
    expected_basis <- fixed_age_basis(rep(tab$age[i], tab$target_n[i]), object$age_spec)
    if (max(abs(mat$X0[, basis, drop = FALSE] - expected_basis)) > 1e-12 ||
        max(abs(mat$X1[, basis, drop = FALSE] - expected_basis)) > 1e-12 ||
        any(mat$X0[, object$exposure] != 0) || any(mat$X1[, object$exposure] != 1))
      stop("Age/exposure labels disagree with stored counterfactual designs.")
    # Recompute from per-imputation parameters and empirical target matrices.
    # Never predict from coefficients pooled across completed datasets.
    values[[i]] <- part$estimates[scales]
    gradients[[i]] <- part$gradient[scales, , drop = FALSE]
    labels <- paste0(scales, "@age=", format(tab$age[i], scientific = FALSE, trim = TRUE, digits = 15))
    names(values[[i]]) <- rownames(gradients[[i]]) <- labels
    metadata[[i]] <- data.frame(estimand = labels, age = tab$age[i], target_n = tab$target_n[i],
      measure = c("risk0", "risk1", "rd", "rr"), scale = c("identity", "identity", "identity", "log"),
      row.names = NULL)
  }
  estimate <- do.call(c, values); gradient <- do.call(rbind, gradients)
  if (anyDuplicated(names(estimate))) stop("Age labels are not uniquely representable.")
  covariance <- gradient %*% object$vcov %*% t(gradient)
  mi_validate_covariance(covariance, names(estimate), "Standardized joint vector")
  list(estimate = estimate, covariance = covariance, gradient = gradient,
    metadata = do.call(rbind, metadata),
    contract = list(target_reference_id = target_reference_id, target = standardized$target,
      ages = tab$age, target_n = tab$target_n, age_spec = object$age_spec,
      outcome = object$outcome, exposure = object$exposure, age = object$age,
      covariates = object$covariates, coefficient_names = names(object$beta),
      covariance_type = object$covariance_type, fit_n = stats::nobs(object$fit),
      uncertainty_target = object$uncertainty_target))
}

pool_standardized_mi <- function(components, df_complete = Inf, level = .95) {
  if (!is.list(components) || length(components) < 2L)
    stop("At least two standardized per-imputation components are required; M=1 cannot estimate between variance.")
  first <- components[[1]]
  required <- c("estimate", "covariance", "gradient", "metadata", "contract")
  for (i in seq_along(components)) {
    item <- components[[i]]
    if (!is.list(item) || !all(required %in% names(item))) stop("Malformed per-imputation components.")
    if (!identical(item$contract, first$contract) || !identical(item$metadata, first$metadata))
      stop("Imputations must have identical estimand, cohort target, age basis, model and uncertainty contracts.")
    if (!identical(names(item$estimate), names(first$estimate))) stop("Per-imputation estimate order differs.")
  }
  pooled <- pool_mi_joint(do.call(rbind, lapply(components, `[[`, "estimate")),
                          lapply(components, `[[`, "covariance"), df_complete, level)
  tab <- cbind(first$metadata, pooled$summary[setdiff(names(pooled$summary), "estimand")])
  tab$pooling_scale_estimate <- tab$estimate
  tab$pooling_scale_lower <- tab$lower; tab$pooling_scale_upper <- tab$upper
  names(tab)[names(tab) == "se"] <- "pooling_scale_se"
  ratio <- tab$scale == "log"
  tab$estimate[ratio] <- exp(tab$estimate[ratio]); tab$lower[ratio] <- exp(tab$lower[ratio])
  tab$upper[ratio] <- exp(tab$upper[ratio])
  if (any(!is.finite(as.matrix(tab[c("estimate", "lower", "upper")]))))
    stop("Back-transformed pointwise intervals are numerically unbounded; no clipping.")
  tab$outside_parameter_space <- ifelse(tab$measure %in% c("risk0", "risk1"),
    tab$lower < 0 | tab$upper > 1, ifelse(tab$measure == "rd", tab$lower < -1 | tab$upper > 1, FALSE))
  pooled$summary <- tab
  pooled$contract <- first$contract
  pooled$covariance_scale <- "Joint risk0/risk1/RD identity scales and log(RR), in explicit estimand order."
  pooled$uncertainty_target <- paste("Each completed-data variance conditions on its empirical covariate target;",
    "between-imputation variability represents uncertainty in missing values, including the completed target.",
    "This does not add superpopulation covariate-distribution variance, recover excluded records, or correct selection.")
  pooled$membership_caveat <- paste("Matching target_reference_id/counts is an explicit caller contract, not a row-ID audit;",
    "upstream checks must verify identical cohort membership and age assignment across imputations.")
  pooled$scale_caveat <- paste("RR is exp(mean(log(RR_m))), not the ratio of pooled risks or an arithmetic mean of RRs.",
    "Risks and RDs use identity-scale t intervals; out-of-range limits are flagged, never clipped.")
  class(pooled) <- c("sep_standardized_mi", class(pooled))
  pooled
}
