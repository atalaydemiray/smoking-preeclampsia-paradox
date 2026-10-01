# Synthetic-tested estimator foundation; not a national-analysis entry point.
# All uncertainty below conditions on the supplied empirical target distribution.

sep_numeric <- function(x, label, allow_empty = FALSE) {
  if (!is.numeric(x) || (!allow_empty && !length(x)) || any(!is.finite(x)))
    stop(label, " must be a finite numeric vector.", call. = FALSE)
  invisible(x)
}

lock_age_basis <- function(age, knots = NULL, boundaries = c(15, 44),
                           probs = c(.1, .5, .9)) {
  sep_numeric(age, "age")
  sep_numeric(boundaries, "boundaries")
  if (length(boundaries) != 2L || boundaries[1] >= boundaries[2])
    stop("Two increasing age boundaries are required.", call. = FALSE)
  if (any(age < boundaries[1] | age > boundaries[2]))
    stop("Ages outside locked boundaries are not permitted.", call. = FALSE)
  if (is.null(knots)) {
    sep_numeric(probs, "probs")
    if (any(probs <= 0 | probs >= 1) || is.unsorted(probs, strictly = TRUE))
      stop("Knot probabilities must be strictly increasing inside (0,1).", call. = FALSE)
    knots <- as.numeric(stats::quantile(age, probs, names = FALSE, type = 7))
  }
  sep_numeric(knots, "knots", allow_empty = TRUE)
  if (is.unsorted(knots, strictly = TRUE) || any(knots <= boundaries[1] | knots >= boundaries[2]))
    stop("Knots must be unique, increasing and strictly within boundaries.", call. = FALSE)
  structure(list(knots = knots, boundaries = boundaries,
                 columns = paste0("sep_age_basis_", seq_len(length(knots) + 1L))),
            class = "sep_age_spec")
}

fixed_age_basis <- function(age, spec) {
  if (!inherits(spec, "sep_age_spec")) stop("A locked age specification is required.")
  sep_numeric(age, "age")
  if (any(age < spec$boundaries[1] | age > spec$boundaries[2]))
    stop("Ages outside locked boundaries are not permitted.", call. = FALSE)
  ans <- splines::ns(age, knots = spec$knots, Boundary.knots = spec$boundaries,
                    intercept = FALSE)
  colnames(ans) <- spec$columns
  ans
}

sep_check_data <- function(data, columns) {
  if (!is.data.frame(data) || !nrow(data)) stop("A nonempty data.frame is required.")
  if (anyDuplicated(names(data)) || !all(columns %in% names(data)))
    stop("Missing required or duplicated data columns.", call. = FALSE)
  if (any(!stats::complete.cases(data[columns])))
    stop("Missing analysis values: resolve eligibility/imputation explicitly.", call. = FALSE)
  for (name in columns) {
    if (is.numeric(data[[name]]) && any(!is.finite(data[[name]])))
      stop("Nonfinite analysis values in ", name, call. = FALSE)
  }
}

fit_standardized_logit <- function(data, outcome, exposure, age, covariates = character(),
                                   age_spec, covariance = c("model", "HC0")) {
  covariance <- match.arg(covariance)
  columns <- c(outcome, exposure, age, covariates)
  if (anyDuplicated(columns) || any(make.names(columns) != columns))
    stop("Distinct syntactic column names are required.", call. = FALSE)
  sep_check_data(data, columns)
  if (!is.numeric(data[[outcome]]) || !all(data[[outcome]] %in% c(0, 1)) ||
      length(unique(data[[outcome]])) != 2L)
    stop("Outcome must contain both numeric binary states.", call. = FALSE)
  if (!is.numeric(data[[exposure]]) || !all(data[[exposure]] %in% c(0, 1)) ||
      length(unique(data[[exposure]])) != 2L)
    stop("Exposure must contain both numeric binary states.", call. = FALSE)
  if (any(age_spec$columns %in% names(data))) stop("Reserved age-basis column collision.")
  fitted_data <- data[columns]
  fitted_data[age_spec$columns] <- fixed_age_basis(data[[age]], age_spec)
  rhs <- c(paste0(exposure, " * (", paste(age_spec$columns, collapse = " + "), ")"), covariates)
  formula <- stats::reformulate(rhs, response = outcome)
  warnings <- character()
  fit <- withCallingHandlers(stats::glm(formula, data = fitted_data,
                                       family = stats::binomial(), na.action = stats::na.fail,
                                       model = TRUE, x = TRUE, y = TRUE,
                                       control = stats::glm.control(epsilon = 1e-12, maxit = 100L)),
                             warning = function(w) {
                               warnings <<- c(warnings, conditionMessage(w))
                               invokeRestart("muffleWarning")
                             })
  if (!isTRUE(fit$converged) || fit$rank != length(stats::coef(fit)) ||
      any(!is.finite(stats::coef(fit))))
    stop("Nonconverged or rank-deficient outcome model.", call. = FALSE)
  if (length(warnings))
    stop("Outcome model warning requires investigation: ", paste(unique(warnings), collapse = "; "),
         call. = FALSE)
  if (any(fit$fitted.values < 1e-10 | fit$fitted.values > 1 - 1e-10))
    stop("Near-boundary fitted probabilities require separation diagnostics.", call. = FALSE)
  if (covariance == "HC0" && !requireNamespace("sandwich", quietly = TRUE))
    stop("HC0 requires an already installed sandwich package; no installation is attempted.")
  vc <- if (covariance == "model") stats::vcov(fit) else sandwich::vcovHC(fit, type = "HC0")
  structure(list(fit = fit, beta = stats::coef(fit), vcov = vc,
                 outcome = outcome, exposure = exposure, age = age, covariates = covariates,
                 age_spec = age_spec, covariance_type = covariance,
                 uncertainty_target = "coefficient uncertainty conditional on empirical target covariates"),
            class = "sep_standardization_fit")
}

sep_validate_parameters <- function(beta, vcov) {
  sep_numeric(beta, "beta")
  if (is.null(names(beta)) || anyDuplicated(names(beta))) stop("Named coefficients are required.")
  if (!is.matrix(vcov) || !is.numeric(vcov) || any(!is.finite(vcov)) ||
      !identical(dim(vcov), c(length(beta), length(beta))) ||
      !identical(rownames(vcov), names(beta)) || !identical(colnames(vcov), names(beta)))
    stop("Covariance dimensions/names must match coefficients in order.", call. = FALSE)
  tolerance <- 1e-10 * max(1, max(abs(vcov)))
  if (max(abs(vcov - t(vcov))) > tolerance) stop("Covariance must be symmetric.")
  ev <- eigen((vcov + t(vcov)) / 2, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) < -tolerance) stop("Covariance must be positive semidefinite.")
  invisible(TRUE)
}

logit_risk_contrasts <- function(beta, vcov, X0, X1) {
  sep_validate_parameters(beta, vcov)
  for (X in list(X0, X1)) {
    if (!is.matrix(X) || !is.numeric(X) || !nrow(X) || any(!is.finite(X)) ||
        !identical(colnames(X), names(beta)))
      stop("Nonempty finite design matrices must match named coefficients.", call. = FALSE)
  }
  if (nrow(X0) != nrow(X1)) stop("Both exposures must use the same target rows.")
  p0 <- stats::plogis(drop(X0 %*% beta)); p1 <- stats::plogis(drop(X1 %*% beta))
  r0 <- mean(p0); r1 <- mean(p1)
  if (r0 <= 0 || r0 >= 1 || r1 <= 0 || r1 >= 1)
    stop("Boundary standardized risks cannot support log/logit inference; no clipping.")
  g0 <- colMeans(X0 * (p0 * (1 - p0)))
  g1 <- colMeans(X1 * (p1 * (1 - p1)))
  glr <- g1 / r1 - g0 / r0
  estimates <- c(risk0 = r0, risk1 = r1, rd = r1 - r0,
                 rr = r1 / r0, log_rr = log(r1) - log(r0))
  gradient <- rbind(risk0 = g0, risk1 = g1, rd = g1 - g0,
                    rr = (r1 / r0) * glr, log_rr = glr)
  variance <- gradient %*% vcov %*% t(gradient)
  list(estimates = estimates, gradient = gradient, covariance = variance,
       se = sqrt(pmax(diag(variance), 0)))
}

sep_design <- function(object, target_data, age_value, exposure_value) {
  sep_check_data(target_data, c(object$age, object$covariates))
  newdata <- target_data[c(object$age, object$covariates)]
  newdata[[object$age]] <- age_value
  newdata[[object$exposure]] <- exposure_value
  newdata[object$age_spec$columns] <- fixed_age_basis(newdata[[object$age]], object$age_spec)
  X <- stats::model.matrix(stats::delete.response(stats::terms(object$fit)), data = newdata,
                           contrasts.arg = object$fit$contrasts, xlev = object$fit$xlevels)
  if (!identical(colnames(X), names(object$beta))) stop("Prediction design has changed.")
  X
}

standardize_logit <- function(object, target_data, ages, target, level = .95) {
  if (!inherits(object, "sep_standardization_fit")) stop("A standardization fit is required.")
  target <- match.arg(target, c("age_specific_empirical", "common_empirical"))
  sep_numeric(ages, "ages")
  if (is.unsorted(ages, strictly = TRUE)) stop("Ages must be unique and increasing.")
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1)
    stop("level must lie strictly inside (0,1).")
  sep_check_data(target_data, c(object$age, object$covariates))
  fixed_age_basis(ages, object$age_spec)
  matrices <- results <- vector("list", length(ages))
  z <- stats::qnorm((1 + level) / 2)
  for (i in seq_along(ages)) {
    d <- if (target == "age_specific_empirical") target_data[target_data[[object$age]] == ages[i], , drop = FALSE] else target_data
    if (!nrow(d)) stop("No empirical target rows at age ", ages[i], "; no interpolation or fallback.")
    X0 <- sep_design(object, d, ages[i], 0); X1 <- sep_design(object, d, ages[i], 1)
    est <- logit_risk_contrasts(object$beta, object$vcov, X0, X1)
    matrices[[i]] <- list(X0 = X0, X1 = X1)
    e <- est$estimates; s <- est$se
    risk_ci <- function(p, se) stats::plogis(stats::qlogis(p) + c(-1, 1) * z * se / (p * (1 - p)))
    ci0 <- risk_ci(e["risk0"], s["risk0"]); ci1 <- risk_ci(e["risk1"], s["risk1"])
    results[[i]] <- data.frame(age = ages[i], target_n = nrow(d), risk0 = e["risk0"],
      risk1 = e["risk1"], rd = e["rd"], rr = e["rr"], se_risk0 = s["risk0"],
      se_risk1 = s["risk1"], se_rd = s["rd"], se_log_rr = s["log_rr"],
      risk0_lower = ci0[1], risk0_upper = ci0[2], risk1_lower = ci1[1], risk1_upper = ci1[2],
      rd_lower = e["rd"] - z * s["rd"], rd_upper = e["rd"] + z * s["rd"],
      rr_lower = exp(e["log_rr"] - z * s["log_rr"]), rr_upper = exp(e["log_rr"] + z * s["log_rr"]),
      row.names = NULL)
  }
  ans <- do.call(rbind, results)
  structure(list(summary = ans, matrices = matrices, object = object, target = target,
                 level = level, uncertainty_target = object$uncertainty_target),
            class = "sep_standardized_risks")
}

grid_crossings <- function(ages, rd, tolerance = 1e-10) {
  sep_numeric(ages, "ages"); sep_numeric(rd, "rd")
  if (length(ages) != length(rd) || is.unsorted(ages, strictly = TRUE))
    stop("Crossing ages and risks must match and ages must increase.")
  if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0)
    stop("tolerance must be finite and nonnegative.")
  sign_rd <- ifelse(abs(rd) <= tolerance, 0, sign(rd))
  zeros <- ages[sign_rd == 0]
  adjacent <- if (length(rd) > 1L) seq_len(length(rd) - 1L) else integer()
  change <- adjacent[sign_rd[adjacent] * sign_rd[adjacent + 1L] < 0]
  brackets <- data.frame(lower_age = ages[change], upper_age = ages[change + 1L])
  flat <- any(sign_rd[adjacent] == 0 & sign_rd[adjacent + 1L] == 0)
  count <- length(zeros) + nrow(brackets)
  status <- if (all(sign_rd == 0)) "all_zero" else if (flat) "flat_zero_segment" else if (!count)
    "no_grid_crossing" else if (count == 1L) "single_grid_crossing" else "multiple_grid_crossings"
  list(status = status, zero_ages = zeros, sign_change_brackets = brackets,
       interpretation = "Grid crossings only; no interpolated continuous-age root or tangent-crossing claim.")
}

sep_local_seed <- function(seed, expression) {
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed)) stop("A finite numeric seed is required.")
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit(if (had_seed) assign(".Random.seed", previous, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  force(expression)
}

coefficient_bands <- function(standardized, draws = 1000L, seed = 1L) {
  if (!inherits(standardized, "sep_standardized_risks")) stop("Standardized risks required.")
  if (length(draws) != 1L || !is.numeric(draws) || !is.finite(draws) || draws < 100 || draws != as.integer(draws))
    stop("At least 100 integer coefficient draws are required.")
  obj <- standardized$object
  sep_validate_parameters(obj$beta, obj$vcov)
  eigen_v <- eigen((obj$vcov + t(obj$vcov)) / 2, symmetric = TRUE)
  # Only floating-point negative eigenvalues tolerated by validation are set to zero.
  factor_v <- eigen_v$vectors %*% diag(sqrt(pmax(eigen_v$values, 0)), nrow = length(obj$beta))
  beta_draw <- sep_local_seed(seed, sweep(factor_v %*% matrix(stats::rnorm(length(obj$beta) * draws),
                                                           nrow = length(obj$beta)), 1, obj$beta, "+"))
  n_age <- nrow(standardized$summary)
  values <- array(NA_real_, c(n_age, draws, 4L), dimnames = list(NULL, NULL, c("risk0", "risk1", "rd", "rr")))
  for (i in seq_len(n_age)) {
    mats <- standardized$matrices[[i]]
    r0 <- colMeans(stats::plogis(mats$X0 %*% beta_draw))
    r1 <- colMeans(stats::plogis(mats$X1 %*% beta_draw))
    if (any(r0 <= 0 | r0 >= 1 | r1 <= 0 | r1 >= 1))
      stop("Coefficient draws produced boundary risks; no clipping.")
    values[i, , ] <- cbind(r0, r1, r1 - r0, r1 / r0)
  }
  tab <- standardized$summary; alpha <- 1 - standardized$level
  bands <- vector("list", 4L)
  for (j in 1:4) {
    measure <- dimnames(values)[[3]][j]
    v <- matrix(values[, , j], nrow = n_age, ncol = draws)
    point <- tab[[measure]]
    transform <- if (measure %in% c("risk0", "risk1")) stats::qlogis else if (measure == "rr") log else identity
    inverse <- if (measure %in% c("risk0", "risk1")) stats::plogis else if (measure == "rr") exp else identity
    se <- if (measure %in% c("risk0", "risk1")) tab[[paste0("se_", measure)]] / (point * (1 - point)) else
      if (measure == "rr") tab$se_log_rr else tab$se_rd
    deviation <- sweep(transform(v), 1, transform(point), "-")
    active <- se > 1e-14
    if (any(!active) && any(abs(deviation[!active, , drop = FALSE]) > 1e-8))
      stop("Nonconstant draw at a numerically zero delta standard error.")
    max_t <- if (any(active)) apply(abs(sweep(deviation[active, , drop = FALSE], 1, se[active], "/")), 2, max) else rep(0, draws)
    critical <- unname(stats::quantile(max_t, standardized$level, type = 8))
    pointwise <- t(apply(v, 1, stats::quantile, probs = c(alpha / 2, 1 - alpha / 2), type = 8))
    bands[[j]] <- data.frame(age = tab$age, measure = measure, estimate = point,
      pointwise_lower = pointwise[, 1], pointwise_upper = pointwise[, 2],
      simultaneous_lower = inverse(transform(point) - critical * se),
      simultaneous_upper = inverse(transform(point) + critical * se), critical = critical)
  }
  statuses <- vapply(seq_len(draws), function(i) grid_crossings(tab$age, values[, i, "rd"])$status, character(1))
  counts <- as.data.frame(table(statuses), stringsAsFactors = FALSE)
  names(counts) <- c("status", "draws"); counts$proportion <- counts$draws / draws
  list(bands = do.call(rbind, bands), point_crossings = grid_crossings(tab$age, tab$rd),
       draw_crossing_status = counts, coefficient_draws = draws, seed = seed,
       family = "Separate simultaneous family for each measure across supplied ages only",
       uncertainty_target = standardized$uncertainty_target,
       caveat = "Approximate normal-coefficient simulation, not a bootstrap or biological-root posterior.")
}
