# Synthetic-only tests; no source records or national fits.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
if (length(script) != 1L) stop("Run using Rscript.")
v2 <- dirname(dirname(normalizePath(script, mustWork = TRUE)))
source(file.path(v2, "R", "04_standardization.R"))
checks <- 0L
check <- function(x, label) {
  if (!isTRUE(x)) stop("FAILED: ", label, call. = FALSE)
  checks <<- checks + 1L
}
equal <- function(x, y, label, tolerance = 1e-8) {
  if (is.numeric(x) && is.numeric(y)) { x <- as.numeric(x); y <- as.numeric(y) }
  check(isTRUE(all.equal(x, y, tolerance = tolerance, check.attributes = FALSE)), label)
}
error <- function(expr, pattern, label) {
  ans <- tryCatch({ force(expr); NULL }, error = identity)
  check(inherits(ans, "error") && grepl(pattern, conditionMessage(ans)), label)
}

spec <- lock_age_basis(rep(15:44, each = 4))
equal(spec$knots, c(17.9, 29.5, 41.1), "Outcome-free quantile knots")
whole <- fixed_age_basis(15:44, spec)
equal(whole[c(3, 12, 25), ], fixed_age_basis(c(17, 26, 39), spec), "Subset prediction preserves basis")
equal(whole[9, ], drop(fixed_age_basis(23, spec)), "Singleton prediction preserves basis")
error(lock_age_basis(rep(20, 10)), "unique", "Duplicate quantile knots fail")
error(lock_age_basis(c(14, 20)), "outside", "Training boundaries enforced")
error(fixed_age_basis(45, spec), "outside", "Prediction extrapolation forbidden")
error(lock_age_basis(20:40, knots = c(25, 25)), "unique", "Duplicate explicit knots fail")

# Independent finite-difference gradients of all five reported quantities.
beta <- c(intercept = -2, exposure = .4, z = .6)
V <- matrix(c(.04, .005, -.001, .005, .03, .002, -.001, .002, .02), 3,
            dimnames = list(names(beta), names(beta)))
X0 <- cbind(intercept = 1, exposure = 0, z = c(-1, 0, .5, 2))
X1 <- X0; X1[, "exposure"] <- 1
est <- logit_risk_contrasts(beta, V, X0, X1)
num <- sapply(seq_along(beta), function(j) {
  plus <- minus <- beta; plus[j] <- plus[j] + 1e-6; minus[j] <- minus[j] - 1e-6
  (logit_risk_contrasts(plus, V, X0, X1)$estimates -
     logit_risk_contrasts(minus, V, X0, X1)$estimates) / 2e-6
})
equal(est$gradient, num, "All delta gradients agree with finite differences", tolerance = 1e-7)
equal(est$covariance, num %*% V %*% t(num), "Full shared-coefficient covariance", tolerance = 1e-7)
equal(est$estimates["risk0"], mean(plogis(-2 + .6 * X0[, "z"])), "Manual standardized reference risk")
equal(est$covariance["rd", "rd"], est$covariance["risk1", "risk1"] +
        est$covariance["risk0", "risk0"] - 2 * est$covariance["risk0", "risk1"],
      "RD variance includes cross-exposure covariance")
bnull <- beta; bnull["exposure"] <- 0
null <- logit_risk_contrasts(bnull, V, X0, X1)
equal(null$estimates[c("rd", "rr", "log_rr")], c(0, 1, 0), "Exact null on RD and RR scales")
identical_targets <- logit_risk_contrasts(beta, V, X0, X0)
equal(identical_targets$se[c("rd", "log_rr")], c(0, 0), "Identical interventions give zero contrast variance")
error(logit_risk_contrasts(beta, V, X0, X1[-1, ]), "same target", "Unequal target row counts fail")
error(logit_risk_contrasts(beta, V[c(2, 1, 3), ], X0, X1), "names", "Covariance ordering fails loudly")
badV <- V; badV[1, 1] <- -1
error(logit_risk_contrasts(beta, badV, X0, X1), "semidefinite", "Indefinite covariance rejected")
badX <- X0; badX[1, 1] <- NA_real_
error(logit_risk_contrasts(beta, V, badX, X1), "finite", "Missing design entry rejected")

# Paired records yield an exact empirical null while retaining outcome variation.
set.seed(913)
base <- data.frame(age = rep(15:44, each = 30), z = rnorm(900))
base$y <- rbinom(nrow(base), 1, plogis(-1.8 + .025 * (base$age - 30) + .5 * base$z))
d <- rbind(transform(base, a = 0), transform(base, a = 1))
fit <- fit_standardized_logit(d, "y", "a", "age", "z", spec)
target <- standardize_logit(fit, d, c(20, 30, 40), target = "age_specific_empirical")
equal(target$summary$rd, rep(0, 3), "Exact paired-data fitted RD null", tolerance = 1e-7)
equal(target$summary$rr, rep(1, 3), "Exact paired-data fitted RR null", tolerance = 1e-7)
equal(target$summary$target_n, rep(60L, 3), "Age-specific target counts")

# Verify the hand-reconstructed natural-spline prediction and conditional target.
for (i in 1:3) {
  m <- target$matrices[[i]]
  equal(target$summary$risk0[i], mean(plogis(drop(m$X0 %*% coef(fit$fit)))), "Direct prediction average")
}
common <- standardize_logit(fit, d, c(20, 30, 40), target = "common_empirical")
equal(common$summary$target_n, rep(nrow(d), 3), "Common target explicitly uses all rows")
check(any(abs(common$summary$risk0 - target$summary$risk0) > 1e-5), "Different targets can yield different risks")
error(standardize_logit(fit, d, 20.5, target = "age_specific_empirical"), "No empirical", "No silent age interpolation")
error(standardize_logit(fit, d, c(30, 20), target = "age_specific_empirical"), "increasing", "Unsorted ages rejected")
error(standardize_logit(fit, d, 20), "target", "Target choice required")
bad <- d; bad$z[1] <- NA
error(fit_standardized_logit(bad, "y", "a", "age", "z", spec), "Missing", "No implicit complete-case deletion")
bad <- d; bad$a <- 1
error(fit_standardized_logit(bad, "y", "a", "age", "z", spec), "both", "Single exposure arm rejected")
bad <- d; bad$duplicate <- bad$z
error(fit_standardized_logit(bad, "y", "a", "age", c("z", "duplicate"), spec), "rank", "Aliased model rejected")

if (requireNamespace("sandwich", quietly = TRUE)) {
  hc <- fit_standardized_logit(d, "y", "a", "age", "z", spec, "HC0")
  X <- hc$fit$x; mu <- fitted(hc$fit); residual <- hc$fit$y - mu
  bread <- solve(crossprod(X, X * (mu * (1 - mu))))
  manual <- bread %*% crossprod(X, X * residual^2) %*% bread
  equal(hc$vcov, manual, "HC0 agrees with independent score-sandwich calculation", 1e-7)
} else stop("Engineering test requires the previously verified installed sandwich package.")

equal(grid_crossings(20:22, c(-.1, .1, .2))$status, "single_grid_crossing", "Single sign bracket")
equal(grid_crossings(20:22, c(-.1, .1, -.1))$status, "multiple_grid_crossings", "Multiple sign brackets")
equal(grid_crossings(20:22, c(0, 0, 0))$status, "all_zero", "All-zero curve is not a selected root")
equal(grid_crossings(20:22, c(.1, .2, .3))$status, "no_grid_crossing", "No-root status")
equal(grid_crossings(20:22, c(0, 0, .1))$status, "flat_zero_segment", "Flat zero segment retained")
equal(grid_crossings(20:22, c(-.1, 0, .1))$zero_ages, 21, "Exact grid zero not double-counted")

set.seed(333); before <- .Random.seed
bands <- coefficient_bands(target, draws = 200L, seed = 99L)
check(identical(before, .Random.seed), "Local coefficient simulation preserves caller RNG")
again <- coefficient_bands(target, draws = 200L, seed = 99L)
check(identical(bands, again), "Coefficient simulation reproducible")
check(nrow(bands$bands) == 12L, "All four measures and ages included")
check(all(bands$bands$simultaneous_lower <= bands$bands$estimate &
          bands$bands$simultaneous_upper >= bands$bands$estimate), "Simultaneous bands center on estimates")
risk_bands <- bands$bands[bands$bands$measure %in% c("risk0", "risk1"), ]
check(all(risk_bands$simultaneous_lower > 0 & risk_bands$simultaneous_upper < 1), "Logit-scale risk bands bounded")
equal(sum(bands$draw_crossing_status$draws), 200L, "Every draw has a crossing status")
equal(sum(bands$draw_crossing_status$proportion), 1, "Crossing frequencies sum to one")
error(coefficient_bands(target, draws = 20), "100", "Insufficient draws fail")
cat("Passed", checks, "synthetic standardization checks. No national records were read.\n")
