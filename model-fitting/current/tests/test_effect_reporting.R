argv <- commandArgs(FALSE)
self <- normalizePath(sub("^--file=", "", argv[grepl("^--file=", argv)]))
v2 <- dirname(dirname(self))
source(file.path(v2, "R/04_standardization.R"))
for (risks in list(c(.2, .4), c(.4, .2))) {
  p0 <- risks[1]; p1 <- risks[2]
  beta <- c("(Intercept)" = qlogis(p0), A = qlogis(p1) - qlogis(p0))
  V <- diag(.01, 2); dimnames(V) <- list(names(beta), names(beta))
  X0 <- matrix(c(1, 0), nrow = 1, dimnames = list(NULL, names(beta)))
  X1 <- matrix(c(1, 1), nrow = 1, dimnames = list(NULL, names(beta)))
  result <- logit_risk_contrasts(beta, V, X0, X1)
  rr <- unname(result$estimates["rr"])
  rd <- unname(result$estimates["rd"])
  stopifnot(abs(rr - p1 / p0) < 1e-12,
            abs(rd - (p1 - p0)) < 1e-12,
            abs(result$estimates["risk0"] - p0) < 1e-12,
            abs(result$estimates["risk1"] - p1) < 1e-12,
            abs(rr - exp(beta["A"])) > .1,
            abs(1000 * rd - 1000 * (p1 - p0)) < 1e-10,
            !any(tolower(names(result$estimates)) %in% c("or", "odds_ratio")))
}
cat("Passed 14 common-outcome reporting checks: standardized RR/RD/risks, not logistic ORs.\n")
