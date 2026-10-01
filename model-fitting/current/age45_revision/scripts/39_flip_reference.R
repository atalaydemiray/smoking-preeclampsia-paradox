# One reference group per model (protocol addendum K, 19 September 2026): SS is the reference of the
# primary model and S of the before-pregnancy model. This is an exact re-expression of fits that already
# exist; NOTHING is refitted and no existing output is modified.
#
#   RD(g vs r) = -RD(r vs g)             standard error unchanged, interval negated and bounds swapped
#   RR(g vs r) =  1 / RR(r vs g)         log-scale SE unchanged, interval reciprocal with bounds swapped
#   crossover  =  unchanged              the conditional contrast is negated, so its roots are identical
#   sign regions swap; standardized risks are unchanged, only their labels move
#
# The crossover is not edited: the relevant coefficient block is negated and analyze_age_crossover is run
# again on it, so the roots, the local intervals, the null-age set and the sign regions all come from the
# engine rather than from arithmetic on its output.
#
# Writes:
#   outputs/supplementary/joint_3group/sn_vs_ss/   SN compared with SS   (from ss_vs_sn)
#   outputs/supplementary/joint_3group/nn_vs_ss/   NN compared with SS   (from ss_vs_nn)
#   outputs/supplementary/flipped/n_vs_s/          N  compared with S    (from models/prepregnancy_main)
# Each in the engine's file layout, with a receipt and a verification table.
#
# From 01_Analysis: Rscript --vanilla age45_revision/scripts/39_flip_reference.R
main <- function() {
  source("age45_revision/R/fit_helpers.R"); age45_initialize()
  suppressPackageStartupMessages(library(data.table))
  JOINT <- "age45_revision/outputs/supplementary/joint_3group"
  SELF <- "age45_revision/scripts/39_flip_reference.R"
  ages <- 15:45; tol <- 1e-9
  manifest <- rbind(age45_code_manifest(), data.frame(path = SELF, sha256 = age45_sha(SELF)))
  checks <- list()

  # ---- transform one standardized table -------------------------------------------------------------
  # risk0 and risk1 swap; rd negates with its bounds swapped; rr becomes its reciprocal with bounds
  # swapped. Standard errors on the rd and log-rr scales are unchanged, and are carried through so the
  # Bonferroni columns stay correct.
  flip_tab <- function(a) {
    b <- data.table::copy(a)
    swap <- function(x, y) list(y, x)
    b[, c("risk0", "risk1") := swap(a$risk0, a$risk1)]
    b[, c("risk0_lower", "risk1_lower") := swap(a$risk0_lower, a$risk1_lower)]
    b[, c("risk0_upper", "risk1_upper") := swap(a$risk0_upper, a$risk1_upper)]
    b[, c("se_risk0", "se_risk1") := swap(a$se_risk0, a$se_risk1)]
    b[, rd := -a$rd]; b[, rd_lower := -a$rd_upper]; b[, rd_upper := -a$rd_lower]   # se_rd unchanged
    b[, rr := 1 / a$rr]; b[, rr_lower := 1 / a$rr_upper]; b[, rr_upper := 1 / a$rr_lower]
    for (nm in c("risk0", "risk1", "rd", "risk0_lower", "risk0_upper", "risk1_lower", "risk1_upper", "rd_lower", "rd_upper"))
      b[[paste0(nm, "_per1000")]] <- 1000 * b[[nm]]
    if ("rd_bonferroni_lower_per1000" %in% names(a)) {          # negate and swap, the SE is unchanged
      b[, rd_bonferroni_lower_per1000 := -a$rd_bonferroni_upper_per1000]
      b[, rd_bonferroni_upper_per1000 := -a$rd_bonferroni_lower_per1000]
    }
    b
  }
  flip_overall <- function(o) {
    b <- data.table::copy(o); g <- function(m, col) o[[col]][o$measure == m]
    set_m <- function(m, est, lo, hi) {
      b[measure == m, estimate := est]; b[measure == m, lower := lo]; b[measure == m, upper := hi]
    }
    set_m("risk0", g("risk1", "estimate"), g("risk1", "lower"), g("risk1", "upper"))
    set_m("risk1", g("risk0", "estimate"), g("risk0", "lower"), g("risk0", "upper"))
    set_m("rd", -g("rd", "estimate"), -g("rd", "upper"), -g("rd", "lower"))
    set_m("rr", 1 / g("rr", "estimate"), 1 / g("rr", "upper"), 1 / g("rr", "lower"))
    for (nm in c("estimate", "lower", "upper"))
      b[[paste0(nm, "_per1000")]] <- ifelse(b$measure == "rr", NA_real_, 1000 * b[[nm]])
    b
  }
  # ---- write one flipped contrast --------------------------------------------------------------------
  write_flip <- function(id, src, out, beta, V, spec, label) {
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    for (cv in c("HC0", "model")) {
      a <- fread(file.path(src, paste0(cv, "_age_standardized.csv")))
      o <- fread(file.path(src, paste0(cv, "_overall_standardized.csv")))
      fa <- flip_tab(a); fo <- flip_overall(o)
      stopifnot(identical(as.numeric(fa$age), as.numeric(ages)),
                all(abs(fa$rr * a$rr - 1) < tol), all(abs(fa$rd + a$rd) < tol),
                all(fa$risk0 > 0 & fa$risk0 < 1), all(fa$risk1 > 0 & fa$risk1 < 1))
      fwrite(fa, file.path(out, paste0(cv, "_age_standardized.csv")))
      fwrite(fo, file.path(out, paste0(cv, "_overall_standardized.csv")))
      # the crossover comes from the engine, run on the negated coefficient block
      Vc <- V[[cv]]
      co_new <- analyze_age_crossover(-beta, Vc, spec$age_spec$knots, spec$age_spec$boundaries)
      co_old <- readRDS(file.path(src, paste0(cv, "_crossover.rds")))
      r_new <- as.data.table(co_new$roots); r_old <- as.data.table(co_old$roots)
      stopifnot(nrow(r_new) == nrow(r_old))
      if (nrow(r_new)) stopifnot(max(abs(r_new$age - r_old$age)) < tol,
                                 max(abs(r_new$delta_lower - r_old$delta_lower)) < tol,
                                 max(abs(r_new$delta_upper - r_old$delta_upper)) < tol)
      stopifnot(isTRUE(co_new$simultaneous$both_signs_demonstrated) == isTRUE(co_old$simultaneous$both_signs_demonstrated))
      ev <- evaluate_age_crossover(co_new, ages)
      stopifnot(all(sign(ev$h) == sign(fa$rd)))          # the negated contrast must track the negated RD
      saveRDS(co_new, file.path(out, paste0(cv, "_crossover.rds")))
      fwrite(co_new$roots, file.path(out, paste0(cv, "_roots.csv")))
      for (method in c("pointwise", "simultaneous"))
        fwrite(co_new[[method]]$confidence_set, file.path(out, paste0(cv, "_", method, "_null_age_set.csv")))
      if (cv == "HC0") {
        neg_old <- subset(co_old$simultaneous$sign_regions, classification == "strict_negative")
        pos_new <- subset(co_new$simultaneous$sign_regions, classification == "strict_positive")
        stopifnot(nrow(neg_old) == nrow(pos_new))        # the regions swap, they do not vanish
        checks[[id]] <<- data.table(contrast = id, quantity = c("overall RR x original RR", "overall RD + original RD",
          "max |crossover difference|", "sign regions swapped", "reversal flag identical"),
          value = c(fo[measure == "rr", estimate] * o[measure == "rr", estimate],
                    fo[measure == "rd", estimate] + o[measure == "rd", estimate],
                    if (nrow(r_new)) max(abs(r_new$age - r_old$age)) else 0, 1, 1),
          expected = c(1, 0, 0, 1, 1))
      }
    }
    # the arm support of the source contrast, with the arms swapped
    sup <- fread(file.path(src, "age_arm_support.csv")); sup[, A := 1L - A]
    fwrite(sup[order(age, A)], file.path(out, "age_arm_support.csv"))
    files <- list.files(out, full.names = TRUE); files <- files[basename(files) != "receipt.json"]
    age45_json(list(status = "complete_flipped_contrast", model = id, label = label,
      protocol = "addendum K, 19 September 2026", derived_from = src, refitted = FALSE,
      identities = c("RD(g vs r) = -RD(r vs g)", "RR(g vs r) = 1/RR(r vs g)", "crossover unchanged",
                     "simultaneous sign regions swap", "standardized risks unchanged"),
      code = manifest, outputs = data.frame(path = files, sha256 = vapply(files, age45_sha, ""))),
      file.path(out, "receipt.json"))
    message("wrote ", out)
  }
  # ---- primary model: SN vs SS and NN vs SS ----------------------------------------------------------
  r <- jsonlite::fromJSON(file.path(JOINT, "receipt.json"))
  stopifnot(identical(r$status, "complete_joint_three_group_fit"))
  fit <- readRDS(file.path(JOINT, "model.rds"))
  spec <- list(age_spec = fit$age_spec)
  n <- names(fit$beta); a_at <- match(r$coefficient_blocks$A, n); sn_at <- match(r$coefficient_blocks$SN, n)
  stopifnot(!anyNA(a_at), !anyNA(sn_at))
  Vs <- list(HC0 = fit$vcov_HC0, model = fit$vcov_model)
  blk <- function(at) lapply(Vs, function(V) V[at, at, drop = FALSE])
  # NN vs SS: the source contrast SS vs NN has block beta[a_at]
  write_flip("nn_vs_ss", file.path(JOINT, "ss_vs_nn"), file.path(JOINT, "nn_vs_ss"),
             fit$beta[a_at], blk(a_at), spec, "NN compared with SS (primary model)")
  # SN vs SS: the source contrast SS vs SN has block beta[a_at] - beta[sn_at]
  beta_d <- fit$beta[a_at] - fit$beta[sn_at]
  Vd <- lapply(Vs, function(V) { z <- V[a_at, a_at, drop = FALSE] + V[sn_at, sn_at, drop = FALSE] -
                                      V[a_at, sn_at, drop = FALSE] - V[sn_at, a_at, drop = FALSE]
                                 z <- (z + t(z)) / 2; dimnames(z) <- list(names(beta_d), names(beta_d)); z })
  write_flip("sn_vs_ss", file.path(JOINT, "ss_vs_sn"), file.path(JOINT, "sn_vs_ss"),
             beta_d, Vd, spec, "SN compared with SS (primary model)")
  # ---- before-pregnancy model: N vs S ----------------------------------------------------------------
  PRE <- "age45_revision/outputs/models/prepregnancy_main"
  rp <- jsonlite::fromJSON(file.path(PRE, "receipt.json"))
  stopifnot(identical(rp$status, "complete_validated_age15_45_fit"))
  pfit <- readRDS(file.path(PRE, "model.rds"))
  pn <- names(pfit$beta); pat <- which(pn == "A" | startsWith(pn, "A:splines::ns(age,"))
  stopifnot(length(pat) == length(pfit$age_spec$knots) + 2L)
  write_flip("n_vs_s", PRE, "age45_revision/outputs/supplementary/flipped/n_vs_s",
             pfit$beta[pat], list(HC0 = pfit$vcov_HC0[pat, pat, drop = FALSE], model = pfit$vcov_model[pat, pat, drop = FALSE]),
             list(age_spec = pfit$age_spec), "N compared with S (before-pregnancy model)")
  # ---- verification table ----------------------------------------------------------------------------
  v <- rbindlist(checks)
  v[, ok := abs(value - expected) < 1e-8]
  out <- "age45_revision/outputs/supplementary/flipped"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
  fwrite(v, file.path(out, "flip_verification.csv")); print(v)
  if (!all(v$ok)) stop("reference flip: an identity did not hold; nothing downstream may use these outputs")
  message("reference flip verified: every identity holds")
  invisible(TRUE)
}
if (sys.nframe() == 0L) main()
