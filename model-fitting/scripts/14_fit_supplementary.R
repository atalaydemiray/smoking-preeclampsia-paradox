# Supplementary fits (protocol/SUPPLEMENTARY_AMENDMENT_2026-09-11.md and ADDENDUM_B). Same
# numerical engines as the main analysis (streaming logistic fit, HC0, age-specific empirical
# standardization, crossover engine); results go to
# outputs/supplementary/models/<id>. Released main outputs are never touched.
#
# Memory design (11 Sep 2026, after two exit-137 kills on the 28.6M-record population):
#   * one fit per R process is the intended mode ("<set> <id>"); in that mode a population
#     is loaded only if the requested id needs it, identity columns are cut to the three
#     record keys as soon as the exposure flags are extracted, and the population is freed
#     before the fit starts, so a process holds one fit frame plus (when the standardization
#     reference differs from the fit rows) one reference frame;
#   * when the reference rows equal the fit rows the same object is used, not a copy;
#   * the integrity signature hashes each column separately instead of serializing the whole
#     frame, which is what 12_large_cohort_refits.R does for the released 30M-record fits.
# None of this changes a fitted number: the data handed to fit_streaming_logit and to
# standardize_streaming_logit are identical to the earlier driver's.
#
# From model-fitting/:
#   Rscript --vanilla scripts/14_fit_supplementary.R small              # 16 fits, one process
#   Rscript --vanilla scripts/14_fit_supplementary.R large <model_id>   # one 30M-record fit
#   Rscript --vanilla scripts/14_fit_supplementary.R chtn <model_id>
#   Rscript --vanilla scripts/14_fit_supplementary.R only <model_id>    # search every set
main <- function(set = "small", only = NULL) {
  source("R/17_fit_helpers.R"); age45_initialize()
  library(data.table)
  ROOT <- "outputs/supplementary/models"; dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
  INP <- "derived/supplementary_inputs"
  manifest <- rbind(age45_code_manifest(), data.frame(path = c("scripts/13_prepare_supplementary_inputs.R",
    "scripts/14_fit_supplementary.R"), sha256 = c(age45_sha("scripts/13_prepare_supplementary_inputs.R"),
    age45_sha("scripts/14_fit_supplementary.R"))))
  single <- !is.null(only)                       # one-fit-per-process mode
  wants <- function(ids) !single || only %in% ids
  strip_race <- function(spec) {
    spec$covariates <- setdiff(spec$covariates, "race_ethnicity")
    spec$fit_covariates <- setdiff(spec$fit_covariates, "race_ethnicity")
    f <- paste(deparse(spec$formula, width.cutoff = 500), collapse = " ")
    spec$formula <- as.formula(gsub("\\s*\\+\\s*race_ethnicity", "", f), env = baseenv())
    spec
  }
  hash_frame <- function(x) digest::digest(list(names = names(x), n = nrow(x),
    columns = vapply(x, function(col) digest::digest(col, algo = "sha256"), "")), algo = "sha256")
  complete_rows <- function(data, covariates) {           # complete cases without copying the columns
    ok <- rep(TRUE, nrow(data)); for (v in covariates) ok <- ok & !is.na(data[[v]]); ok
  }
  # ---- fit wrapper: age45_fit logic with a supplementary output root -------------------
  supp_fit <- function(id, d, spec, identity, reference = NULL, root_allowed = TRUE, eligible_n = nrow(d), note = "") {
    if (single && id != only) return(invisible(NULL))
    out <- file.path(ROOT, id); dir.create(out, recursive = TRUE, showWarnings = FALSE)
    rp <- file.path(out, "receipt.json")
    same_reference <- is.null(reference); if (same_reference) reference <- d
    hd <- hash_frame(d)
    signature <- digest::digest(list(data = hd, identity = hash_frame(identity),
      reference = if (same_reference) hd else hash_frame(reference),
      formula = deparse(spec$formula), age = spec$age_spec, code = manifest), algo = "sha256")
    if (file.exists(rp)) {
      old <- jsonlite::fromJSON(rp)
      if (old$status == "complete_validated_supplementary_fit" && old$input_signature == signature) {
        for (i in seq_len(nrow(old$outputs))) stopifnot(age45_sha(old$outputs$path[i]) == old$outputs$sha256[i])
        message(id, ": verified completed fit reused"); return(invisible(NULL))
      }
    }
    stopifnot(!anyNA(d), !anyNA(reference), all(d$age %in% 15:45), nrow(d) == nrow(identity),
      all(d$A %in% 0:1), sum(d$A == 1) > 0, sum(d$A == 0) > 0,
      !anyDuplicated(identity[c("source_type", "year", "source_row")]))
    before <- Sys.time()
    age45_json(list(status = "running", model = id, fit_n = nrow(d), reference_n = nrow(reference),
      input_signature = signature, started = as.character(before)), file.path(out, "running.json"))
    message(id, ": fitting N=", nrow(d), "; exposed=", sum(d$A == 1), "; events=", sum(d$Y), "; ", note)
    fit <- fit_streaming_logit(d, spec$formula, "Y", "A", "age", spec$age_spec, "HC0", 25000L)
    stopifnot(fit$converged, max(abs(fit$final_score)) / nrow(d) < 1e-12)
    saveRDS(fit, file.path(out, "model.rds"))
    support <- data.table(age = d$age, A = d$A, Y = d$Y)[, .(n = .N, events = sum(Y), non_events = sum(1 - Y)), by = .(age, A)]
    support <- merge(CJ(age = 15:45, A = 0:1), support, by = c("age", "A"), all.x = TRUE)
    support[is.na(n), c("n", "events", "non_events") := list(0L, 0L, 0L)]
    support[, sparse := events < 20 | non_events < 20]
    fwrite(support, file.path(out, "age_arm_support.csv"))
    # Root finding is skipped when an arm has no support at some age (curve is still saved).
    root_ok <- root_allowed && !any(support$n == 0)
    age45_save_standardization(fit, reference, out, root_ok)
    files <- list.files(out, full.names = TRUE); files <- files[!basename(files) %in% c("receipt.json", "running.json")]
    age45_json(list(status = "complete_validated_supplementary_fit", model = id, note = note,
      eligible_n = eligible_n, fit_n = nrow(d), events = sum(d$Y), A0 = sum(d$A == 0), A1 = sum(d$A == 1),
      reference_n = nrow(reference), reference_equals_fit_rows = same_reference,
      input_signature = signature, input_signature_method = "columnwise SHA256 of fit data, identity keys and reference",
      age_domain = c(15, 45), formula = paste(deparse(spec$formula, width.cutoff = 500), collapse = " "),
      age_knots = spec$age_spec$knots, age_boundaries = spec$age_spec$boundaries, covariates = spec$covariates,
      no_imputation = TRUE, root_finding = root_ok, iterations = fit$iterations, score_max = max(abs(fit$final_score)),
      code = manifest, elapsed_seconds = as.numeric(difftime(Sys.time(), before, units = "secs")),
      outputs = data.frame(path = files, sha256 = vapply(files, age45_sha, ""))), rp)
    unlink(file.path(out, "running.json"))
    message(id, ": completed in ", round(as.numeric(difftime(Sys.time(), before, units = "secs"))), " seconds")
    rm(fit); gc(FALSE); invisible(NULL)
  }
  # ---- loaders: return an environment so the population can be released before fitting --
  load_ext <- function(name) {
    parts <- ids <- list(); spec <- NULL
    for (y in 2016:2024) {
      stem <- file.path(INP, paste(y, name, sep = "_")); r <- jsonlite::fromJSON(paste0(stem, "_receipt.json"))
      stopifnot(r$status == "complete_prepared_supplementary", age45_sha(paste0(stem, ".rds")) == r$output_sha256)
      p <- readRDS(paste0(stem, ".rds")); spec <- p$spec
      parts[[as.character(y)]] <- p$data; ids[[as.character(y)]] <- p$identity
      message(name, ": loaded ", y, " N=", nrow(p$data)); rm(p); gc(FALSE)
    }
    d <- as.data.frame(rbindlist(parts, use.names = TRUE)); id <- as.data.frame(rbindlist(ids, use.names = TRUE))
    rm(parts, ids); gc(FALSE)
    d$year_factor <- factor(as.character(d$year_factor), levels = 2016:2024); spec$years <- 2016:2024
    stopifnot(nrow(d) == nrow(id), !anyDuplicated(id[c("source_type", "year", "source_row")]))
    list2env(list(data = d, identity = id, spec = spec))
  }
  load_main <- function(ct) list2env(age45_load_natality(ct, 2016:2024))
  keys <- c("source_type", "year", "source_row")
  vars_of <- function(spec) c("Y", "A", "age", spec$covariates)
  run_subset <- function(id, pe, spec, rows, A, refrows = rows, root = TRUE, note = "", Y = NULL) {
    # Skip before building anything (the fits this skips are exactly the ones supp_fit skips).
    if (single && id != only) return(invisible(NULL))
    vars <- vars_of(spec)
    d <- pe$data[rows, vars, drop = FALSE]; d$A <- as.integer(A[rows])
    if (!is.null(Y)) d$Y <- as.integer(Y[rows])
    d$year_factor <- factor(as.character(d$year_factor), levels = spec$years)
    ref <- NULL
    if (!identical(refrows, rows)) {
      ref <- pe$data[refrows, vars, drop = FALSE]
      ref$year_factor <- factor(as.character(ref$year_factor), levels = spec$years)
    }
    ident <- pe$identity[rows, keys, drop = FALSE]; eligible <- nrow(pe$data)
    if (single) { pe$data <- NULL; pe$identity <- NULL; gc(FALSE) }   # release the population
    supp_fit(id, d, spec, ident, ref, root, eligible_n = eligible, note = note)
  }
  strata <- list(nhw = "Non-Hispanic White", nhb = "Non-Hispanic Black", hisp = "Hispanic",
    other = c("Non-Hispanic AIAN", "Non-Hispanic Asian", "Non-Hispanic NHOPI", "Non-Hispanic multiple race"))
  dose_id <- function(prefix, lev) paste0(prefix, gsub("[-+]", "_", gsub("\\+", "plus", lev)))
  # ---- SMALL: within-smoker population (about 2 million records) ---------------------
  if (set %in% c("small", "all")) {
    WITHIN <- c(dose_id("supp_t1dose_", c("1-5", "6-10", "11-20", "21+")), "supp_t1change_reduced", "supp_t1change_same_or_more",
      "supp_strict_continued_vs_stopped", "supp_strict_relapse_vs_stopped",
      paste0("supp_timing_", c("stopped_by_T1", "stopped_in_T2", "stopped_in_T3")))
    if (wants(WITHIN)) {
      pe <- load_ext("within_ext"); spec <- pe$spec
      cc <- complete_rows(pe$data, spec$covariates)
      fl <- list(t1dose = pe$identity$t1_dose_cat, change = pe$identity$dose_change, t1 = pe$identity$t1_smoking,
        later = pe$identity$later_positive, timing = pe$identity$timing)
      pe$identity <- pe$identity[, keys, drop = FALSE]; gc(FALSE)
      allref <- which(cc)
      # S-B first-trimester dose: each category vs 0 cigarettes in T1, shared reference.
      for (lev in c("1-5", "6-10", "11-20", "21+")) {
        rows <- which(cc & (fl$t1dose %in% c("0", lev)))
        run_subset(dose_id("supp_t1dose_", lev), pe, spec, rows, A = fl$t1dose == lev, refrows = allref,
          note = paste("T1 dose", lev, "vs 0; adjusted for prepregnancy dose"))
      }
      # S-B dose change vs stopped.
      for (lev in c("reduced", "same_or_more")) {
        rows <- which(cc & fl$change %in% c("stopped", lev))
        run_subset(paste0("supp_t1change_", lev), pe, spec, rows, A = fl$change == lev, refrows = allref,
          note = paste("T1 dose", lev, "vs stopped"))
      }
      # S-C strict stopping; the relapse contrast is a bias illustration (addendum B, section 2).
      strict <- fl$t1 %in% FALSE & !fl$later; relapse <- fl$t1 %in% FALSE & fl$later
      rows <- which(cc & (fl$t1 %in% TRUE | strict))
      run_subset("supp_strict_continued_vs_stopped", pe, spec, rows, A = fl$t1 %in% TRUE, refrows = allref,
        note = "continued vs stopped with no later positive report (missing = not positive)")
      rows <- which(cc & (relapse | strict))
      run_subset("supp_strict_relapse_vs_stopped", pe, spec, rows, A = relapse, refrows = allref, root = FALSE,
        note = "T1 zero then later positive vs strict stopped; exposed arm requires a later report (bias illustration)")
      # S-E stopping timing vs smoked throughout (bias illustration: comparator requires a T3 report).
      for (lev in c("stopped_by_T1", "stopped_in_T2", "stopped_in_T3")) {
        rows <- which(cc & fl$timing %in% c("continued_through", lev))
        run_subset(paste0("supp_timing_", lev), pe, spec, rows, A = fl$timing == lev, refrows = allref,
          note = paste(lev, "vs continued through delivery; conditions on post-onset fields by design"))
      }
      rm(pe, fl); gc(FALSE)
    }
    PRIMARY <- c("supp_preterm_primary", paste0("supp_race_primary_", names(strata)))
    if (wants(PRIMARY)) {
      # S-F control outcome and S-G race strata on the primary population.
      pe <- load_main("primary"); spec <- pe$spec
      cc <- complete_rows(pe$data, spec$covariates)
      preterm <- pe$identity$oe_weeks < 37; race <- as.character(pe$data$race_ethnicity); A <- pe$data$A
      pe$identity <- pe$identity[, keys, drop = FALSE]; gc(FALSE)
      run_subset("supp_preterm_primary", pe, spec, which(cc), A = A, note = "control outcome preterm <37 wk; M1 population", Y = preterm)
      sp <- strip_race(spec)
      for (s in names(strata)) run_subset(paste0("supp_race_primary_", s), pe, sp, which(cc & race %in% strata[[s]]), A = A,
        note = paste("M1 within", s))
      rm(pe); gc(FALSE)
    }
  }
  # ---- LARGE: prepregnancy population (about 30 million records) ---------------------
  if (set %in% c("large", "all")) {
    EXT <- c("supp_3lvl_stopped_vs_none", "supp_3lvl_continued_vs_none", "supp_clean_prepregnancy", "supp_clean_broad",
      dose_id("supp_pdose_", c("1-5", "6-10", "11-20", "21+")), "supp_t3_through_vs_none")
    if (wants(EXT)) {
      pe <- load_ext("prepregnancy_ext"); spec <- pe$spec
      cc <- complete_rows(pe$data, spec$covariates)
      fl <- list(pre = pe$identity$pre_smoking, t1 = pe$identity$t1_smoking, later = pe$identity$later_positive,
        pdose = pe$identity$pre_dose_cat, a4p = pe$identity$all_four_positive, a4z = pe$identity$all_four_zero)
      pe$identity <- pe$identity[, keys, drop = FALSE]; gc(FALSE)
      # S-A three-level early smoking, shared reference (all three groups).
      grp <- ifelse(fl$pre %in% FALSE & fl$t1 %in% FALSE, "none",
        ifelse(fl$pre %in% TRUE & fl$t1 %in% FALSE, "stopped",
        ifelse(fl$pre %in% TRUE & fl$t1 %in% TRUE, "continued", NA)))
      allref <- which(cc & !is.na(grp))
      for (lev in c("stopped", "continued")) {
        rows <- which(cc & grp %in% c("none", lev))
        run_subset(paste0("supp_3lvl_", lev, "_vs_none"), pe, spec, rows, A = grp == lev, refrows = allref,
          note = paste(lev, "by T1 vs no early smoking; common reference"))
      }
      # S-D clean reference for M3 and M2 (reference narrowed; missing later fields count as not positive).
      clean0 <- fl$pre %in% FALSE & !(fl$t1 %in% TRUE) & !fl$later
      rows <- which(cc & (fl$pre %in% TRUE | clean0))
      run_subset("supp_clean_prepregnancy", pe, spec, rows, A = fl$pre %in% TRUE, note = "M3 with reference zero in every recorded window")
      both <- fl$pre %in% TRUE & fl$t1 %in% TRUE
      rows <- which(cc & (both | (clean0 & fl$t1 %in% FALSE)))
      run_subset("supp_clean_broad", pe, spec, rows, A = both, note = "M2 with reference zero in every recorded window")
      # S-B before-pregnancy dose vs 0, shared reference.
      pref <- which(cc & !is.na(fl$pdose))
      for (lev in c("1-5", "6-10", "11-20", "21+")) {
        rows <- which(cc & fl$pdose %in% c("0", lev))
        run_subset(dose_id("supp_pdose_", lev), pe, spec, rows, A = fl$pdose == lev, refrows = pref, note = paste("prepregnancy dose", lev, "vs 0"))
      }
      # S-E bias illustration: positive in all four windows vs zero in all four (requires T3 defined).
      rows <- which(cc & (fl$a4p | fl$a4z))
      run_subset("supp_t3_through_vs_none", pe, spec, rows, A = fl$a4p,
        note = "all four windows positive vs all four zero; conditions on reaching the third trimester by design")
      rm(pe, fl, grp); gc(FALSE)
    }
    # S-F control outcome and S-G race strata on the broad and prepregnancy main populations.
    for (ct in c("broad", "prepregnancy")) {
      GROUP <- c(paste0("supp_preterm_", ct), if (ct == "broad") paste0("supp_race_broad_", names(strata)))
      if (!wants(GROUP)) next
      pe <- load_main(ct); spec <- pe$spec
      cc <- complete_rows(pe$data, spec$covariates)
      preterm <- pe$identity$oe_weeks < 37; A <- pe$data$A
      race <- if (ct == "broad") as.character(pe$data$race_ethnicity) else NULL
      pe$identity <- pe$identity[, keys, drop = FALSE]; gc(FALSE)
      run_subset(paste0("supp_preterm_", ct), pe, spec, which(cc), A = A,
        note = paste("control outcome preterm <37 wk;", ct, "population"), Y = preterm)
      if (ct == "broad") {
        sp <- strip_race(spec)
        for (s in names(strata)) run_subset(paste0("supp_race_broad_", s), pe, sp, which(cc & race %in% strata[[s]]), A = A,
          note = paste("M2 within", s))
      }
      rm(pe); gc(FALSE)
    }
  }
  # ---- CHTN: chronic hypertension retained and adjusted (26_prepare_chronic_hypertension.R; addendum B) ----
  if (set %in% c("chtn", "all")) {
    CH <- "derived/chronic_hypertension"
    for (ct in c("primary", "broad", "prepregnancy")) {
      if (!wants(paste0("supp_chtn_", ct))) next
      parts <- ids <- list(); spec <- NULL
      for (y in 2016:2024) {
        stem <- file.path(CH, paste(y, ct, "chtn", sep = "_")); r <- jsonlite::fromJSON(paste0(stem, "_receipt.json"))
        stopifnot(r$status == "complete_prepared_chtn", age45_sha(paste0(stem, ".rds")) == r$output_sha256)
        p <- readRDS(paste0(stem, ".rds")); spec <- p$spec
        parts[[as.character(y)]] <- p$data; ids[[as.character(y)]] <- p$identity[, keys, drop = FALSE]
        message("chtn ", ct, ": loaded ", y, " N=", nrow(p$data)); rm(p); gc(FALSE)
      }
      d <- as.data.frame(rbindlist(parts, use.names = TRUE)); id <- as.data.frame(rbindlist(ids, use.names = TRUE))
      rm(parts, ids); gc(FALSE)
      d$year_factor <- factor(as.character(d$year_factor), levels = 2016:2024); spec$years <- 2016:2024
      pe <- list2env(list(data = d, identity = id, spec = spec)); rm(d, id); gc(FALSE)
      cc <- complete_rows(pe$data, spec$covariates); A <- pe$data$A
      run_subset(paste0("supp_chtn_", ct), pe, spec, which(cc), A = A,
        note = paste(ct, "with chronic hypertension retained and adjusted (explicit yes or no)"))
      rm(pe); gc(FALSE)
    }
  }
  invisible(TRUE)
}
if (sys.nframe() == 0L) {
  a <- commandArgs(TRUE)
  # "only <id>"   search every set for that id (loads only the population that id needs)
  # "<set> <id>"  run one id within one set, in its own process
  # "<set>"       run the whole set in one process
  if (length(a) >= 2 && a[1] == "only") main("all", only = a[2]) else
  if (length(a) >= 2) main(a[1], only = a[2]) else main(if (length(a)) a[1] else "small")
}
