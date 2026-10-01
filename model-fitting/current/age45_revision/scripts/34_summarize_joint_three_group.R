# Summaries of the joint three-group model (33_fit_joint_three_group.R, addendum H) and of the released
# main fits, in the assembler's format (24_assemble_supplementary.R), so that every downstream table and
# figure reads one file per quantity. No fitting; every receipt hash is verified before reading.
#
# Writes to age45_revision/outputs/supplementary/joint_3group/summary/:
#   main_summary.csv        one row per contrast: joint_ss_vs_nn, joint_sn_vs_nn, prepregnancy_main (S vs N),
#                           primary_main (SS vs SN, supplementary), broad_main (SS vs NN, SS+NN standardization)
#   main_age_estimates.csv  the age curves of the same contrasts, one row per age and contrast
#   three_group_curves.csv  NN, SN, SS standardized risks per 1,000 by age from the joint model
#
# From 01_Analysis: Rscript --vanilla age45_revision/scripts/34_summarize_joint_three_group.R [joint_dir]
main <- function(joint = "age45_revision/outputs/supplementary/joint_3group") {
  source("age45_revision/R/fit_helpers.R"); age45_initialize()
  suppressPackageStartupMessages(library(data.table))
  out <- file.path(joint, "summary"); dir.create(out, recursive = TRUE, showWarnings = FALSE)
  fmtset <- function(x) if (!nrow(x)) "Empty" else paste0(ifelse(x$lower_closed, "[", "("), sprintf("%.3f", x$lower), ", ",
    sprintf("%.3f", x$upper), ifelse(x$upper_closed, "]", ")"), collapse = " U ")
  verify <- function(rp, status) {
    r <- jsonlite::fromJSON(rp); stopifnot(identical(r$status, status))
    # run logs in a model folder receive their last line after the receipt hashed them; skip them
    for (i in seq_len(nrow(r$outputs))) if (!grepl("_log\\.txt$", r$outputs$path[i]))
      stopifnot(age45_sha(r$outputs$path[i]) == r$outputs$sha256[i]); r
  }
  one <- function(id, label, dir, n0, n1, events, fit_n, reference_n, note) {
    ov <- fread(file.path(dir, "HC0_overall_standardized.csv")); at <- function(m, f) ov[[f]][ov$measure == m]
    z <- readRDS(file.path(dir, "HC0_crossover.rds")); rt <- as.data.table(z$roots)
    mr <- rt[kind == "crossing" & age >= 20 & age <= 40]; if (nrow(mr) > 1) mr <- mr[which.max(regular_delta)]
    if (!nrow(mr)) { s <- rt[kind == "crossing" & regular_delta == TRUE]; if (nrow(s) == 1) mr <- s }
    sr <- z$simultaneous$sign_regions
    a <- fread(file.path(dir, "HC0_age_standardized.csv")); a[, model_id := id]
    list(summary = data.table(model_id = id, label = label, note = note, fit_n = fit_n, events = events, A0 = n0, A1 = n1, reference_n = reference_n,
      risk0_per1000 = 1000 * at("risk0", "estimate"), risk0_lower_per1000 = 1000 * at("risk0", "lower"), risk0_upper_per1000 = 1000 * at("risk0", "upper"),
      risk1_per1000 = 1000 * at("risk1", "estimate"), risk1_lower_per1000 = 1000 * at("risk1", "lower"), risk1_upper_per1000 = 1000 * at("risk1", "upper"),
      rr = at("rr", "estimate"), rr_lower = at("rr", "lower"), rr_upper = at("rr", "upper"),
      rd_per1000 = 1000 * at("rd", "estimate"), rd_lower_per1000 = 1000 * at("rd", "lower"), rd_upper_per1000 = 1000 * at("rd", "upper"),
      fitted_null_ages = paste(sprintf("%.3f", z$roots$age), collapse = "; "), n_roots = nrow(z$roots),
      unique_regular_interior_root = isTRUE(z$root_summary$unique_regular_interior_root),
      full_95_fixed_null_age_set = fmtset(z$pointwise$confidence_set),
      simultaneous_strict_negative = fmtset(subset(sr, classification == "strict_negative")),
      simultaneous_strict_positive = fmtset(subset(sr, classification == "strict_positive")),
      simultaneous_reversal = isTRUE(z$simultaneous$both_signs_demonstrated),
      omnibus_statistic = z$omnibus$statistic, omnibus_df = z$omnibus$df, omnibus_p = z$omnibus$p_value,
      crossover_age = if (nrow(mr)) mr$age else NA_real_, delta_lower = if (nrow(mr)) mr$delta_lower else NA_real_,
      delta_upper = if (nrow(mr)) mr$delta_upper else NA_real_, regular_delta = if (nrow(mr)) isTRUE(mr$regular_delta) else NA), ages = a)
  }
  rows <- list()
  # ---- joint model -------------------------------------------------------------------------------------
  r <- verify(file.path(joint, "receipt.json"), "complete_joint_three_group_fit")
  nb <- r$n_by_group; eb <- r$events_by_group
  rd <- verify(file.path(joint, "ss_vs_sn", "receipt.json"), "complete_joint_ss_vs_sn_derived")
  stopifnot(identical(rd$fit_receipt_signature, r$input_signature))
  rows$ssn <- one("joint_ss_vs_sn", "SS vs SN (joint three-group model)", file.path(joint, "ss_vs_sn"), nb$SN, nb$SS, eb$SN + eb$SS, r$fit_n, r$fit_n,
    "joint model; SN reference; standardized to the pooled SS, SN, NN population; crossover from the difference of the two coefficient blocks")
  rows$ss <- one("joint_ss_vs_nn", "SS vs NN (joint three-group model)", file.path(joint, "ss_vs_nn"), nb$NN, nb$SS, eb$NN + eb$SS, r$fit_n, r$fit_n,
    "joint model; NN reference; standardized to the pooled SS, SN, NN population")
  rows$sn <- one("joint_sn_vs_nn", "SN vs NN (joint three-group model; supplementary)", file.path(joint, "sn_vs_nn"), nb$NN, nb$SN, eb$NN + eb$SN, r$fit_n, r$fit_n,
    "joint model; NN reference; supplementary contrast, not a main display")
  # ---- released main fits (read-only) ------------------------------------------------------------------
  rel <- function(id, label, note) {
    dir <- file.path("age45_revision/outputs/models", id); rr <- verify(file.path(dir, "receipt.json"), "complete_validated_age15_45_fit")
    one(id, label, dir, rr$A0, rr$A1, rr$events, rr$fit_n, rr$reference_n, note)
  }
  rows$pre <- rel("prepregnancy_main", "S vs N (any smoking before pregnancy vs none, irrespective of T1)", "released main fit; N reference")
  rows$pri <- rel("primary_main", "SS vs SN, women who smoked before pregnancy (supplementary)", "released fit; SN reference; standardized to women who smoked before pregnancy")
  rows$bro <- rel("broad_main", "SS vs NN, standardized to the SS and NN records only", "released fit; superseded in the main text by the joint model")
  # ---- one reference per model (protocol addendum K): SS for the primary model, S before pregnancy ----
  # Exact re-expressions written by 39_flip_reference.R; the standardized risks and crossovers are the
  # same numbers, only the reference label and the direction of RR and RD change.
  fl <- function(id, label, dir, n_ref, n_cmp, events, fit_n, note) {
    if (!file.exists(file.path(dir, "receipt.json"))) { message("flipped contrast ", id, " not present"); return(NULL) }
    verify(file.path(dir, "receipt.json"), "complete_flipped_contrast")
    one(id, label, dir, n_ref, n_cmp, events, fit_n, fit_n, note)
  }
  rows$f_sn <- fl("sn_vs_ss", "SN compared with SS (primary model)", file.path(joint, "sn_vs_ss"),
    nb$SS, nb$SN, eb$SN + eb$SS, r$fit_n, "addendum K; SS is the reference; standardized to the pooled SS, SN, NN population")
  rows$f_nn <- fl("nn_vs_ss", "NN compared with SS (primary model)", file.path(joint, "nn_vs_ss"),
    nb$SS, nb$NN, eb$NN + eb$SS, r$fit_n, "addendum K; SS is the reference; standardized to the pooled SS, SN, NN population")
  pre_r <- jsonlite::fromJSON("age45_revision/outputs/models/prepregnancy_main/receipt.json")
  rows$f_n <- fl("n_vs_s", "N compared with S (before-pregnancy model)", "age45_revision/outputs/supplementary/flipped/n_vs_s",
    pre_r$A1, pre_r$A0, pre_r$events, pre_r$fit_n, "addendum K; S is the reference; released before-pregnancy fit re-expressed")
  # ---- first-trimester margin (protocol addendum J), supplementary ------------------------------------
  t1dir <- "age45_revision/outputs/supplementary/t1_only"
  if (file.exists(file.path(t1dir, "receipt.json"))) {
    rt <- verify(file.path(t1dir, "receipt.json"), "complete_t1_only_fit")
    rows$t1 <- one("t1_only", "T1 smoking vs none in T1, irrespective of before-pregnancy smoking (supplementary)",
      t1dir, rt$A0, rt$A1, rt$events, rt$fit_n, rt$fit_n,
      "addendum J; first-trimester margin; reference pools SN with NN; before-pregnancy smoking not adjusted for")
  } else message("t1_only outputs not present; the first-trimester margin is omitted from the summary")
  # Per-contrast age_arm_support.csv (A = 1 exposed, 0 = NN) in the engine's layout, so the bias
  # analyses can read a joint contrast exactly as they read a released model folder.
  sup <- fread(file.path(joint, "age_group_support.csv"))
  for (g in c("SS", "SN")) {
    z <- sup[group %in% c("NN", g)]; z[, A := as.integer(group == g)]
    z <- z[order(age, A), .(age, A, n, events, non_events, sparse)]
    fwrite(z, file.path(joint, if (g == "SS") "ss_vs_nn" else "sn_vs_nn", "age_arm_support.csv"))
  }
  summary <- rbindlist(lapply(rows, `[[`, "summary")); ages <- rbindlist(lapply(rows, `[[`, "ages"), fill = TRUE)
  fwrite(summary, file.path(out, "main_summary.csv")); fwrite(ages, file.path(out, "main_age_estimates.csv"))
  file.copy(file.path(joint, "HC0_three_group_curves.csv"), file.path(out, "three_group_curves.csv"), overwrite = TRUE)
  files <- list.files(out, full.names = TRUE)
  age45_json(list(status = "complete_joint_three_group_summary", joint_receipt_signature = r$input_signature,
    validated_against_pair = r$validated_against_pair, rows = summary$model_id,
    outputs = data.frame(path = files, sha256 = vapply(files, age45_sha, ""))), file.path(out, "receipt.json"))
  print(summary[, .(model_id, rr = round(rr, 4), rd_per1000 = round(rd_per1000, 2), crossover_age = round(crossover_age, 2),
    delta_lower = round(delta_lower, 2), delta_upper = round(delta_upper, 2), simultaneous_reversal)])
  invisible(summary)
}
if (sys.nframe() == 0L) { a <- commandArgs(trailingOnly = TRUE); if (length(a)) main(a[1]) else main() }
