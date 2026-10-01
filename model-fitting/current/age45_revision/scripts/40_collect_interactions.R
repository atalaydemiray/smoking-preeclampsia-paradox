# Aggregate-only reporting; run after the two separate 40_interaction_revision.R processes.
# Rscript --vanilla age45_revision/scripts/40_collect_interactions.R
main <- function() {
  source("age45_revision/R/fit_helpers.R"); age45_initialize()
  suppressPackageStartupMessages(library(data.table))
  ROOT <- "age45_revision/outputs/supplementary/interaction_revision_20261001"
  SELF <- "age45_revision/scripts/40_collect_interactions.R"
  rows <- missing <- populations <- inputs <- list()
  for (population in c("joint", "prepregnancy")) {
    out <- file.path(ROOT, population); rp <- file.path(out, "receipt.json")
    r <- jsonlite::fromJSON(rp)
    stopifnot(r$status == "complete_validated_interaction_revision", r$validation_passed,
      r$full_record_likelihood_score_covariance_verified)
    for (i in seq_len(nrow(r$outputs))) stopifnot(age45_sha(r$outputs$path[i]) == r$outputs$sha256[i])
    age45_json(list(status = "complete_validated_interaction_revision", population = population,
      authoritative_receipt = rp, receipt_sha256 = age45_sha(rp)), file.path(out, "running.json"))
    rows[[population]] <- fread(file.path(out, "interaction_tests.csv"))
    missing[[population]] <- fread(file.path(out, "missingness_overall.csv"))
    populations[[population]] <- fread(file.path(out, "population_overall.csv"))[, population := population]
    inputs[[population]] <- data.table(path = rp, sha256 = age45_sha(rp))
  }
  tab <- rbindlist(rows)
  tab[, `:=`(multiplicity_family = NA_character_, family_size = NA_integer_,
    holm_log_p = NA_real_, holm_p_value = NA_real_, holm_p_display = NA_character_)]
  # Holm evaluated on log-tail probabilities, preserving values below floating-point range.
  holm_log <- function(logp) {
    ord <- order(logp); n <- length(logp)
    ans <- numeric(n); ans[ord] <- pmin(0, cummax(logp[ord] + log(n:1L))); ans
  }
  families <- list(
    "Two global age-interaction questions: model-based LRT" = which(tab$test == "Likelihood ratio (model based)"),
    "Two global age-interaction questions: HC0 Wald" = which(tab$test == "Omnibus Wald (HC0)"),
    "Three exploratory contrast-specific age-interaction questions within the joint model" = which(tab$test == "Contrast Wald (HC0; exploratory)"))
  stopifnot(length(families[[1]]) == 2L, length(families[[2]]) == 2L, length(families[[3]]) == 3L)
  for (family in names(families)) {
    ix <- families[[family]]; lp <- holm_log(tab$log_p_value[ix])
    tab[ix, `:=`(multiplicity_family = family, family_size = length(ix), holm_log_p = lp,
      holm_p_value = exp(lp), holm_p_display = ifelse(lp < log(.001), "<0.001", sprintf("%.3f", exp(lp))))]
  }
  fwrite(tab, file.path(ROOT, "interaction_tests.csv"))
  wide_lrt <- tab[test == "Likelihood ratio (model based)", .(population, n, events,
    lrt_statistic = statistic, lrt_df = df, lrt_p = p_value, lrt_log_p = log_p_value,
    lrt_p_display = p_display, lrt_holm_p = holm_p_value, lrt_holm_log_p = holm_log_p,
    lrt_holm_p_display = holm_p_display)]
  wide_wald <- tab[test == "Omnibus Wald (HC0)", .(population,
    wald_hc0_statistic = statistic, wald_hc0_df = df, wald_hc0_p = p_value,
    wald_hc0_log_p = log_p_value, wald_hc0_p_display = p_display,
    wald_hc0_holm_p = holm_p_value, wald_hc0_holm_log_p = holm_log_p,
    wald_hc0_holm_p_display = holm_p_display)]
  global <- merge(wide_lrt, wide_wald, by = "population", sort = FALSE)
  global[, holmcheck := lrt_holm_log_p < log(.001) & wald_hc0_holm_log_p < log(.001)]
  fwrite(global, file.path(ROOT, "global_interaction_tests.csv"))
  fwrite(rbindlist(missing), file.path(ROOT, "missingness_overall.csv"))
  fwrite(rbindlist(populations, use.names = TRUE), file.path(ROOT, "population_overall.csv"))
  note <- c(
    "Global interaction hypotheses comprise 8 smoking-by-age spline coefficients in the joint SS/SN/NN model and 4 in the binary S/N model; exposure intercepts are not tested.",
    "LRT and HC0 omnibus Wald statistics address the same model-scale null. Holm adjustment is applied separately across the two global hypotheses for each inferential method; it does not combine LRT and Wald tests as independent evidence.",
    "The three contrast-specific HC0 Wald tests are exploratory; their additional Holm column addresses those three comparisons within the joint model.",
    "Saved p_value or holm_p_value can underflow numerically to zero. Use finite log_p_value/holm_log_p for computations and p_display/holm_p_display for publication; never report P=0.",
    "Interaction significance does not itself establish reversal or test a crossover age. Existing modelwise simultaneous sign inference and local crossover intervals answer separate questions and are not jointly adjusted across the three published contrasts.",
    "The paper's standardized risks, risk ratios, risk differences, and crossover ages are unchanged because the verified full models were reused. Only the nested reduced models are newly fitted.",
    "Missingness denominators are each model's exposure-eligible population before covariate complete-case selection. Unknown counts overlap across variables and must not be added.")
  writeLines(note, file.path(ROOT, "INTERPRETATION_AND_MULTIPLICITY.txt"))
  outputs <- file.path(ROOT, c("interaction_tests.csv", "global_interaction_tests.csv", "missingness_overall.csv", "population_overall.csv", "INTERPRETATION_AND_MULTIPLICITY.txt"))
  age45_json(list(status = "complete_validated_interaction_collection", generated = as.character(Sys.time()),
    code = data.frame(path = SELF, sha256 = age45_sha(SELF)), inputs = rbindlist(inputs),
    reporting_clarification = data.frame(path = "age45_revision/protocol/INTERACTION_AMENDMENT_2026-10-01_REPORTING_CLARIFICATION.md",
      sha256 = age45_sha("age45_revision/protocol/INTERACTION_AMENDMENT_2026-10-01_REPORTING_CLARIFICATION.md")),
    outputs = data.frame(path = outputs, sha256 = vapply(outputs, age45_sha, ""))), file.path(ROOT, "receipt.json"))
  print(tab[, .(population, test, statistic, df, p_display, holm_p_display)])
}
if (sys.nframe() == 0L) main()
