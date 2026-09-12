# Aggregate completed supplementary fits (14_fit_supplementary.R) into report CSVs.
# No fitting. Verifies every receipt hash before reading. Writes to
# outputs/supplementary/report.
# From model-fitting/: Rscript --vanilla scripts/16_assemble_supplementary.R
main <- function() {
  source("R/17_fit_helpers.R"); age45_initialize()
  suppressMessages(library(data.table))
  base <- "outputs/supplementary/models"; out <- "outputs/supplementary/report"
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  fmtset <- function(x) if (!nrow(x)) "Empty" else paste0(ifelse(x$lower_closed, "[", "("), sprintf("%.3f", x$lower), ", ",
    sprintf("%.3f", x$upper), ifelse(x$upper_closed, "]", ")"), collapse = " U ")
  ids <- sort(list.dirs(base, full.names = FALSE, recursive = FALSE))
  overall <- ages <- roots <- signs <- list(); skipped <- character()
  for (id in ids) {
    rp <- file.path(base, id, "receipt.json")
    if (!file.exists(rp)) { skipped <- c(skipped, id); next }
    r <- jsonlite::fromJSON(rp)
    if (r$status != "complete_validated_supplementary_fit") { skipped <- c(skipped, id); next }
    for (i in seq_len(nrow(r$outputs))) stopifnot(age45_sha(r$outputs$path[i]) == r$outputs$sha256[i])
    ov <- fread(file.path(base, id, "HC0_overall_standardized.csv"))
    at <- function(m, f) ov[[f]][ov$measure == m]
    overall[[id]] <- data.table(model_id = id, note = r$note, eligible_n = r$eligible_n, fit_n = r$fit_n, events = r$events,
      A0 = r$A0, A1 = r$A1, reference_n = r$reference_n,
      risk0_per1000 = 1000 * at("risk0", "estimate"), risk0_lower_per1000 = 1000 * at("risk0", "lower"), risk0_upper_per1000 = 1000 * at("risk0", "upper"),
      risk1_per1000 = 1000 * at("risk1", "estimate"), risk1_lower_per1000 = 1000 * at("risk1", "lower"), risk1_upper_per1000 = 1000 * at("risk1", "upper"),
      rr = at("rr", "estimate"), rr_lower = at("rr", "lower"), rr_upper = at("rr", "upper"),
      rd_per1000 = 1000 * at("rd", "estimate"), rd_lower_per1000 = 1000 * at("rd", "lower"), rd_upper_per1000 = 1000 * at("rd", "upper"),
      covariates = paste(r$covariates, collapse = "; "), root_finding = isTRUE(r$root_finding), elapsed_seconds = r$elapsed_seconds)
    a <- fread(file.path(base, id, "HC0_age_standardized.csv")); a[, model_id := id]; ages[[id]] <- a
    if (isTRUE(r$root_finding)) {
      z <- readRDS(file.path(base, id, "HC0_crossover.rds"))
      rt <- as.data.table(z$roots); rt[, model_id := rep(id, .N)]; roots[[id]] <- rt
      sr <- z$simultaneous$sign_regions
      signs[[id]] <- data.table(model_id = id, fitted_null_ages = paste(sprintf("%.3f", z$roots$age), collapse = "; "),
        n_roots = nrow(z$roots), unique_regular_interior_root = z$root_summary$unique_regular_interior_root,
        full_95_fixed_null_age_set = fmtset(z$pointwise$confidence_set),
        simultaneous_strict_negative = fmtset(subset(sr, classification == "strict_negative")),
        simultaneous_strict_positive = fmtset(subset(sr, classification == "strict_positive")),
        simultaneous_reversal = z$simultaneous$both_signs_demonstrated,
        omnibus_statistic = z$omnibus$statistic, omnibus_df = z$omnibus$df, omnibus_p = z$omnibus$p_value)
    } else {
      # Sign brackets from the age grid when root finding was not run.
      s <- sign(a$rd); i <- which(diff(s) != 0)
      signs[[id]] <- data.table(model_id = id, fitted_null_ages = if (length(i)) paste(sprintf("%d to %d", a$age[i], a$age[i] + 1L), collapse = "; ") else "none",
        n_roots = NA_integer_, unique_regular_interior_root = NA, full_95_fixed_null_age_set = NA_character_,
        simultaneous_strict_negative = { x <- a$age[a$rd_bonferroni_upper_per1000 < 0]; if (length(x)) paste0("Bonferroni ages ", min(x), "-", max(x)) else "none" },
        simultaneous_strict_positive = { x <- a$age[a$rd_bonferroni_lower_per1000 > 0]; if (length(x)) paste0("Bonferroni ages ", min(x), "-", max(x)) else "none" },
        simultaneous_reversal = NA, omnibus_statistic = NA_real_, omnibus_df = NA_integer_, omnibus_p = NA_real_)
    }
  }
  overall <- rbindlist(overall); ages <- rbindlist(ages, fill = TRUE); roots <- rbindlist(roots, fill = TRUE); signs <- rbindlist(signs, fill = TRUE)
  main_root <- if (nrow(roots)) roots[age >= 20 & age <= 40 & kind == "crossing", .(model_id, crossover_age = age, delta_lower, delta_upper, regular_delta, status)] else
    data.table(model_id = character(), crossover_age = numeric(), delta_lower = numeric(), delta_upper = numeric(), regular_delta = logical(), status = character())
  if (anyDuplicated(main_root$model_id)) { main_root <- main_root[, .SD[which.max(regular_delta)], by = model_id]; message("note: a model had more than one crossing in 20-40; the regular one is kept") }
  # A model whose only regular crossing lies outside 20-40 still has a well-defined crossover; report it.
  if (nrow(roots)) {
    single <- roots[kind == "crossing" & regular_delta == TRUE, .N, by = model_id][N == 1, model_id]
    extra <- roots[model_id %in% setdiff(single, main_root$model_id) & kind == "crossing" & regular_delta == TRUE,
                   .(model_id, crossover_age = age, delta_lower, delta_upper, regular_delta, status)]
    if (nrow(extra)) { main_root <- rbind(main_root, extra); message("note: ", nrow(extra), " model(s) with a single regular crossing outside 20-40 reported at that age") }
  }
  summary <- merge(merge(overall, signs, by = "model_id", all.x = TRUE), main_root, by = "model_id", all.x = TRUE)
  fwrite(summary, file.path(out, "supp_summary.csv"))
  fwrite(ages, file.path(out, "supp_age_estimates.csv"))
  fwrite(roots, file.path(out, "supp_roots.csv"))
  jsonlite::write_json(list(status = "assembled_supplementary", models = nrow(overall), skipped_incomplete = skipped,
    assembled = as.character(Sys.time())), file.path(out, "receipt.json"), pretty = TRUE, auto_unbox = TRUE)
  message("Assembled ", nrow(overall), " supplementary fits; skipped ", length(skipped), ": ", paste(skipped, collapse = ", "))
  invisible(summary)
}
if (sys.nframe() == 0L) main()
