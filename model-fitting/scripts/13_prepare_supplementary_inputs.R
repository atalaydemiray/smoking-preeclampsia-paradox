# Prepare the two extended population sets used by the supplementary analyses
# (supplementary amendment, 11 September 2026). Same covariate construction
# as the main models; adds early-smoking flags built from the raw certificate
# fields. Writes only to derived/supplementary_inputs.
# From model-fitting/: Rscript --vanilla scripts/13_prepare_supplementary_inputs.R
main <- function(years = 2024:2016) {
  library(data.table); setDTthreads(1L)
  for (f in c("01_measurement_helpers", "05_model_specification"))
    source(file.path("R", paste0(f, ".R")), local = environment())
  sha <- function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE)
  writej <- function(x, p) jsonlite::write_json(x, p, pretty = TRUE, auto_unbox = TRUE, digits = 16, null = "null")
  out <- "derived/supplementary_inputs"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
  flag <- function(x) { x <- as.character(x); ifelse(x %in% "1", "reported", ifelse(x %in% "0", "not_reported", "unknown")) }
  dose_cat <- function(code) {
    # code: raw certificate value 0-98 (99 already NA); 98 is the top code.
    factor(ifelse(is.na(code), NA, ifelse(code == 0, "0", ifelse(code <= 5, "1-5",
      ifelse(code <= 10, "6-10", ifelse(code <= 20, "11-20", "21+"))))),
      levels = c("0", "1-5", "6-10", "11-20", "21+"))
  }
  add_flags <- function(p, d) {
    at <- p$identity$input_row
    dec <- function(k) decode_cigarettes(as.character(d[[paste0("source_cig_", k)]][at]),
      flag(d[[paste0("source_f_cigs_", k)]][at]), paste0("cig_", k))
    pre <- dec(0); t1 <- dec(1); t2 <- dec(2); t3 <- dec(3)
    id <- p$identity
    id$oe_weeks <- as.numeric(d$oe_weeks[at])
    id$pre_smoking <- pre$smoking; id$t1_smoking <- t1$smoking
    id$t2_smoking <- t2$smoking; id$t3_smoking <- t3$smoking
    id$pre_dose_cat <- dose_cat(pre$raw_code * ifelse(is.na(pre$smoking), NA, 1))
    id$t1_dose_cat <- dose_cat(t1$raw_code * ifelse(is.na(t1$smoking), NA, 1))
    id$later_positive <- (t2$smoking %in% TRUE) | (t3$smoking %in% TRUE)
    id$all_four_known <- !is.na(pre$smoking) & !is.na(t1$smoking) & !is.na(t2$smoking) & !is.na(t3$smoking)
    id$all_four_positive <- id$all_four_known & pre$smoking & t1$smoking & t2$smoking & t3$smoking
    id$all_four_zero <- id$all_four_known & !pre$smoking & !t1$smoking & !t2$smoking & !t3$smoking
    # Stopping time with missing treated as not positive (never requires reaching a week).
    id$timing <- ifelse(!(pre$smoking %in% TRUE), NA,
      ifelse(t1$smoking %in% FALSE & !id$later_positive, "stopped_by_T1",
      ifelse(t1$smoking %in% TRUE & !(t2$smoking %in% TRUE) & !(t3$smoking %in% TRUE), "stopped_in_T2",
      ifelse(t1$smoking %in% TRUE & t2$smoking %in% TRUE & !(t3$smoking %in% TRUE), "stopped_in_T3",
      ifelse(t1$smoking %in% TRUE & t2$smoking %in% TRUE & t3$smoking %in% TRUE, "continued_through", "other")))))
    # Dose change among women who smoked before pregnancy with both doses known.
    both <- !is.na(pre$raw_code) & !is.na(t1$raw_code) & pre$smoking %in% TRUE & !is.na(t1$smoking)
    id$dose_change <- ifelse(!both, NA, ifelse(t1$raw_code == 0, "stopped",
      ifelse(t1$raw_code < pre$raw_code, "reduced", "same_or_more")))
    p$identity <- id
    p
  }
  sets <- list(prepregnancy_ext = "prepregnancy_only", within_ext = "within_prepregnancy_smokers")
  for (y in years) {
    rp <- file.path("derived/natality", paste0(y, "_receipt.json"))
    r <- jsonlite::fromJSON(rp)
    stopifnot(r$status == "complete_package_import_old_age_reconciled")
    file <- r$outputs$path[grepl("clinical[.]rds$", r$outputs$path)]
    stopifnot(length(file) == 1L, sha(file) == r$outputs$sha256[match(file, r$outputs$path)])
    d <- readRDS(file)$data
    for (set in names(sets)) {
      stem <- file.path(out, paste(y, set, sep = "_"))
      if (file.exists(paste0(stem, "_receipt.json"))) {
        old <- jsonlite::fromJSON(paste0(stem, "_receipt.json"))
        stopifnot(old$input_sha256 == sha(file), sha(paste0(stem, ".rds")) == old$output_sha256)
        message(y, " ", set, ": verified existing prepared input reused"); next
      }
      spec <- lock_sep_model_specification("natality_main", sets[[set]], 2016:2024, c(21, 27, 35), c(19.6, 26.1, 37.8))
      p <- prepare_sep_model_data(d, spec, "natality")
      p <- add_flags(p, d)
      stopifnot(nrow(p$data) == nrow(p$identity), all(p$data$age %in% 15:45), p$baseline_records_dropped == 0)
      if (set == "within_ext") stopifnot(all(p$identity$pre_smoking), !anyNA(p$identity$t1_smoking),
        all((p$data$A == 1) == p$identity$t1_smoking))
      if (set == "prepregnancy_ext") stopifnot(!anyNA(p$identity$pre_smoking), all((p$data$A == 1) == p$identity$pre_smoking))
      cc <- complete.cases(p$data[spec$covariates])
      p$construction <- list(amendment = "SUPPLEMENTARY_AMENDMENT_2026-09-11", input_sha256 = sha(file),
        no_imputation = TRUE, flags = c("pre/t1/t2/t3_smoking", "pre_dose_cat", "t1_dose_cat",
        "later_positive", "all_four_known", "all_four_positive", "all_four_zero", "timing", "dose_change"))
      path <- paste0(stem, ".rds"); saveRDS(p, path, compress = "gzip")
      stopifnot(isTRUE(all.equal(readRDS(path), p, tolerance = 0, check.attributes = TRUE)))
      id <- as.data.table(p$identity)
      counts <- list(eligible_n = nrow(p$data), cc_n = sum(cc), cc_events = sum(p$data$Y[cc]),
        t1_known_n = sum(!is.na(id$t1_smoking)), later_positive_n = sum(id$later_positive),
        all_four_known_n = sum(id$all_four_known), timing = as.list(table(id$timing, useNA = "ifany")),
        t1_dose = as.list(table(id$t1_dose_cat, useNA = "ifany")), pre_dose = as.list(table(id$pre_dose_cat, useNA = "ifany")),
        dose_change = as.list(table(id$dose_change, useNA = "ifany")))
      writej(list(status = "complete_prepared_supplementary", year = y, set = set, contrast = sets[[set]],
        counts = counts, input_sha256 = sha(file), output_sha256 = sha(path), no_imputation = TRUE,
        age_boundaries = spec$age_spec$boundaries, age_knots = spec$age_spec$knots,
        code_sha256 = list(script = sha("scripts/13_prepare_supplementary_inputs.R"),
          model_specification = sha("R/05_model_specification.R"),
          measurement_helpers = sha("R/01_measurement_helpers.R"))),
        paste0(stem, "_receipt.json"))
      message(y, " ", set, ": eligible=", counts$eligible_n, "; CC=", counts$cc_n, "; T1 known=", counts$t1_known_n)
      rm(p); gc(FALSE)
    }
    rm(d); gc(FALSE)
  }
  invisible(TRUE)
}
if (sys.nframe() == 0L) {
  a <- commandArgs(TRUE)
  main(if (length(a)) as.integer(strsplit(a[1], ",", fixed = TRUE)[[1]]) else 2024:2016)
}
