#!/usr/bin/env Rscript
argv <- commandArgs(trailingOnly = FALSE)
self <- normalizePath(sub("^--file=", "", argv[grepl("^--file=", argv)]))
v2 <- dirname(dirname(self))
source(file.path(v2, "R", "01_measurement_helpers.R"))
source(file.path(v2, "R", "03_natality_adapter.R"))
checks <- 0L
check <- function(x) { stopifnot(isTRUE(x)); checks <<- checks + 1L }
fails <- function(expr) check(inherits(tryCatch(force(expr), error = identity), "error"))
fixture <- function(year = 2024, n = 10) {
  fields <- natality_selected_columns(year)
  x <- as.data.frame(setNames(rep(list(rep(1, n)), length(fields)), fields))
  x$dob_yy <- year; x$mager <- 30; x$oegest_comb <- 39
  x$imp_plur <- x$mage_impflg <- ""
  for (f in c("rf_phype", "rf_ghype", "rf_pdiab", "rf_ppterm", "rf_cesar", "rf_inftr")) x[[f]] <- "N"
  x$rf_artec <- "X"; x$bmi <- 24.1
  x$priorlive <- x$priordead <- 0
  for (f in paste0("cig_", 0:3)) x[[f]] <- 0
  x
}
d <- fixture()
a <- adapt_natality(d, 2024)
check(all(a$art$art == 0))
check(all(a$art$status == "structural_nonuse_parent_no"))
check(all(natality_ledger(a)$n_retained == 10))
check(all(a$baselines$prepregnancy_bmi$value == 24.1))
d$cig_0[1:4] <- c(98, 99, 0, 5); d$cig_1[1:4] <- c(0, 0, 99, 2)
a <- adapt_natality(d, 2024)
check(a$early$pattern[1] == "pre_smoking_t1_abstinence")
check(is.na(a$early$pre_exact_cigarettes[1]))
check(is.na(a$early$pattern[2]) && is.na(a$early$pattern[3]))
check(a$early$within_prepregnancy_smokers[4] == 1L)
future <- d; future$cig_2 <- 99; future$cig_3 <- 98; future$f_cigs_3 <- 0
check(identical(adapt_natality(future, 2024)$early, a$early))
future$rf_ghype <- "Y"; future$oegest_comb <- 20
check(identical(adapt_natality(future, 2024)$early, a$early))
d$f_cigs_0[1] <- 0; d$f_rf_phyper[2] <- 0; d$rf_phype[3] <- "U"
a <- adapt_natality(d, 2024)
check(is.na(a$early$pattern[1]))
check(!natality_masks(a)$known_no_prepregnancy_hypertension[2])
check(!natality_masks(a)$known_no_prepregnancy_hypertension[3])
d <- fixture(); d$oegest_comb[1:3] <- c(99, 19, 27)
a <- adapt_natality(d, 2024)
check(sum(natality_masks(a)$known_oe_at_least_threshold) == 8)
check(sum(natality_masks(a, 28)$known_oe_at_least_threshold) == 7)
d <- fixture(); d$rf_artec[1] <- "Y"; d$rf_inftr[2] <- "Y"
a <- adapt_natality(d, 2024)
check(sum(a$art$usable_parent_child_conflict) == 2)
check(all(is.na(a$art$art[1:2])))
check(all(natality_ledger(a)$n_retained == 10))
d <- fixture(2016); a <- adapt_natality(d, 2016)
check(a$missing_parent_flag)
check(all(is.na(a$art$art)))
check(all(is.na(a$baselines$infertility_treatment$value)))
check(all(natality_ledger(a)$n_retained == 10))
d$f_rf_inft <- 1
check(all(adapt_natality(d, 2016)$art$art == 0))
d <- fixture(2015); d$f_rf_pdiab[1] <- 0
check(adapt_natality(d, 2015)$art$status[1] == "not_reported")
d <- fixture(2019); d$dplural[1] <- 5
check(adapt_natality(d, 2019)$plurality$value[1] == 5)
d <- fixture(); d$dplural[1] <- 5; fails(adapt_natality(d, 2024))
d <- fixture(); d$f_cigs_1[1] <- 2; fails(adapt_natality(d, 2024))
d <- fixture(); d$rf_inftr[1] <- "X"; fails(adapt_natality(d, 2024))
d <- fixture(); d$cig_0[1] <- 100; fails(adapt_natality(d, 2024))
d <- fixture(); d$oegest_comb[1] <- 16; fails(adapt_natality(d, 2024))
d <- fixture(); d$bmi[1] <- 24.15; fails(adapt_natality(d, 2024))
d <- fixture(); d$meduc[1] <- 9
check(is.na(adapt_natality(d, 2024)$baselines$education$value[1]))
d <- fixture(); d$dob_yy[1] <- 2023; fails(adapt_natality(d, 2024))
fails(natality_source_spec(2013))
cat("PASS:", checks, "natality adapter synthetic checks.\n")
