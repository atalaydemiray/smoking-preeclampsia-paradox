source("R/01_measurement_helpers.R")
source("R/09_fetal_clinical_adapter.R")
s <- fetal_schema_with_provenance_r(jsonlite::fromJSON("config/fetal_schema.json",simplifyVector=FALSE))
checks <- 0L
check <- function(x) { stopifnot(isTRUE(x)); checks <<- checks+1L }
fails <- function(expr) inherits(tryCatch({force(expr);NULL},error=identity),"error")
fixture <- function(year=2024L,n=8L) {
  d <- as.data.frame(setNames(lapply(s$fields, function(f) {
    z <- s$domains[[f$kind]]
    value <- if (!is.null(z$allowed)) unlist(z$allowed)[1] else as.character(z$known_min)
    rep(value,n)
  }), names(s$fields)),stringsAsFactors=FALSE)
  for (f in names(d)[startsWith(names(d),"F_")]) d[[f]] <- "1"
  d$DOD_YY <- as.character(year); d$MAGER <- "30"; d$OE_GEST <- "38"
  d$OE_TABFLG <- "2"; d$PHYP <- "N"; d$GHYP <- "N"; d$PDIAB <- "N"
  d$CIG0 <- "10"; d$CIG1 <- "05"; d$BMI <- "27.3"
  d$INFTR <- "N"; d$ART <- "X"
  for (f in unlist(s$years[[as.character(year)]]$unavailable_fields)) d[[f]] <- NA_character_
  d
}
d <- fixture(); a <- adapt_fetal_r(d,s,2024)
check(all(a$data$primary_member)); check(all(a$data$primary_t1_smoking==1L))
check(all(a$data$prepregnancy_bmi==27.3)); check(all(a$data$art_parent_aware==0L))
check(all(a$data$source_cig0=="10")); check(all(a$data$race_imputation_status=="not_imputed"))
e <- d; e$CIG3 <- "99"; e$F_CIG3 <- "0"; b <- adapt_fetal_r(e,s,2024)
check(identical(a$data$primary_member,b$data$primary_member))
check(identical(a$data$early_pattern,b$data$early_pattern))
e <- d; e$GHYP[1] <- "U"; e$CIG0[2] <- "99"; e$F_PHYP[3] <- "0"
e$CIG0[4] <- "98"; e$CIG1[4] <- "00"
b <- adapt_fetal_r(e,s,2024)$data
check(!b$primary_member[1] && b$mortality_member[1])
check(!b$primary_member[2] && !b$mortality_member[2] && b$clinical_member[2])
check(!b$clinical_member[3]); check(b$pre_topcoded[4] && is.na(b$pre_exact_cigarettes[4]))
check(b$primary_t1_smoking[4]==0L && b$pre_dose_lower_bound[4]==98)
e <- fixture(2018); e$MRACEIMP[5] <- "3"; b <- adapt_fetal_r(e,s,2018)$data
check(b$race_imputation_status[5]=="undocumented_source_code" && b$primary_member[5])
e <- fixture(2018); e$F_WEIGHT <- "0"
b <- adapt_fetal_r(e,s,2018)$data
check(all(b$prepregnancy_bmi==27.3)); check(all(!b$bmi_both_component_flags_reported))
e <- fixture(2019); e$F_HEIGHT <- "0"
check(all(adapt_fetal_r(e,s,2019)$data$prepregnancy_bmi==27.3))
e$F_WEIGHT <- "0"; check(all(is.na(adapt_fetal_r(e,s,2019)$data$prepregnancy_bmi)))
e <- fixture(); e$F_BMI <- "0"; check(all(is.na(adapt_fetal_r(e,s,2024)$data$prepregnancy_bmi)))
e <- fixture(); e$BMI <- "99.9"; check(all(is.na(adapt_fetal_r(e,s,2024)$data$prepregnancy_bmi)))
e <- fixture(); e$PHYP[1] <- "Z"; check(fails(adapt_fetal_r(e,s,2024)))
e <- fixture(); e$BMI[1] <- "bogus"; e$F_BMI[1] <- "0"
check(fails(adapt_fetal_r(e,s,2024)))
e <- fixture(); e$BMI[1] <- "70"; check(fails(adapt_fetal_r(e,s,2024)))
e <- fixture(); e$F_EDUCATION[1] <- "Z"; check(fails(adapt_fetal_r(e,s,2024)))
e <- fixture(); e$MAGE_IMPFLG[1] <- "1"
check(adapt_fetal_r(e,s,2024)$data$age_imputation_status[1]=="imputed")
check(identical(fetal_binary_provenance_r(c(NA,"","1","0","9")),
  c("source_missing","raw_blank","raw_code1","undocumented_source_code","undocumented_source_code")))
e <- fixture(); e$IMP_PLUR[1] <- "9"; check(fails(adapt_fetal_r(e,s,2024)))
e <- fixture(); e$COMBGEST_USED[1] <- "0"; check(fails(adapt_fetal_r(e,s,2024)))
e <- fixture(); e$DPLURAL[1] <- "9"; z <- adapt_fetal_r(e,s,2024)$data
check(!z$clinical_member[1] && grepl("undocumented_code9",z$plurality_source_status[1]))
e$DPLURAL[1] <- "8"; check(fails(adapt_fetal_r(e,s,2024)))
e <- fixture(2018); e$DPLURAL[1] <- "5"
check(!adapt_fetal_r(e,s,2018)$data$clinical_member[1])
e$DOD_YY <- "2019"; check(fails(adapt_fetal_r(e,s,2019)))
check(fails(fetal_parse_lines_r("short",s,2024)))
for (year in 2018:2024) {
  d <- fixture(year,1L); layout <- fetal_layout_r(s,year)
  line <- strrep(" ",s$years[[as.character(year)]]$payload_bytes)
  for (f in names(layout)) {
    z <- layout[[f]]; value <- d[[f]]; value <- paste0(strrep(" ",z$end-z$start+1-nchar(value)),value)
    substr(line,z$start,z$end) <- value
  }
  parsed <- fetal_parse_lines_r(line,s,year)
  check(identical(parsed[names(layout)],d[names(layout)]))
}
cat("Passed",checks,"synthetic R fetal clinical checks; no study data read.\n")
