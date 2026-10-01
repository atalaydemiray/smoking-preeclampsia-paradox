# R fetal-record adapter; raw evidence and model candidates stay separate.
# Source R/01_measurement_helpers.R first. No I/O or model fitting here.
fetal_schema_with_provenance_r <- function(schema) {
  schema$fields$MAGE_IMPFLG <- list(start=84L,end=84L,kind="imputed")
  schema$fields$MAGE_REPFLG <- list(start=85L,end=85L,kind="imputed")
  schema$r_provenance_extension <- list(review_date="2026-09-06",
    maternal_age_imputation_position=84L,maternal_reported_age_position=85L,
    one_based_guide_pages=c(`2018`=30L,`2019`=31L,`2020`=31L,`2021`=32L,
      `2022`=33L,`2023`=36L,`2024`=37L),
    note="Both fields documented in all annual guides; original JSON and prior audits retained unchanged")
  schema
}

fetal_binary_provenance_r <- function(x, one="raw_code1", blank="raw_blank") {
  out <- rep("undocumented_source_code",length(x))
  out[is.na(x)] <- "source_missing"
  out[!is.na(x) & x == ""] <- blank
  out[x %in% "1"] <- one
  out
}

fetal_layout_r <- function(schema, year) {
  y <- schema$years[[as.character(year)]]
  if (is.null(y)) stop("Unsupported fetal year.")
  layout <- schema$fields
  for (f in names(y$field_overrides)) layout[[f]] <- modifyList(layout[[f]], y$field_overrides[[f]])
  layout[setdiff(names(layout), unlist(y$unavailable_fields))]
}

fetal_parse_lines_r <- function(lines, schema, year) {
  annual <- schema$years[[as.character(year)]]
  if (anyNA(lines) || any(nchar(lines,type="bytes") != annual$payload_bytes))
    stop("Fetal payload length differs from pinned layout.")
  layout <- fetal_layout_r(schema, year)
  d <- as.data.frame(setNames(lapply(layout, function(f) trimws(substr(lines,f$start,f$end))),
    names(layout)), stringsAsFactors=FALSE)
  for (f in unlist(annual$unavailable_fields)) d[[f]] <- NA_character_
  if (any(d$DOD_YY != as.character(year))) stop("Fetal record year mismatch.")
  d[names(schema$fields)]
}

fetal_decode_r <- function(x, domain, flags=list()) {
  n <- length(x); status <- rep("observed",n); value <- x
  blank <- is.na(x) | x == ""
  status[blank] <- "unknown_blank"
  if (!is.null(domain$allowed)) {
    status[!blank & !x %in% unlist(domain$allowed)] <- "invalid_code"
    if ("" %in% unlist(domain$allowed)) status[!is.na(x) & x == ""] <- "observed"
  } else {
    syntax <- if (identical(domain$numeric_type,"float")) "^[0-9]+([.][0-9]+)?$" else "^[0-9]+$"
    value <- suppressWarnings(as.numeric(x))
    invalid <- !blank & (!grepl(syntax,x) | !is.finite(value) |
      value < domain$known_min | value > domain$known_max)
    status[invalid] <- "invalid_code"
  }
  status[x %in% unlist(domain$unknown)] <- "unknown_code"
  status[x %in% unlist(domain$not_applicable)] <- "not_applicable"
  # Nonreporting takes priority; original code still survives in source_ fields.
  if (length(flags)) {
    unavailable <- Reduce(`|`, lapply(flags, function(f) is.na(f) | f == ""))
    invalid <- Reduce(`|`, lapply(flags, function(f) !is.na(f) & !f %in% c("","0","1")))
    nonreport <- Reduce(`|`, lapply(flags, function(f) f %in% "0"))
    status[unavailable] <- "unknown_reporting_status"
    status[invalid] <- "invalid_reporting_flag"
    status[nonreport] <- "not_reported"
  }
  value[status != "observed"] <- NA
  if (!is.null(domain$allowed) && all(unlist(domain$allowed) %in% c("Y","N","U","X"))) {
    value <- ifelse(is.na(value),NA_integer_,as.integer(value == "Y"))
  }
  list(value=value,status=status)
}

adapt_fetal_r <- function(raw, schema, year, first_source_row=1L) {
  if (!is.data.frame(raw) || anyDuplicated(names(raw)) || any(!names(schema$fields) %in% names(raw)))
    stop("Incomplete fetal raw schema.")
  n <- nrow(raw)
  if (anyNA(raw$DOD_YY) || any(raw$DOD_YY != as.character(year))) stop("Fetal raw year mismatch.")
  dec <- function(f, flags=character()) {
    domain <- schema$domains[[schema$fields[[f]]$kind]]
    # 2018 guide permits5=quintuplet+; from2019 code4=quadruplet+.
    if (f == "DPLURAL" && year == 2018) domain$allowed <- as.character(1:5)
    fetal_decode_r(raw[[f]],domain,unname(raw[flags]))
  }
  # Raw-domain validation precedes reporting masks so a malformed raw value
  # cannot disappear behind flag0 and enter ordinary MI as unexplained NA.
  raw_issues <- list()
  for (f in names(schema$fields)) {
    bad <- dec(f)$status == "invalid_code"
    known_quarantine <- if (f == "DPLURAL") raw[[f]] %in% "9" else
      if (f == "MRACEIMP" && year == 2018) raw[[f]] %in% "3" else rep(FALSE,n)
    if (any(bad & !known_quarantine)) stop("Unresolved raw fetal domain violation: ",f)
    if (any(bad)) raw_issues[[f]] <- data.frame(field=f,code=raw[[f]][bad],
      disposition="documented_quarantine_not_reinterpreted",stringsAsFactors=FALSE)
  }
  age <- dec("MAGER","F_MAGE"); oe <- dec("OE_GEST","F_OE")
  chtn <- dec("PHYP","F_PHYP"); gh <- dec("GHYP","F_GHYP")
  core <- c("DOD_YY","RESTATUS","DPLURAL","MAGER","OE_GEST","PHYP","GHYP","CIG0","CIG1",
    "F_MAGE","F_OE","F_PHYP","F_GHYP","F_CIG0","F_CIG1")
  for (f in core) {
    z <- dec(f)
    bad <- z$status == "invalid_code"
    # Known raw code9 occurs outside the singleton population in every source
    # year, but is not defined in the reviewed DPLURAL legends. Quarantine and
    # count it; do not call it singleton, nonmissing, or a newly inferred value.
    if (f == "DPLURAL") bad <- bad & !raw$DPLURAL %in% "9"
    if (any(bad)) stop("Invalid core fetal source code: ",f)
  }
  normalized_flag <- function(x) ifelse(x %in% "1","reported",
    ifelse(x %in% "0","not_reported","unknown"))
  early <- classify_early_smoking(raw$CIG0,raw$CIG1,
    normalized_flag(raw$F_CIG0),normalized_flag(raw$F_CIG1))
  resident <- raw$RESTATUS %in% c("1","2","3")
  singleton <- resident & raw$DPLURAL %in% "1"
  age_ok <- singleton & !is.na(age$value) & age$value >= 15 & age$value <= 45
  oe_ok <- age_ok & !is.na(oe$value) & oe$value >= 20
  clinical <- oe_ok & chtn$value %in% 0
  early_known <- clinical & early$classification_status == "classified"
  broad <- early_known & !is.na(gh$value)
  primary <- broad & early$exposure_eligible
  complementary <- broad & early$pattern %in% c("no_reported_pre_or_t1_smoking","pre_and_t1_smoking")
  d <- data.frame(source_row=seq.int(first_source_row,length.out=n),year=rep(as.integer(year),n),
    source_type=rep("fetal_death",n),age=age$value,oe_weeks=oe$value,gh=gh$value,
    gh_status=gh$status,chtn_status=chtn$status,early_pattern=early$pattern,
    early_status=early$classification_status,clinical_member=clinical,
    primary_member=primary,broad_member=broad,complementary_member=complementary,
    mortality_member=early_known,primary_t1_smoking=early$within_prepregnancy_smokers,
    oe28_member=clinical & !is.na(oe$value) & oe$value >= 28,
    resident_surveillance_ge20=resident & raw$OE_TABFLG %in% "2",
    stringsAsFactors=FALSE)
  d$plurality_source_status <- ifelse(raw$DPLURAL %in% "9",
    "undocumented_code9_excluded_by_singleton_requirement",dec("DPLURAL")$status)
  for (f in names(early)[grepl("^(pre_|t1_)",names(early))]) d[[f]] <- early[[f]]
  mapping <- list(maternal_age=c("MAGER","F_MAGE"),
    race_hispanic_origin=c("RACE_ETHNICITY","F_RACE","F_HISPANIC"),
    education=c("EDUCATION","F_EDUCATION"),nativity="NATIVITY",
    live_birth_order_recode="LBO_REC",
    prior_liveborn_children_now_living=c("PRIORLIVE","F_PRIORLIVE"),
    prior_liveborn_children_now_dead=c("PRIORDEAD","F_PRIORDEAD"),
    prepregnancy_diabetes=c("PDIAB","F_PDIAB"),previous_cesarean=c("PCESAR","F_PCESAR"),
    infertility_treatment=c("INFTR","F_INFTR"),eclampsia=c("ECLAM","F_ECLAM"))
  for (f in names(mapping)) {
    spec <- mapping[[f]]; z <- dec(spec[1],spec[-1])
    if (f == "nativity") z$status[z$status == "observed"] <- "observed_reporting_support_unresolved"
    if (f %in% c("live_birth_order_recode","prior_liveborn_children_now_dead") && year >= 2022)
      z$status[z$status == "observed"] <- "observed_history_contamination_warning_not_primary_covariate"
    d[[f]] <- z$value; d[[paste0(f,"_status")]] <- z$status
  }
  # Literal BMI-row reporting pointers, not one inferred cross-year flag:
  # 2018:375/height; 2019-2021:376/weight; 2022+:445/dedicated BMI.
  # Retain both component flags for a stricter, separately named sensitivity.
  bmi_flags <- if (year >= 2022) "F_BMI" else if (year == 2018) "F_HEIGHT" else "F_WEIGHT"
  bmi <- dec("BMI",bmi_flags)
  d$prepregnancy_bmi <- bmi$value; d$prepregnancy_bmi_status <- bmi$status
  d$bmi_reporting_rule <- bmi_flags
  d$bmi_both_component_flags_reported <- raw$F_HEIGHT %in% "1" & raw$F_WEIGHT %in% "1"
  d$previous_preterm_birth <- NA_integer_
  d$previous_preterm_birth_status <- "structurally_unavailable"
  d$age_imputation_status <- fetal_binary_provenance_r(raw$MAGE_IMPFLG,"imputed","not_imputed")
  d$mage_repflg_status <- fetal_binary_provenance_r(raw$MAGE_REPFLG,"reported_age_used","reported_age_not_used")
  d$plurality_imputation_status <- fetal_binary_provenance_r(raw$IMP_PLUR)
  d$gestation_substitution_status <- fetal_binary_provenance_r(raw$COMBGEST_USED,"combined_estimate_used")
  d$race_imputation_status <- ifelse(is.na(raw$MRACEIMP),"source_missing",
    ifelse(raw$MRACEIMP == "","not_imputed",ifelse(raw$MRACEIMP == "1","unknown_race_imputed",
    ifelse(raw$MRACEIMP == "2","formerly_other_race_imputed","undocumented_source_code"))))
  d$race_any_imputed <- ifelse(d$race_imputation_status %in% c("source_missing","undocumented_source_code"),
    NA,d$race_imputation_status != "not_imputed")
  # Unknown, no, and contradictory ART stay separate; no wholesale exclusions.
  art_flag <- normalized_flag(raw$F_ART); inf_flag <- normalized_flag(raw$F_INFTR)
  contradiction <- raw$INFTR %in% "N" & raw$ART %in% "Y" |
    raw$INFTR %in% "Y" & raw$ART %in% "X"
  usable_conflict <- contradiction & art_flag == "reported" & inf_flag == "reported"
  art_flag[usable_conflict] <- "unknown"
  art <- decode_art(raw$ART,raw$INFTR,art_flag,inf_flag)
  art$status[usable_conflict] <- "contradictory_parent_child"
  d$art_parent_aware <- art$art; d$art_parent_aware_status <- art$status
  for (f in names(raw)) d[[paste0("source_",tolower(f))]] <- raw[[f]]
  masks <- list(source_records=rep(TRUE,n),us_residents=resident,singleton=singleton,
    maternal_age_15_45=age_ok,known_oe_at_least20=oe_ok,
    known_no_prepregnancy_hypertension=clinical,known_early_pattern=early_known,
    known_recorded_gh_pe=broad,primary_within_prepregnancy_smokers=primary)
  counts <- vapply(masks,sum,integer(1))
  ledger <- data.frame(year=year,stage=names(counts),n_entering=c(n,head(counts,-1)),
    n_retained=unname(counts),n_excluded=c(n,head(counts,-1))-counts,row.names=NULL)
  list(data=d,ledger=ledger,raw_quarantine=raw_issues)
}
