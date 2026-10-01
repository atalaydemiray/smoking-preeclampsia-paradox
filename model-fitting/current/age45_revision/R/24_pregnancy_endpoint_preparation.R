# Pure recorded fetal-death endpoint preparation. Requires audited R01/R10.
# No call to the GH-specific prepare_sep_model_data; GH is never a selection
# criterion, a substituted outcome, or an adjustment covariate here.
lock_pregnancy_endpoint_spec <- function(tier, years=2018:2024,
    age_knots=c(21,27,35), bmi_knots=c(19.6,26.1,37.8)) {
  tier <- match.arg(tier,c("shared_core","shared_augmented"))
  if (!identical(as.integer(years),2018:2024) &&
      !identical(as.integer(years),c(2019L,2020L,2021L,2024L)))
    stop("Explicit all-source or full-core-reporting-flag year set required")
  if (!identical(as.numeric(age_knots),c(21,27,35)) ||
      !identical(as.numeric(bmi_knots),c(19.6,26.1,37.8))) stop("Original fixed knots required")
  s <- lock_sep_model_specification(tier,"within_prepregnancy_smokers",years,
    age_knots,if(tier=="shared_core")NULL else bmi_knots,race_mode="edited")
  s$outcome_definition <- "recorded_fetal_death_source_indicator"
  s$estimand_label <- paste("Standardized recorded fetal-death association among selected",
    "pregnancy-ending certificates, not fetal loss from conception")
  s$forbidden_adjustments <- unique(c(s$forbidden_adjustments,"gh","gh_status","chtn_status"))
  s
}

pregnancy_endpoint_fields <- function(source,tier) {
  source <- match.arg(source,c("natality","fetal_death"))
  tier <- match.arg(tier,c("shared_core","shared_augmented"))
  raw <- if(source=="natality")
    c("source_rf_phype","source_f_rf_phyper","source_cig_0","source_cig_1","source_f_cigs_0","source_f_cigs_1") else
    c("source_phyp","source_f_phyp","source_cig0","source_cig1","source_f_cig0","source_f_cig1","source_f_mage","source_f_oe")
  baseline <- c("race_hispanic_origin","prepregnancy_diabetes","prior_liveborn_children_now_living",
    if(tier=="shared_augmented")c("prepregnancy_bmi","education"))
  c("source_row","year","age","oe_weeks","source_restatus","source_dplural",raw,
    baseline,paste0(baseline,"_status"),"race_imputation_status")
}

prepare_pregnancy_endpoint <- function(data,spec,source,min_oe=20L) {
  source <- match.arg(source,c("natality","fetal_death"))
  if (!inherits(spec,"sep_model_specification") || !spec$tier %in% c("shared_core","shared_augmented") ||
      spec$contrast!="within_prepregnancy_smokers" || spec$race_mode!="edited" ||
      !identical(spec,lock_pregnancy_endpoint_spec(spec$tier,spec$years)))
    stop("Locked recorded fetal-death specification required")
  if(length(min_oe)!=1L || !min_oe %in% c(20L,28L))stop("Explicit OE20 or OE28 target required")
  if(!is.data.frame(data)||!nrow(data)||anyDuplicated(names(data)))stop("Nonempty unique-column clinical frame required")
  fields <- pregnancy_endpoint_fields(source,spec$tier)
  if(any(!fields %in% names(data)))stop("Missing source-specific clinical/baseline fields")
  # Keep optional diagnostic fields separate from the selection/model variables.
  d <- as.data.frame(setNames(lapply(fields,function(f)data[[f]]),fields))
  n <- nrow(d)
  if("source_type" %in% names(data) &&
      (anyNA(data$source_type)||any(as.character(data$source_type)!=source)))stop("Declared source/provenance conflict")
  year <- .sep_spec_numeric(d$year,"year"); row <- .sep_spec_numeric(d$source_row,"source_row")
  age <- .sep_spec_numeric(d$age,"age",TRUE); oe <- .sep_spec_numeric(d$oe_weeks,"oe_weeks",TRUE)
  if(any(!year %in% spec$years)||any(row<1|row!=floor(row))||anyDuplicated(data.frame(year,row)))
    stop("Invalid or duplicate source-record identity/year")
  flag <- function(x) {
    x<-as.character(x)
    if(any(!is.na(x)&!x %in% c("","0","1")))stop("Invalid source reporting flag")
    ifelse(x %in% "1","reported",ifelse(x %in% "0","not_reported","unknown"))
  }
  field <- function(natality,fetal)if(source=="natality")natality else fetal
  chtn <- as.character(d[[field("source_rf_phype","source_phyp")]])
  if(any(!is.na(chtn)&!chtn %in% c("","Y","N","U")))stop("Invalid CHTN raw code")
  cflag <- flag(d[[field("source_f_rf_phyper","source_f_phyp")]])
  age_oe_reported <- if(source=="fetal_death")
    flag(d$source_f_mage)=="reported" & flag(d$source_f_oe)=="reported" else rep(TRUE,n)
  clinical <- as.character(d$source_restatus) %in% c("1","2","3") &
    as.character(d$source_dplural) %in% "1" & !is.na(age)&age %in% 15:45 &
    !is.na(oe)&oe %in% 20:47 & age_oe_reported & chtn %in% "N" & cflag=="reported"
  if("clinical_member" %in% names(data))
    .sep_spec_membership(data$clinical_member,clinical,"clinical_member",allow_missing=source=="natality")
  raw_pre <- as.character(d[[field("source_cig_0","source_cig0")]])
  pflag <- flag(d[[field("source_f_cigs_0","source_f_cig0")]])
  early <- classify_early_smoking(raw_pre,as.character(d[[field("source_cig_1","source_cig1")]]),
    pflag,flag(d[[field("source_f_cigs_1","source_f_cig1")]]))
  pre <- decode_cigarettes(raw_pre,pflag,"prepregnancy smoking")
  eligible <- clinical & early$exposure_eligible & !is.na(oe) & oe>=min_oe
  index <- which(eligible); selected <- d[index,,drop=FALSE]
  # Literal source indicator; no GH-derived endpoint or GH-known membership.
  out <- data.frame(Y=rep(as.integer(source=="fetal_death"),length(index)),
    A=early$within_prepregnancy_smokers[index],age=age[index],year_factor=factor(year[index],levels=spec$years))
  reason <- raw_status <- list()
  add <- function(name,field,allowed=NULL,bounds=NULL,transform=identity) {
    z <- .sep_spec_baseline(selected,field,allowed,bounds)
    out[[name]] <<- transform(z$value); reason[[name]] <<- z$reason; raw_status[[name]] <<- z$raw_status
  }
  race_labels <- c("Non-Hispanic White","Non-Hispanic Black","Non-Hispanic AIAN","Non-Hispanic Asian",
    "Non-Hispanic NHOPI","Non-Hispanic multiple race","Hispanic")
  add("race_ethnicity","race_hispanic_origin",1:7,transform=function(x)factor(x,levels=1:7,labels=race_labels))
  yesno <- function(x)factor(x,levels=0:1,labels=c("No","Yes"))
  add("prepreg_diabetes","prepregnancy_diabetes",0:1,transform=yesno)
  add("prior_living4","prior_liveborn_children_now_living",0:30,
    transform=function(x)factor(pmin(x,3),levels=0:3,labels=c("0","1","2","3 or more")))
  if(spec$tier=="shared_augmented") {
    add("bmi","prepregnancy_bmi",bounds=spec$bmi_spec$boundaries)
    add("education4","education",1:8,transform=function(x)factor(c(1,1,2,3,3,4,4,4)[x],levels=1:4,
      labels=c("Less than high school","High school or GED","Some college or associate","Bachelor or higher")))
  }
  dose <- pre$raw_code[index]
  if(any(!dose %in% 1:98))stop("Known positive prepregnancy dose required")
  out$prepreg_dose5 <- cut(dose,c(0,5,10,20,40,Inf),right=TRUE,
    labels=c("1-5","6-10","11-20","21-40","41 or more"))
  reason$prepreg_dose5 <- ifelse(dose==98,"observed_topcoded_98_or_more","observed_edited")
  raw_status$prepreg_dose5 <- pre$status[index]
  race_provenance <- as.character(selected$race_imputation_status)
  if(anyNA(race_provenance)||any(!race_provenance %in% c("not_imputed","unknown_race_imputed",
      "formerly_other_race_imputed","source_missing","unknown_source_na","field_not_supplied","undocumented_source_code")))
    stop("Invalid race imputation provenance")
  # GH diagnostics are optional and computed after membership is fixed. Invalid
  # or unsupported GH is explicitly not interpreted as a recorded negative.
  gh_state <- rep("not_supplied",length(index))
  if(all(c("gh","gh_status") %in% names(data))) {
    g <- suppressWarnings(as.numeric(as.character(data$gh[index])))
    gs <- as.character(data$gh_status[index]); observed <- gs %in% c("observed","observed_yes","observed_no")
    gh_state <- rep("unknown_or_unsupported",length(index))
    gh_state[observed & !is.na(g)&g==0] <- "recorded_no"
    gh_state[observed & !is.na(g)&g==1] <- "recorded_yes"
  }
  identity <- data.frame(input_row=index,source_type=rep(source,length(index)),year=as.integer(year[index]),source_row=row[index],
    oe_weeks=oe[index],prepregnancy_dose_raw=dose,prepregnancy_dose_topcoded=dose==98,
    postselection_gh_state=gh_state,stringsAsFactors=FALSE)
  if(anyNA(out[c("Y","A","age","year_factor")])||!setequal(names(out),c("Y","A","age",spec$covariates)))
    stop("Prepared endpoint model contract mismatch")
  missingness <- as.data.frame(reason); original_status <- as.data.frame(raw_status)
  counts <- do.call(rbind,lapply(names(reason),function(f){
    if(!length(reason[[f]]))return(data.frame(reason=character(),n=integer(),field=character()))
    x<-as.data.frame(table(reason[[f]]),stringsAsFactors=FALSE);names(x)<-c("reason","n");x$field<-rep(f,nrow(x));x
  }))
  structure(list(data=out,identity=identity,spec=spec,missingness=missingness,original_status=original_status,
    missingness_counts=counts,race_provenance=race_provenance,n_input=n,n_clinical=sum(clinical),
    n_exposure_eligible=sum(clinical&early$exposure_eligible),n_selected=length(index),
    n_excluded_oe=sum(clinical&early$exposure_eligible&oe<min_oe),minimum_oe=as.integer(min_oe),
    baseline_records_dropped=0L,requires_missingness_resolution=anyNA(out),GH_required=FALSE,GH_adjusted=FALSE,
    primary_GH_member_used=FALSE,source_is_outcome_not_adjustment=TRUE,
    reference_target="Own combined-source empirical eligible population; not live-only paired-GH reference",
    endpoint_caveat="Recorded fetal death among selected pregnancy-ending certificates; no conception cohort or early losses"),
    class="sep_prepared_pregnancy_endpoint")
}

bind_pregnancy_endpoint_parts <- function(parts,spec) {
  if(!length(parts)||any(!vapply(parts,inherits,TRUE,"sep_prepared_pregnancy_endpoint")))stop("Endpoint parts required")
  if(any(!vapply(parts,function(p)identical(p$spec,spec),TRUE)))stop("Endpoint specification drift")
  fields<-c("data","identity","missingness","original_status")
  out<-setNames(lapply(fields,function(f)as.data.frame(data.table::rbindlist(lapply(parts,`[[`,f),use.names=TRUE))),fields)
  out$identity$annual_input_row<-out$identity$input_row;out$identity$input_row<-seq_len(nrow(out$data))
  if(anyDuplicated(out$identity[c("source_type","year","source_row")])||
      any(out$data$Y!=as.integer(out$identity$source_type=="fetal_death")))stop("Duplicate record or endpoint identity drift")
  out$race_provenance<-unlist(lapply(parts,`[[`,"race_provenance"),use.names=FALSE)
  out$spec<-spec;out$n_input<-sum(vapply(parts,`[[`,0,"n_input"));out$n_selected<-nrow(out$data)
  out$baseline_records_dropped<-0L;out$requires_missingness_resolution<-anyNA(out$data)
  out$GH_required<-out$GH_adjusted<-out$primary_GH_member_used<-FALSE
  out$source_is_outcome_not_adjustment<-TRUE
  out$reference_target<-"Own combined-source empirical eligible population; not live-only paired-GH reference"
  class(out)<-"sep_prepared_pregnancy_endpoint";out
}
