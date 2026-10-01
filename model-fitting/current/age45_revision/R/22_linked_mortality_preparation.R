# Pure linked-infant model preparation. Never substitutes a mortality indicator
# for GH, conditions on known GH, or reuses a GH-restricted input implicitly.
# Requires source R01 and R10 for audited decoders, baseline mappings and specs.
prepare_linked_mortality_model <- function(data,spec,endpoint="infant") {
  endpoints<-c(infant="linked_infant_death_observed",neonatal="linked_neonatal_death_observed",
    early_neonatal="linked_early_neonatal_death_observed")
  if(!endpoint %in% names(endpoints)||!inherits(spec,"sep_model_specification")||
     spec$tier!="natality_main"||spec$contrast!="within_prepregnancy_smokers"||spec$race_mode!="edited"||
     !identical(spec$years,2018:2023))stop("Explicit 2018-2023 main within-smoker linked endpoint specification required")
  if(!is.data.frame(data)||!nrow(data)||anyDuplicated(names(data)))stop("Nonempty unique-column input required")
  d<-as.data.frame(data);n<-nrow(d)
  required<-c("source_type","source_row","year","age","oe_weeks","source_restatus","source_dplural",
    "source_rf_phype","source_f_rf_phype","source_cig_0","source_cig_1","source_f_cigs_0","source_f_cigs_1",
    "clinical_member","primary_mortality_member","mortality_linkage_status","linked_infant_death_observed",endpoints[[endpoint]])
  if(any(!required %in% names(d))||anyNA(d$source_type)||any(d$source_type!="linked_birth_cohort"))stop("Verified linked-birth cohort fields/provenance required")
  year<-.sep_spec_numeric(d$year,"year");age<-.sep_spec_numeric(d$age,"age",TRUE)
  oe<-.sep_spec_numeric(d$oe_weeks,"oe_weeks",TRUE);row<-.sep_spec_numeric(d$source_row,"source_row")
  if(any(!year %in% spec$years)||any(row<1|row!=floor(row))||anyDuplicated(data.frame(year,row)))stop("Invalid cohort identity/year")
  flag<-function(x) {
    x<-as.character(x);if(anyNA(x)||any(!x %in% c("","0","1")))stop("Invalid reporting flag")
    ifelse(x=="1","reported",ifelse(x=="0","not_reported","unknown"))
  }
  chtn<-as.character(d$source_rf_phype)
  if(anyNA(chtn)||any(!chtn %in% c("","Y","N","U")))stop("Invalid CHTN code")
  clinical<-as.character(d$source_restatus) %in% c("1","2","3") & as.character(d$source_dplural) %in% "1" &
    !is.na(age)&age %in% 15:45 & !is.na(oe)&oe %in% 20:47 & chtn=="N" & flag(d$source_f_rf_phype)=="reported"
  .sep_spec_membership(d$clinical_member,clinical,"clinical_member")
  early<-classify_early_smoking(as.character(d$source_cig_0),as.character(d$source_cig_1),
    flag(d$source_f_cigs_0),flag(d$source_f_cigs_1))
  eligible<-clinical&early$exposure_eligible
  .sep_spec_membership(d$primary_mortality_member,eligible,"primary_mortality_member")
  status<-as.character(d$mortality_linkage_status)
  if(anyNA(status)||any(!status %in% c("no_linked_death_observed","linked_infant_death_observed")))
    stop("Complete numerator search with resolved linkage required; partial searches are not zeros")
  infant<-.sep_spec_numeric(d$linked_infant_death_observed,"linked infant",TRUE)
  if(anyNA(infant)||any(!infant %in% 0:1)||any(infant!=as.integer(status=="linked_infant_death_observed")))
    stop("Observed infant linkage/indicator conflict")
  Y<-.sep_spec_numeric(d[[endpoints[[endpoint]]]],endpoint,TRUE)
  if(any(!is.na(Y)&!Y %in% 0:1)||any(infant==0&(is.na(Y)|Y!=0))||any(!is.na(Y)&Y>infant))stop("Inconsistent mortality endpoint")
  index<-which(eligible&!is.na(Y));if(!length(index))stop("No endpoint-known eligible records")
  selected<-d[index,,drop=FALSE]
  out<-data.frame(Y=Y[index],A=early$within_prepregnancy_smokers[index],age=age[index],year_factor=factor(year[index],levels=spec$years))
  reason<-raw<-list()
  add<-function(name,field,allowed=NULL,bounds=NULL,transform=identity) {
    z<-.sep_spec_baseline(selected,field,allowed,bounds)
    out[[name]]<<-transform(z$value);reason[[name]]<<-z$reason;raw[[name]]<<-z$raw_status
  }
  race_labels<-c("Non-Hispanic White","Non-Hispanic Black","Non-Hispanic AIAN","Non-Hispanic Asian",
    "Non-Hispanic NHOPI","Non-Hispanic multiple race","Hispanic")
  add("race_ethnicity","race_hispanic_origin",1:7,transform=function(x)factor(x,levels=1:7,labels=race_labels))
  yesno<-function(x)factor(x,levels=0:1,labels=c("No","Yes"))
  add("prepreg_diabetes","prepregnancy_diabetes",0:1,transform=yesno)
  add("prior_living4","prior_liveborn_children_now_living",0:30,transform=function(x)
    factor(pmin(x,3),levels=0:3,labels=c("0","1","2","3 or more")))
  add("bmi","prepregnancy_bmi",bounds=spec$bmi_spec$boundaries)
  add("education4","education",1:8,transform=function(x)factor(c(1,1,2,3,3,4,4,4)[x],levels=1:4,
    labels=c("Less than high school","High school or GED","Some college or associate","Bachelor or higher")))
  add("prior_preterm","previous_preterm_birth",0:1,transform=yesno)
  add("prior_cesarean","previous_cesarean",0:1,transform=yesno)
  add("nativity","nativity",1:2,transform=function(x)factor(x,levels=1:2,labels=c("Born in 50 US states","Born elsewhere including territories")))
  decoded_pre<-decode_cigarettes(as.character(d$source_cig_0),flag(d$source_f_cigs_0),"prepregnancy smoking")
  dose<-decoded_pre$raw_code[index]
  if(any(!dose %in% 1:98))stop("Known positive prepregnancy dose required")
  out$prepreg_dose5<-cut(dose,c(0,5,10,20,40,Inf),right=TRUE,labels=c("1-5","6-10","11-20","21-40","41 or more"))
  reason$prepreg_dose5<-ifelse(dose==98,"observed_topcoded_98_or_more","observed_edited")
  raw$prepreg_dose5<-decoded_pre$status[index]
  if(anyNA(out[c("Y","A","age","year_factor")])||!setequal(names(out),c("Y","A","age",spec$covariates)))stop("Prepared endpoint contract mismatch")
  spec$estimand_label<-paste("Standardized association with observed linked",endpoint,"death among eligible live births; not complete true mortality")
  spec$outcome_definition<-endpoints[[endpoint]]
  structure(list(data=out,identity=data.frame(input_row=index,source_type="linked_birth_cohort",year=as.integer(year[index]),source_row=row[index]),
    spec=spec,endpoint=endpoint,missingness=as.data.frame(reason),original_status=as.data.frame(raw),
    n_input=n,n_exposure_eligible=sum(eligible),n_endpoint_unknown=sum(eligible&is.na(Y)),n_selected=length(index),
    baseline_records_dropped=0L,requires_missingness_resolution=anyNA(out),GH_required=FALSE,GH_adjusted=FALSE,
    primary_GH_member_used=FALSE,period_RECWT_used=FALSE,unmatched_births_proven_survivors=FALSE,
    endpoint_caveat="Observed linked endpoint; unmatched births are not proven survivors; 2018 death-age method differs for neonatal subtypes"),
    class="sep_prepared_mortality_data")
}
