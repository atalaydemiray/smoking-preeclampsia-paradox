source("R/01_measurement_helpers.R");source("R/10_model_specification.R");source("R/22_linked_mortality_preparation.R")
env<-new.env(parent=globalenv());invisible(capture.output(sys.source("tests/test_model_specification.R",env)))
s<-lock_sep_model_specification("natality_main","within_prepregnancy_smokers",2018:2023,c(21,27,35),c(19.6,26.1,37.8))
d<-env$fixture(28);d$year<-rep(2018:2023,length.out=28);d$source_type<-"linked_birth_cohort"
# Linked native spelling differs from the converted natality spelling.
d$source_f_rf_phype<-d$source_f_rf_phyper;d$source_f_rf_phyper<-NULL
d$clinical_member<-TRUE;d$primary_mortality_member<-TRUE
d$mortality_linkage_status<-rep(c("no_linked_death_observed","linked_infant_death_observed"),14)
d$linked_infant_death_observed<-rep(0:1,14)
d$linked_neonatal_death_observed<-d$linked_early_neonatal_death_observed<-rep(c(0,1,0,0),7)
p<-prepare_linked_mortality_model(d,s)
checks<-0L;check<-function(x){stopifnot(isTRUE(x));checks<<-checks+1L}
fails<-function(e)check(inherits(tryCatch(force(e),error=identity),"error"))
check(p$n_selected==28L);check(identical(p$data$Y,as.numeric(d$linked_infant_death_observed)))
check(!p$GH_required&&!p$GH_adjusted&&!p$primary_GH_member_used&&!p$period_RECWT_used)
u<-d;u$gh<-NA;u$gh_status<-"unknown_codeU";u$primary_member<-FALSE
q<-prepare_linked_mortality_model(u,s);check(identical(p$data,q$data));check(identical(p$identity,q$identity))
u$source_cig_3<-"99";check(identical(prepare_linked_mortality_model(u,s)$data,p$data))
u<-d;u$prepregnancy_bmi[1]<-NA;u$prepregnancy_bmi_status[1]<-"unknown_code"
q<-prepare_linked_mortality_model(u,s);check(q$n_selected==28L&&sum(!complete.cases(q$data))==1L)
check(q$missingness$bmi[1]=="item_unknown")
u<-d;u$mortality_linkage_status[1]<-"numerator_search_incomplete";fails(prepare_linked_mortality_model(u,s))
u<-d;u$linked_infant_death_observed[1]<-1;fails(prepare_linked_mortality_model(u,s))
u<-d;u$linked_neonatal_death_observed[2]<-NA;q<-prepare_linked_mortality_model(u,s,"neonatal")
check(q$n_endpoint_unknown==1L&&q$n_selected==27L);check(!2L %in% q$identity$input_row)
check(prepare_linked_mortality_model(u,s,"infant")$n_selected==28L)
u<-d;u$linked_neonatal_death_observed[1]<-NA;fails(prepare_linked_mortality_model(u,s,"neonatal"))
u<-d;u$source_row[2]<-u$source_row[1];u$year[2]<-u$year[1];fails(prepare_linked_mortality_model(u,s))
u<-d;u$primary_mortality_member[1]<-FALSE;fails(prepare_linked_mortality_model(u,s))
u<-d;u$source_cig_0[1]<-"98";q<-prepare_linked_mortality_model(u,s);check(as.character(q$data$prepreg_dose5[1])=="41 or more")
check(q$missingness$prepreg_dose5[1]=="observed_topcoded_98_or_more")
decoded<-decode_cigarettes(as.character(u$source_cig_0),rep("reported",nrow(u)),"prepregnancy smoking")
check(identical(q$original_status$prepreg_dose5,decoded$status))
u<-d;u$source_type<-"natality";fails(prepare_linked_mortality_model(u,s))
# Independent mapping agreement with frozen maternal preparation where GH is known;
# outcome is compared separately and is never used to construct infant eligibility.
u<-d;u$source_type<-"natality";u$primary_member<-TRUE;u$gh<-0;u$gh_status<-"observed_no"
u$source_f_rf_phyper<-u$source_f_rf_phype;u$source_f_rf_phype<-NULL
g<-prepare_sep_model_data(u,s,"natality")
check(identical(p$data[setdiff(names(p$data),"Y")],g$data[setdiff(names(p$data),"Y")]))
check(!identical(p$data$Y,g$data$Y))
check(identical(s$age_spec,p$spec$age_spec)&&identical(s$bmi_spec,p$spec$bmi_spec))
u<-d;u$source_f_rf_phyper<-u$source_f_rf_phype;u$source_f_rf_phype<-NULL
fails(prepare_linked_mortality_model(u,s))
cat("PASS:",checks,"linked mortality preparation checks; synthetic only\n")
