# Refit the three main contrasts and 15 retained natality sensitivity models.
# Run from model-fitting/: source(this file); main("primary").
main <- function(group="all") {
  source("R/17_fit_helpers.R")
  age45_initialize()
  groups<-if(group=="all")c("primary","broad","prepregnancy","historical") else group
  stopifnot(all(groups%in%c("primary","broad","prepregnancy","historical")))
  for(g in groups) {
    years<-if(g=="historical")2014:2015 else 2016:2024
    ct<-if(g=="historical")"primary" else g
    p<-age45_load_natality(ct,years);spec<-p$spec
    cc<-complete.cases(p$data[spec$covariates])
    run<-function(id,sp=spec,rows=which(cc),refrows=rows,root=TRUE,eligible=nrow(p$data)) {
      vars<-c("Y","A","age",sp$covariates)
      d<-p$data[rows,vars,drop=FALSE];ref<-p$data[refrows,vars,drop=FALSE]
      d$year_factor<-factor(as.character(d$year_factor),levels=sp$years)
      ref$year_factor<-factor(as.character(ref$year_factor),levels=sp$years)
      age45_fit(id,d,sp,p$identity[rows,,drop=FALSE],ref,root,eligible_n=eligible)
    }
    if(g=="historical") {run("primary_historical"); next}
    run(paste0(g,"_main"))
    if(g=="primary") {
      for(choice in c("core_common_cc","augmented_common_cc","core_available_cc","main_2018_2024_cc")) {
        tier<-if(startsWith(choice,"core"))"shared_core" else
          if(startsWith(choice,"augmented"))"shared_augmented" else "natality_main"
        yy<-if(choice=="main_2018_2024_cc")2018:2024 else 2016:2024
        sp<-lock_sep_model_specification(tier,"within_prepregnancy_smokers",yy,
          c(21,27,35),if(tier=="shared_core")NULL else c(19.6,26.1,37.8))
        rows<-which((if(choice=="core_available_cc")complete.cases(p$data[sp$covariates]) else cc)&p$identity$year%in%yy)
        refs<-which(cc&p$identity$year%in%yy)
        run(choice,sp,rows,refs,eligible=sum(p$identity$year%in%yy))
      }
      run("primary_oe28",rows=which(cc&p$identity$oe_weeks>=28),
        eligible=sum(p$identity$oe_weeks>=28))
      bases<-list(fewer_age_knots=c(22,32),shifted_age_knots=c(23,29,36),
        more_age_knots=c(20,25,30,36),age_by_bmi_nuisance=c(21,27,35))
      for(sc in names(bases)) {
        sp<-lock_sep_model_specification("natality_main","within_prepregnancy_smokers",
          2016:2024,bases[[sc]],c(19.6,26.1,37.8))
        if(sc=="age_by_bmi_nuisance") {
          extra<-paste(.sep_spec_ns_term("age",sp$age_spec),.sep_spec_ns_term("bmi",sp$bmi_spec),sep=":")
          sp$formula<-as.formula(paste(paste(deparse(sp$formula,width.cutoff=500),collapse=" "),"+",extra),env=baseenv())
        }
        run(sc,sp)
      }
    }
    age<-.sep_spec_ns_term("age",spec$age_spec); bmi<-.sep_spec_ns_term("bmi",spec$bmi_spec)
    extras<-c(smoking_by_bmi=paste0("A:",bmi),
      smoking_and_age_by_year=paste0("A:year_factor + ",age,":year_factor"),
      combined=paste0("A:",bmi," + A:year_factor + ",age,":year_factor"))
    for(sc in if(g=="primary")names(extras) else "combined") {
      sp<-spec
      sp$formula<-as.formula(paste(paste(deparse(sp$formula,width.cutoff=500),collapse=" "),"+",extras[[sc]]),env=baseenv())
      run(paste(g,sc,sep="_"),sp,root=FALSE)
    }
    rm(p);gc(FALSE)
  }
}
if(sys.nframe()==0L) {
  a<-commandArgs(TRUE);main(if(length(a))a[1] else "all")
}
