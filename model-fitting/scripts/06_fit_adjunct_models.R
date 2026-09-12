main <- function(group="all") {
  source("R/17_fit_helpers.R");age45_initialize()
  source("R/07_pregnancy_endpoint_preparation.R")
  folder<-"derived/adjunct_models"
  load_part<-function(stem) {
    r<-jsonlite::fromJSON(file.path(folder,paste0(stem,"_receipt.json")))
    stopifnot(r$status=="complete_prepared_age15_45",age45_sha(r$output_path)==r$output_sha256)
    readRDS(r$output_path)
  }
  combine<-function(prefix,tier) {
    parts<-list()
    for(src in c("natality","fetal_death"))for(y in 2018:2024)
      parts[[paste(src,y)]]<-load_part(paste(prefix,y,src,tier,sep="_"))
    d<-as.data.frame(data.table::rbindlist(lapply(parts,`[[`,"data"),use.names=TRUE))
    identity<-as.data.frame(data.table::rbindlist(lapply(parts,`[[`,"identity"),use.names=TRUE))
    stopifnot(!anyDuplicated(identity[c("source_type","year","source_row")]),
      nrow(d)==nrow(identity),all(d$age%in%15:45))
    list(data=d,identity=identity,spec=parts[[1]]$spec)
  }
  if(group%in%c("all","paired"))for(tier in c("shared_core","shared_augmented")) {
    p<-combine("gh",tier)
    for(period in c("all_years","reporting_years")) {
      id<-paste(period,tier,sep="_");out<-file.path("outputs/models",id)
      rp<-file.path(out,"receipt.json")
      if(file.exists(rp)) {
        r<-jsonlite::fromJSON(rp)
        stopifnot(r$status=="complete_paired_age15_45")
        for(i in seq_len(nrow(r$outputs)))stopifnot(age45_sha(r$outputs$path[i])==r$outputs$sha256[i])
        next
      }
      dir.create(out,recursive=TRUE,showWarnings=FALSE)
      years<-if(period=="all_years")2018:2024 else c(2019L,2020L,2021L,2024L)
      pop<-p$identity$year%in%years
      at<-which(pop&complete.cases(p$data[p$spec$covariates]))
      d<-p$data[at,,drop=FALSE];identity<-p$identity[at,,drop=FALSE]
      d$year_factor<-factor(as.character(d$year_factor),levels=years)
      spec<-p$spec;spec$years<-years
      live<-which(identity$source_type=="natality")
      ids<-paste(identity$source_type,identity$year,identity$source_row,sep=":")
      message(id,": fitting paired N=",nrow(d))
      fits<-fit_paired_source_models(d,live,ids,spec$formula,"Y","A","age",spec$age_spec,chunk_size=25000L)
      paired<-paired_source_comparison(d,live,ids,fits$live,fits$inclusive,15:45,
        "age_specific_empirical",target_reference_id=paste0(id,"_age15_45_live_reference"))
      overall<-overall_paired_source_risks(paired)
      saveRDS(fits,file.path(out,"paired_models.rds"))
      saveRDS(paired,file.path(out,"paired_standardization.rds"))
      saveRDS(overall,file.path(out,"overall_paired.rds"))
      stopifnot(identical(readRDS(file.path(out,"paired_models.rds"))$live$beta,fits$live$beta),
        identical(readRDS(file.path(out,"paired_models.rds"))$inclusive$vcov_HC0,fits$inclusive$vcov_HC0))
      data.table::fwrite(paired$paired_summary,file.path(out,"paired_age_differences.csv"))
      data.table::fwrite(overall$summary,file.path(out,"paired_overall_differences.csv"))
      for(member in c("live","inclusive")) {
        subdir<-file.path(out,member);dir.create(subdir,showWarnings=FALSE)
        saveRDS(fits[[member]],file.path(subdir,"model.rds"))
        age45_save_standardization(fits[[member]],d[live,,drop=FALSE],subdir,TRUE)
        data.table::fwrite(paired[[member]]$summary,file.path(out,paste0(member,"_age_standardized.csv")))
        data.table::fwrite(overall[[member]]$summary,file.path(out,paste0(member,"_overall_standardized.csv")))
      }
      sp<-data.table::data.table(source=identity$source_type,year=identity$year,
        age=d$age,A=d$A,Y=d$Y)[,.(n=.N,events=sum(Y)),by=.(source,year,age,A)]
      data.table::fwrite(sp,file.path(out,"source_year_age_arm_support.csv"))
      files<-list.files(out,full.names=TRUE,recursive=TRUE)
      age45_json(list(status="complete_paired_age15_45",model=id,
        eligible_n=sum(pop),fit_n=nrow(d),live_n=length(live),fetal_n=nrow(d)-length(live),
        input_signature=digest::digest(list(d,identity),algo="sha256"),
        code=age45_code_manifest(),
        no_imputation=TRUE,paired_HC0=TRUE,outputs=data.frame(path=files,sha256=vapply(files,age45_sha,""))),rp)
      message(id,": completed")
    }
    rm(p);gc(FALSE)
  }
  if(group%in%c("all","fetal"))for(tier in c("shared_core","shared_augmented")) {
    p<-combine("fetal_endpoint",tier)
    for(period in c("all","flag_supported"))for(oe in c(20L,28L)) {
      years<-if(period=="all")2018:2024 else c(2019L,2020L,2021L,2024L)
      pop<-p$identity$year%in%years&p$identity$oe_weeks>=oe
      at<-which(pop&complete.cases(p$data[p$spec$covariates]))
      d<-p$data[at,,drop=FALSE];identity<-p$identity[at,,drop=FALSE]
      d$year_factor<-factor(as.character(d$year_factor),levels=years)
      spec<-lock_pregnancy_endpoint_spec(tier,years)
      stopifnot(all(d$Y==as.integer(identity$source_type=="fetal_death")))
      age45_fit(paste("fetal",tier,period,paste0("oe",oe),sep="_"),
        d,spec,identity,root_allowed=FALSE,eligible_n=sum(pop))
    }
    rm(p);gc(FALSE)
  }
  if(group%in%c("all","linked"))for(ep in c("infant","neonatal","early_neonatal")) {
    p<-load_part(paste0("linked_",ep))
    stopifnot(!p$GH_required,!p$GH_adjusted,!p$period_RECWT_used)
    for(period in c("main","later")) {
      years<-if(period=="main")2018:2023 else 2019:2023
      pop<-p$identity$year%in%years
      at<-which(pop&complete.cases(p$data[p$spec$covariates]))
      d<-p$data[at,,drop=FALSE];identity<-p$identity[at,,drop=FALSE]
      d$year_factor<-factor(as.character(d$year_factor),levels=years)
      spec<-p$spec;spec$years<-years
      age45_fit(paste("linked",ep,period,sep="_"),d,spec,identity,
        root_allowed=FALSE,eligible_n=sum(pop))
    }
    rm(p);gc(FALSE)
  }
}
if(sys.nframe()==0L) {
  a<-commandArgs(TRUE);main(if(length(a))a[1]else"all")
}
