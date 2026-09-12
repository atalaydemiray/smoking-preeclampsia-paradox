# Shared helpers for the fitting scripts: engine loading, code hashing, natality
# input loading, and the standardization and receipt writing every fit shares.
# Every path here is relative to model-fitting/, the working directory the
# scripts expect.
age45_initialize <- function() {
  stopifnot(file.exists("R/08_streaming_standardization.R"))
  data.table::setDTthreads(1L)
  for(f in c("09_standardization","11_mi_pooling","08_streaming_standardization",
    "12_paired_source_comparison","10_overall_standardization","13_overall_paired",
    "14_age_crossover")) source(paste0("R/",f,".R"),local=.GlobalEnv)
  source("R/01_measurement_helpers.R",local=.GlobalEnv)
  source("R/05_model_specification.R",local=.GlobalEnv)
}
age45_sha <- function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
age45_code_manifest <- function() {
  # The engines and the revision modules used to sit in two directories and are
  # now one, so the same file can be named twice. Hash each file once.
  files <- unique(c(list.files("R",pattern="[.]R$",full.names=TRUE),
    list.files("scripts",pattern="0[36]_.*[.]R$",full.names=TRUE),
    paste0("R/",c("09_standardization","11_mi_pooling","08_streaming_standardization",
      "12_paired_source_comparison","10_overall_standardization","13_overall_paired",
      "14_age_crossover"),".R")))
  data.frame(path=files,sha256=vapply(files,age45_sha,""))
}
age45_json <- function(x,p)jsonlite::write_json(x,p,pretty=TRUE,auto_unbox=TRUE,
  digits=16,null="null",na="null")
age45_load_natality <- function(contrast,years) {
  parts <- ids <- list()
  for(y in years) {
    stem<-file.path("derived/model_inputs",paste(y,contrast,sep="_"))
    r<-jsonlite::fromJSON(paste0(stem,"_receipt.json"))
    stopifnot(r$status=="complete_prepared_age15_45",
      age45_sha(paste0(stem,".rds"))==r$output_sha256)
    p<-readRDS(paste0(stem,".rds"))
    stopifnot(identical(as.numeric(p$spec$age_spec$boundaries),c(15,45)),
      nrow(p$data)==r$counts$eligible_n,
      sum(complete.cases(p$data[p$spec$covariates]))==r$counts$cc_n)
    parts[[as.character(y)]]<-p$data
    ids[[as.character(y)]]<-p$identity
    spec<-p$spec
  }
  p<-list(data=as.data.frame(data.table::rbindlist(parts,use.names=TRUE)),
    identity=as.data.frame(data.table::rbindlist(ids,use.names=TRUE)),spec=spec)
  p$spec$years<-as.integer(years)
  p$data$year_factor<-factor(as.character(p$data$year_factor),levels=years)
  stopifnot(!anyDuplicated(p$identity[c("source_type","year","source_row")]),
    nrow(p$data)==nrow(p$identity),all(p$data$age%in%15:45))
  p
}
age45_crossover <- function(fit,cv) {
  n<-names(fit$beta); sp<-fit$age_spec
  stopifnot(fit$converged,identical(as.numeric(sp$boundaries),c(15,45)))
  at<-which(n=="A"|startsWith(n,"A:splines::ns(age,"))
  ai<-which(n=="A"|startsWith(n,"A:")|grepl(":A$",n))
  stopifnot(length(at)==length(sp$knots)+2L,setequal(at,ai))
  v<-fit[[paste0("vcov_",cv)]]
  analyze_age_crossover(fit$beta[at],v[at,at,drop=FALSE],sp$knots,sp$boundaries)
}
age45_save_standardization <- function(fit,reference,out,root_allowed=TRUE) {
  for(cv in c("HC0","model")) {
    f<-fit;f$covariance_type<-cv;f$vcov<-f[[paste0("vcov_",cv)]]
    st<-standardize_streaming_logit(f,reference,15:45,"age_specific_empirical")
    ov<-overall_streaming_risks(st,as.numeric(nrow(reference)))
    tab<-st$summary
    stopifnot(identical(as.numeric(tab$age),as.numeric(15:45)),
      all(is.finite(tab$rr)),all(tab$risk0>0&tab$risk0<1),
      all(tab$risk1>0&tab$risk1<1))
    for(nm in c("risk0","risk1","rd","risk0_lower","risk0_upper",
      "risk1_lower","risk1_upper","rd_lower","rd_upper"))
      tab[[paste0(nm,"_per1000")]]<-1000*tab[[nm]]
    crit<-qnorm(1-.05/(2*31))
    tab$rd_bonferroni_lower_per1000<-1000*(tab$rd-crit*tab$se_rd)
    tab$rd_bonferroni_upper_per1000<-1000*(tab$rd+crit*tab$se_rd)
    for(nm in c("estimate","lower","upper"))
      ov$summary[[paste0(nm,"_per1000")]]<-ifelse(ov$summary$measure=="rr",NA,1000*ov$summary[[nm]])
    data.table::fwrite(tab,file.path(out,paste0(cv,"_age_standardized.csv")))
    data.table::fwrite(ov$summary,file.path(out,paste0(cv,"_overall_standardized.csv")))
    obj<-list(age=st,overall=ov);path<-file.path(out,paste0(cv,"_joint_standardization.rds"))
    saveRDS(obj,path)
    ch<-readRDS(path)
    stopifnot(identical(ch$age$estimate,st$estimate),
      identical(ch$age$covariance,st$covariance),identical(ch$overall,ov))
    if(root_allowed) {
      co<-age45_crossover(f,cv)
      ev<-evaluate_age_crossover(co,15:45)
      stopifnot(all(sign(ev$h)==sign(tab$rd)))
      saveRDS(co,file.path(out,paste0(cv,"_crossover.rds")))
      data.table::fwrite(co$roots,file.path(out,paste0(cv,"_roots.csv")))
      for(method in c("pointwise","simultaneous"))
        data.table::fwrite(co[[method]]$confidence_set,
          file.path(out,paste0(cv,"_",method,"_null_age_set.csv")))
    }
  }
}
age45_fit <- function(id,d,spec,identity,reference=d,root_allowed=TRUE,eligible_n=nrow(d)) {
  out<-file.path("outputs/models",id)
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  rp<-file.path(out,"receipt.json")
  signature<-digest::digest(list(data=d,identity=identity,reference=reference,
    formula=deparse(spec$formula),age=spec$age_spec,code=age45_code_manifest()),algo="sha256")
  if(file.exists(rp)) {
    old<-jsonlite::fromJSON(rp)
    stopifnot(old$status=="complete_validated_age15_45_fit",old$input_signature==signature)
    for(i in seq_len(nrow(old$outputs)))
      stopifnot(age45_sha(old$outputs$path[i])==old$outputs$sha256[i])
    message(id,": verified completed fit reused")
    return(invisible(readRDS(file.path(out,"model.rds"))))
  }
  stopifnot(!anyNA(d),!anyNA(reference),all(d$age%in%15:45),
    nrow(d)==nrow(identity),any(d$age==45),
    !anyDuplicated(identity[c("source_type","year","source_row")]))
  before<-Sys.time()
  age45_json(list(status="running",model=id,fit_n=nrow(d),reference_n=nrow(reference),
    input_signature=signature,started=as.character(before)),file.path(out,"running.json"))
  message(id,": fitting N=",nrow(d),"; age45 N=",sum(d$age==45))
  fit<-fit_streaming_logit(d,spec$formula,"Y","A","age",spec$age_spec,"HC0",25000L)
  stopifnot(fit$converged,max(abs(fit$final_score))/nrow(d)<1e-12)
  saveRDS(fit,file.path(out,"model.rds"))
  stopifnot(identical(readRDS(file.path(out,"model.rds"))$beta,fit$beta))
  support<-data.table::data.table(age=d$age,A=d$A,Y=d$Y)[,
    .(n=.N,events=sum(Y),non_events=sum(1-Y)),by=.(age,A)]
  support<-merge(data.table::CJ(age=15:45,A=0:1),support,by=c("age","A"),all.x=TRUE)
  support[is.na(n),c("n","events","non_events"):=list(0L,0L,0L)]
  support[,sparse:=events<20|non_events<20]
  data.table::fwrite(support,file.path(out,"age_arm_support.csv"))
  age45_save_standardization(fit,reference,out,root_allowed)
  files<-list.files(out,full.names=TRUE)
  files<-files[basename(files)!="receipt.json"]
  age45_json(list(status="complete_validated_age15_45_fit",model=id,
    eligible_n=eligible_n,fit_n=nrow(d),events=sum(d$Y),A0=sum(d$A==0),A1=sum(d$A==1),
    age45_n=sum(d$age==45),age45_events=sum(d$Y[d$age==45]),
    reference_n=nrow(reference),input_signature=signature,age_domain=c(15,45),
    formula=paste(deparse(spec$formula,width.cutoff=500),collapse=" "),
    age_knots=spec$age_spec$knots,age_boundaries=spec$age_spec$boundaries,
    no_imputation=TRUE,iterations=fit$iterations,score_max=max(abs(fit$final_score)),
    code=age45_code_manifest(),
    elapsed_seconds=as.numeric(difftime(Sys.time(),before,units="secs")),
    outputs=data.frame(path=files,sha256=vapply(files,age45_sha,""))),rp)
  message(id,": completed in ",round(as.numeric(difftime(Sys.time(),before,units="secs")))," seconds")
  invisible(fit)
}
