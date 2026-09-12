# Same numerical engines and criteria, bounded-memory fingerprints for >29M rows.
# The earlier generic driver materialized one serialized data/reference/identity
# object before fitting. Columnwise hashes preserve exact values without that
# large transient allocation. No numerical or cohort criterion is changed.
main <- function(contrast="broad",equivalence_test=FALSE) {
  source("R/17_fit_helpers.R");age45_initialize()
  stopifnot(contrast%in%c("primary","broad","prepregnancy"),
    contrast!="primary"||equivalence_test)
  hash_frame<-function(d)digest::digest(list(names=names(d),n=nrow(d),
    columns=vapply(d,digest::digest,"",algo="sha256")),algo="sha256")
  code<-rbind(age45_code_manifest(),data.frame(path="scripts/12_large_cohort_refits.R",
    sha256=age45_sha("scripts/12_large_cohort_refits.R")))
  parts<-ids<-sources<-list();eligible<-0L
  for(y in 2016:2024) {
    stem<-file.path("derived/model_inputs",paste(y,contrast,sep="_"))
    rp<-paste0(stem,"_receipt.json");r<-jsonlite::fromJSON(rp)
    stopifnot(r$status=="complete_prepared_age15_45",age45_sha(paste0(stem,".rds"))==r$output_sha256)
    p<-readRDS(paste0(stem,".rds"));spec<-p$spec
    cc<-which(complete.cases(p$data[spec$covariates]));vars<-c("Y","A","age",spec$covariates)
    stopifnot(length(cc)==r$counts$cc_n,nrow(p$data)==r$counts$eligible_n,
      all(p$identity$year==y),!is.unsorted(p$identity$source_row,strictly=TRUE))
    parts[[as.character(y)]]<-p$data[cc,vars,drop=FALSE]
    ids[[as.character(y)]]<-p$identity[cc,,drop=FALSE]
    eligible<-eligible+nrow(p$data)
    sources[[as.character(y)]]<-list(year=y,receipt_sha256=age45_sha(rp),data_sha256=r$output_sha256)
    message(contrast,": loaded validated complete cases ",y," N=",length(cc))
    rm(p);gc(FALSE)
  }
  d<-as.data.frame(data.table::rbindlist(parts,use.names=TRUE));identity<-as.data.frame(data.table::rbindlist(ids,use.names=TRUE))
  rm(parts,ids);gc(FALSE)
  d$year_factor<-factor(as.character(d$year_factor),levels=2016:2024)
  stopifnot(!anyNA(d),any(d$age==45),all(d$age%in%15:45),nrow(d)==nrow(identity),
    !anyDuplicated(data.table::as.data.table(identity[c("source_type","year","source_row")])) )
  message(contrast,": columnwise exact-data fingerprint N=",nrow(d))
  dh<-hash_frame(d);ih<-hash_frame(identity)
  support<-data.table::data.table(age=d$age,A=d$A,Y=d$Y)[,
    .(n=.N,events=sum(Y),non_events=sum(1-Y)),by=.(age,A)]
  support<-merge(data.table::CJ(age=15:45,A=0:1),support,by=c("age","A"),all.x=TRUE)
  stopifnot(!anyNA(support));support[,sparse:=events<20|non_events<20]
  # Progress messages after each whole-data solver pass; return untouched values.
  original_pass<-get("streaming_logistic_pass",envir=.GlobalEnv)
  assign("streaming_logistic_pass",function(...) {
    value<-original_pass(...)
    message(contrast,": completed solver pass; deviance=",format(value$deviance,digits=12))
    value
  },envir=.GlobalEnv)
  for(sc in if(equivalence_test)"main"else c("main","combined")) {
    sp<-spec
    if(sc=="combined") {
      age<-.sep_spec_ns_term("age",spec$age_spec);bmi<-.sep_spec_ns_term("bmi",spec$bmi_spec)
      sp$formula<-as.formula(paste(paste(deparse(spec$formula,width.cutoff=500),collapse=" "),
        "+",paste0("A:",bmi," + A:year_factor + ",age,":year_factor")),env=baseenv())
    }
    id<-paste(contrast,sc,sep="_")
    out<-if(equivalence_test)"outputs/validation/fast_driver_equivalence"else file.path("outputs/models",id)
    dir.create(out,recursive=TRUE,showWarnings=FALSE);rp<-file.path(out,"receipt.json")
    signature<-digest::digest(list(data_columns=dh,identity_columns=ih,reference_columns=dh,
      formula=deparse(sp$formula),age=sp$age_spec,code=code),algo="sha256")
    if(file.exists(rp)) {
      prior<-jsonlite::fromJSON(rp);stopifnot(prior$input_signature==signature)
      for(i in seq_len(nrow(prior$outputs)))stopifnot(age45_sha(prior$outputs$path[i])==prior$outputs$sha256[i])
      message(id,": verified completed bounded-memory fit reused");next
    }
    before<-Sys.time()
    age45_json(list(status="running",model=id,fit_n=nrow(d),input_signature=signature,
      started=as.character(before),fingerprint="columnwise SHA256"),file.path(out,"running.json"))
    message(id,": fitting N=",nrow(d),"; age45 N=",sum(d$age==45))
    fit<-fit_streaming_logit(d,sp$formula,"Y","A","age",sp$age_spec,"HC0",25000L)
    stopifnot(fit$converged,max(abs(fit$final_score))/nrow(d)<1e-12)
    saveRDS(fit,file.path(out,"model.rds"))
    data.table::fwrite(support,file.path(out,"age_arm_support.csv"))
    age45_save_standardization(fit,d,out,root_allowed=sc=="main")
    stopifnot(hash_frame(d)==dh,hash_frame(identity)==ih)
    if(equivalence_test) {
      old<-readRDS("outputs/models/primary_main/model.rds")
      oldage<-read.csv("outputs/models/primary_main/HC0_age_standardized.csv")
      newage<-read.csv(file.path(out,"HC0_age_standardized.csv"))
      stopifnot(identical(fit$beta,old$beta),identical(fit$vcov_HC0,old$vcov_HC0),
        identical(newage,oldage))
    }
    files<-list.files(out,full.names=TRUE);files<-files[basename(files)!="receipt.json"]
    age45_json(list(status="complete_validated_age15_45_fit",model=id,
      eligible_n=eligible,fit_n=nrow(d),events=sum(d$Y),A0=sum(d$A==0),A1=sum(d$A==1),
      age45_n=sum(d$age==45),age45_events=sum(d$Y[d$age==45]),reference_n=nrow(d),
      input_signature=signature,input_signature_method="columnwise SHA256; reference equals fit population",
      data_column_fingerprint=dh,identity_column_fingerprint=ih,annual_sources=sources,
      age_domain=c(15,45),formula=paste(deparse(sp$formula,width.cutoff=500),collapse=" "),
      age_knots=sp$age_spec$knots,age_boundaries=sp$age_spec$boundaries,no_imputation=TRUE,
      iterations=fit$iterations,score_max=max(abs(fit$final_score)),code=code,
      numerical_identity_with_original_driver=if(equivalence_test)TRUE else NULL,
      elapsed_seconds=as.numeric(difftime(Sys.time(),before,units="secs")),
      outputs=data.frame(path=files,sha256=vapply(files,age45_sha,""))),rp)
    message(id,": completed in ",round(as.numeric(difftime(Sys.time(),before,units="secs")))," seconds")
    rm(fit);gc(FALSE)
  }
}
if(sys.nframe()==0L) {a<-commandArgs(TRUE);main(if(length(a))a[1]else"broad",length(a)>1&&a[2]=="test")}
