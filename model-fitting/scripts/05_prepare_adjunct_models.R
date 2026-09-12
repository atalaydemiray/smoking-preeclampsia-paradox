main <- function() {
  source("R/17_fit_helpers.R");age45_initialize()
  source("R/06_linked_mortality_preparation.R")
  source("R/07_pregnancy_endpoint_preparation.R")
  out<-"derived/adjunct_models"
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  wait_for<-function(path) {
    waited<-0L
    while(!file.exists(path)) {
      if(waited>=1800L)stop("Timed out waiting for validated source: ",path)
      if(waited%%60L==0L)message("Waiting: ",basename(dirname(path)),"/",basename(path))
      Sys.sleep(15);waited<-waited+15L
    }
  }
  save_checked<-function(p,stem,input_sha) {
    path<-file.path(out,paste0(stem,".rds"))
    rp<-file.path(out,paste0(stem,"_receipt.json"))
    saveRDS(p,path,compress="gzip");ch<-readRDS(path)
    stopifnot(isTRUE(all.equal(ch,p,tolerance=0,check.attributes=TRUE)))
    cc<-complete.cases(p$data[p$spec$covariates])
    age45_json(list(status="complete_prepared_age15_45",n=nrow(p$data),
      complete_n=sum(cc),events=sum(p$data$Y[cc]),age45_complete_n=sum(cc&p$data$age==45),
      input_sha256=input_sha,output_path=path,output_sha256=age45_sha(path)),rp)
  }
  for(src in c("natality","fetal_death"))for(y in 2018:2024) {
    dir<-if(src=="natality")"natality" else "fetal"
    path<-file.path("derived",dir,paste0(y,"_clinical.rds"))
    rp<-sub("_clinical.rds","_receipt.json",path,fixed=TRUE)
    wait_for(rp)
    r<-jsonlite::fromJSON(rp)
    expected<-if(src=="natality")r$outputs$sha256[grepl("clinical[.]rds$",r$outputs$path)] else r$output_sha256
    stopifnot(length(expected)==1L,age45_sha(path)==expected)
    d<-readRDS(path)$data
    for(tier in c("shared_core","shared_augmented")) {
      stem<-paste("gh",y,src,tier,sep="_")
      if(!file.exists(file.path(out,paste0(stem,"_receipt.json")))) {
        spec<-lock_sep_model_specification(tier,"within_prepregnancy_smokers",
          2018:2024,c(21,27,35),if(tier=="shared_core")NULL else c(19.6,26.1,37.8))
        p<-prepare_sep_model_data(as.data.frame(d)[which(d$primary_member),,drop=FALSE],spec,src)
        save_checked(p,stem,expected);rm(p);gc(FALSE)
      }
      stem<-paste("fetal_endpoint",y,src,tier,sep="_")
      if(!file.exists(file.path(out,paste0(stem,"_receipt.json")))) {
        spec<-lock_pregnancy_endpoint_spec(tier)
        p<-prepare_pregnancy_endpoint(d,spec,src)
        save_checked(p,stem,expected);rm(p);gc(FALSE)
      }
    }
    message("Adjunct preparation: ",src," ",y)
    rm(d);gc(FALSE)
  }
  for(ep in c("infant","neonatal","early_neonatal")) {
    stem<-paste0("linked_",ep)
    if(file.exists(file.path(out,paste0(stem,"_receipt.json"))))next
    oldpath<-file.path("derived/model_inputs/linked_mortality_2018_2023_20260906_a",paste0(ep,"_model_scale.rds"))
    oldrp<-file.path(dirname(oldpath),paste0(ep,"_receipt.json"))
    r<-jsonlite::fromJSON(oldrp)
    ent<-r$outputs[basename(r$outputs$path)==basename(oldpath),]
    stopifnot(nrow(ent)==1L,age45_sha(oldpath)==ent$sha256)
    old<-readRDS(oldpath)
    spec<-lock_sep_model_specification("natality_main","within_prepregnancy_smokers",
      2018:2023,c(21,27,35),c(19.6,26.1,37.8))
    parts<-ids<-list();inputs<-c(ent$sha256)
    for(y in 2018:2023) {
      path<-file.path("derived/linked_age45",paste0(y,"_clinical.rds"))
      wait_for(sub("_clinical.rds","_receipt.json",path,fixed=TRUE))
      r<-jsonlite::fromJSON(sub("_clinical.rds","_receipt.json",path,fixed=TRUE))
      stopifnot(r$all_age45_numerator_links_reconciled,age45_sha(path)==r$output_sha256)
      d<-readRDS(path)$data
      z<-prepare_linked_mortality_model(d,spec,ep)
      stopifnot(all(z$data$age==45),!z$GH_required,!z$GH_adjusted)
      # Match the original pooled identity schema. These are prepared-row
      # ordinals; source_row retains the immutable full-file record number.
      z$identity$annual_input_row<-sum(old$identity$year==y)+seq_len(nrow(z$data))
      z$identity$input_row<-nrow(old$data)+sum(vapply(parts,nrow,0L))+seq_len(nrow(z$data))
      parts[[as.character(y)]]<-z$data;ids[[as.character(y)]]<-z$identity
      inputs<-c(inputs,r$output_sha256)
    }
    p<-old
    p$data<-as.data.frame(data.table::rbindlist(c(list(old$data),parts),use.names=TRUE))
    p$identity<-as.data.frame(data.table::rbindlist(c(list(old$identity),ids),use.names=TRUE))
    p$spec<-z$spec;p$n_selected<-nrow(p$data)
    stopifnot(identical(p$data[seq_len(nrow(old$data)),],old$data),
      identical(p$identity[seq_len(nrow(old$data)),],old$identity),
      identical(p$identity$input_row,seq_len(nrow(p$data))),
      !anyDuplicated(p$identity[c("source_type","year","source_row")]))
    # Only data and identity are consumed by fitting. Avoid retaining old
    # status frames whose row count would no longer match the amended cohort.
    p$missingness<-p$original_status<-NULL
    p$n_input<-NA_integer_
    p$construction<-list(legacy_input_sha256=ent$sha256,added_age45_source_sha256=inputs[-1],
      original_data_unchanged=TRUE,no_imputation=TRUE)
    save_checked(p,stem,inputs)
    message("Linked ",ep,": added age45 eligible=",nrow(p$data)-nrow(old$data))
  }
}
if(sys.nframe()==0L)main()
