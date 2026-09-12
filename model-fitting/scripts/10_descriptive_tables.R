# Recompute baseline characteristics and missingness from all amended records.
main <- function() {
  source("R/17_fit_helpers.R");age45_initialize()
  out<-"outputs/descriptive";dir.create(out,recursive=TRUE,showWarnings=FALSE)
  rows<-counts<-missing<-flow<-list()
  for(ct in c("primary","broad","prepregnancy"))for(y in 2016:2024) {
    stem<-file.path("derived/model_inputs",paste(y,ct,sep="_"))
    rp<-paste0(stem,"_receipt.json");r<-jsonlite::fromJSON(rp)
    stopifnot(age45_sha(paste0(stem,".rds"))==r$output_sha256)
    p<-readRDS(paste0(stem,".rds"));cc<-complete.cases(p$data[p$spec$covariates])
    counts[[paste(ct,y)]]<-data.frame(contrast=ct,year=y,eligible_n=nrow(p$data),
      included_n=sum(cc),excluded_n=sum(!cc),included_events=sum(p$data$Y[cc]),
      excluded_events=sum(p$data$Y[!cc]),age45_n=sum(cc&p$data$age==45))
    m<-p$missingness_counts;m$contrast<-ct;m$year<-y;missing[[paste(ct,y)]]<-m
    d<-p$data[cc,,drop=FALSE];rm(p);gc(FALSE)
    for(a in 0:1)for(v in setdiff(names(d),c("A","Y"))) {
      x<-d[[v]][d$A==a];n<-length(x)
      if(is.numeric(x))q<-data.frame(contrast=ct,A=a,variable=v,level="",kind="continuous",n=n,sum=sum(x),sum_sq=sum(x*x)) else {
        z<-table(x);q<-data.frame(contrast=ct,A=a,variable=v,level=names(z),kind="categorical",n=as.integer(z),sum=0,sum_sq=0)
      }
      rows[[length(rows)+1L]]<-q
    }
    message("Descriptive recomputation: ",ct," ",y);rm(d);gc(FALSE)
  }
  z<-data.table::rbindlist(rows)[,lapply(.SD,sum),by=.(contrast,A,variable,level,kind),.SDcols=c("n","sum","sum_sq")]
  denom<-z[variable=="age",.(contrast,A,denominator=n)]
  z<-merge(z,denom,by=c("contrast","A"),sort=FALSE)
  z[,c("mean","sd","pct"):=list(NA_real_,NA_real_,NA_real_)]
  z[kind=="continuous",`:=`(mean=sum/n,sd=sqrt((sum_sq-sum^2/n)/(n-1)))]
  z[kind=="categorical",pct:=100*n/denominator]
  stopifnot(all(z[kind=="categorical",.(sum=sum(n),den=unique(denominator)),by=.(contrast,A,variable)][,sum==den]))
  z[,display:=ifelse(kind=="continuous",sprintf("%.1f (%.1f)",mean,sd),
    paste0(format(n,big.mark=",",trim=TRUE)," (",sprintf("%.1f",pct),"%)"))]
  co<-data.table::rbindlist(counts)
  pooled<-co[,lapply(.SD,sum),by=contrast,.SDcols=setdiff(names(co),c("contrast","year"))]
  pooled[,`:=`(excluded_pct=100*excluded_n/eligible_n,
    included_GH_risk_per1000=1000*included_events/included_n,
    excluded_GH_risk_per1000=1000*excluded_events/excluded_n)]
  for(y in 2016:2024) {
    x<-data.table::fread(file.path("derived/natality",paste0(y,"_ledger.csv")))
    flow[[as.character(y)]]<-x
  }
  flow<-data.table::rbindlist(flow)
  data.table::fwrite(z,file.path(out,"baseline_all_three_contrasts.csv"))
  data.table::fwrite(co,file.path(out,"cc_exclusions_by_year.csv"))
  data.table::fwrite(pooled,file.path(out,"numeric_cc_exclusions_overall.csv"))
  data.table::fwrite(data.table::rbindlist(missing,fill=TRUE),file.path(out,"missingness_by_variable_year.csv"))
  data.table::fwrite(flow,file.path(out,"common_clinical_flow_by_year.csv"))
  age45_json(list(status="complete_full_record_descriptive_recomputation",no_imputation=TRUE,
    script_sha256=age45_sha("scripts/10_descriptive_tables.R")),file.path(out,"receipt.json"))
}
if(sys.nframe()==0L)main()
