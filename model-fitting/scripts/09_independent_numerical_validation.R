# Independent model.matrix-based full-record audit of the three main fits.
# Shares no streaming score, Hessian, meat or standardization calculations.
main <- function(contrast="primary") {
  source("R/17_fit_helpers.R");age45_initialize()
  stopifnot(contrast%in%c("primary","broad","prepregnancy"))
  base<-file.path("outputs/models",paste0(contrast,"_main"))
  fit<-readRDS(file.path(base,"model.rds"));rp<-file.path(base,"receipt.json")
  receipt<-jsonlite::fromJSON(rp);stopifnot(receipt$status=="complete_validated_age15_45_fit")
  b<-fit$beta;k<-length(b);H<-M<-matrix(0,k,k);score<-numeric(k)
  risk<-matrix(0,31,2);grad<-array(0,c(31,2,k));counts<-integer(31);n<-events<-0
  samples<-age45<-list()
  makeX<-function(d) {
    x<-stats::model.matrix(fit$formula,d)
    stopifnot(identical(colnames(x),names(b)));x
  }
  for(y in 2016:2024) {
    path<-file.path("derived/model_inputs",paste0(y,"_",contrast,".rds"))
    rr<-jsonlite::fromJSON(sub(".rds","_receipt.json",path,fixed=TRUE))
    stopifnot(age45_sha(path)==rr$output_sha256)
    p<-readRDS(path);d<-p$data[complete.cases(p$data[p$spec$covariates]),,drop=FALSE]
    d$year_factor<-factor(as.character(d$year_factor),levels=2016:2024)
    set.seed(20260907+y);samples[[as.character(y)]]<-d[sample.int(nrow(d),min(6000L,nrow(d))),,drop=FALSE]
    age45[[as.character(y)]]<-d[d$age==45,,drop=FALSE]
    n<-n+nrow(d);events<-events+sum(d$Y)
    for(first in seq.int(1L,nrow(d),by=25000L)) {
      q<-d[first:min(nrow(d),first+24999L),,drop=FALSE];g<-q$age-14L
      x<-makeX(q);pr<-plogis(as.numeric(x%*%b));res<-q$Y-pr
      score<-score+drop(crossprod(x,res));H<-H+crossprod(x,x*as.numeric(pr*(1-pr)))
      M<-M+crossprod(x,x*as.numeric(res^2));counts<-counts+tabulate(g,31)
      for(a in 0:1) {
        q$A<-a;x<-makeX(q);pr<-plogis(drop(x%*%b))
        rs<-rowsum(cbind(pr,x*as.numeric(pr*(1-pr))),g,reorder=FALSE)
        at<-as.integer(rownames(rs));risk[at,a+1L]<-risk[at,a+1L]+rs[,1]
        grad[at,a+1L,]<-grad[at,a+1L,]+rs[,-1,drop=FALSE]
      }
    }
    message(contrast,": independent full-record audit ",y)
    rm(p,d,q,x);gc(FALSE)
  }
  Vm<-solve(H);Vh<-Vm%*%M%*%Vm
  err<-c(model_covariance=max(abs(Vm-fit$vcov_model)),HC0_covariance=max(abs(Vh-fit$vcov_HC0)),
    mean_score=max(abs(score))/n)
  stopifnot(n==receipt$fit_n,events==receipt$events,all(counts>0),
    err[["mean_score"]]<1e-12,err[["model_covariance"]]<1e-8,err[["HC0_covariance"]]<1e-8)
  risk<-risk/counts
  for(a in 1:2)grad[,a,]<-grad[,a,]/counts
  exact<-read.csv(file.path(base,"HC0_age_standardized.csv"))
  stopifnot(identical(as.integer(exact$target_n),counts),max(abs(risk-cbind(exact$risk0,exact$risk1)))<1e-10)
  G<-matrix(0,31,k)
  for(i in 1:31)G[i,]<-grad[i,2,]-grad[i,1,]
  rdse<-sqrt(rowSums((G%*%Vh)*G))
  err<-c(err,risk=max(abs(risk-cbind(exact$risk0,exact$risk1))),RD_SE=max(abs(rdse-exact$se_rd)))
  stopifnot(err[["RD_SE"]]<1e-10)
  # Analytic versus central finite-difference marginal risk gradients at 45.
  q<-as.data.frame(data.table::rbindlist(age45));graderr<-0
  for(a in 0:1) {
    q$A<-a;x<-makeX(q);pr<-plogis(drop(x%*%b));ga<-colMeans(x*as.numeric(pr*(1-pr)))
    gf<-vapply(seq_along(b),function(j){delta<-rep(0,k);delta[j]<-1e-5
      upper<-mean(plogis(drop(x%*%(b+delta))))
      lower<-mean(plogis(drop(x%*%(b-delta))))
      (upper-lower)/2e-5},0.0)
    graderr<-max(graderr,max(abs(ga-gf)))
  }
  stopifnot(graderr<1e-8)
  # Separate sample refits verify the optimizer against standard glm.
  sm<-as.data.frame(data.table::rbindlist(samples));sm$year_factor<-factor(as.character(sm$year_factor),levels=2016:2024)
  standard<-stats::glm(fit$formula,data=sm,family=binomial(),control=glm.control(epsilon=1e-11,maxit=50))
  streaming<-fit_streaming_logit(sm,fit$formula,"Y","A","age",fit$age_spec,"HC0",25000L)
  stopifnot(standard$converged,streaming$converged,!anyNA(coef(standard)))
  glmerr<-max(abs(coef(standard)-streaming$beta))
  stopifnot(glmerr<1e-6)
  out<-file.path("outputs/validation",contrast);dir.create(out,recursive=TRUE,showWarnings=FALSE)
  age45_json(list(status="passed_independent_full_record_numerical_validation",contrast=contrast,
    records=n,events=events,age45_records=counts[31],no_imputation=TRUE,
    numerical_errors=as.list(c(err,age45_gradient=graderr,glm_coefficients=glmerr)),
    glm_sample_n=nrow(sm),source_fit_receipt_sha256=age45_sha(rp),
    script_sha256=age45_sha("scripts/09_independent_numerical_validation.R")),file.path(out,"receipt.json"))
}
if(sys.nframe()==0L) {a<-commandArgs(TRUE);main(if(length(a))a[1]else"primary")}
