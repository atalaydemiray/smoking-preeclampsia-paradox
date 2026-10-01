# Overall empirical risks from the complete age-specific standardized grid.
overall_streaming_risks <- function(standardized,expected_target_n,level=.95) {
  if(!inherits(standardized,"sep_streaming_standardized_risks") ||
      standardized$target!="age_specific_empirical") stop("Age-specific empirical standardized risks required.")
  d <- standardized$summary
  if(length(expected_target_n)!=1L||!is.numeric(expected_target_n)||!is.finite(expected_target_n)||
     expected_target_n<1||expected_target_n!=floor(expected_target_n)||sum(d$target_n)!=expected_target_n)
    stop("Full prespecified target membership must be covered by the age grid.")
  if(length(level)!=1L||!is.numeric(level)||!is.finite(level)||level<=0||level>=1) stop("Invalid confidence level.")
  w <- d$target_n/expected_target_n
  r <- c(risk0=sum(w*d$risk0),risk1=sum(w*d$risk1))
  if(any(!is.finite(r)|r<=0|r>=1)) stop("Invalid overall empirical risk.")
  g <- do.call(rbind,lapply(c("risk0","risk1"),function(f) {
    i <- which(standardized$metadata$measure==f)
    if(!identical(standardized$metadata$age[i],d$age)) stop("Age-gradient order mismatch.")
    colSums(standardized$gradient[i,,drop=FALSE]*w)
  }))
  rownames(g)<-c("risk0","risk1")
  gradient <- rbind(g,rd=g[2,]-g[1,],log_rr=g[2,]/r[2]-g[1,]/r[1])
  estimate <- c(r,rd=unname(r[2]-r[1]),log_rr=unname(log(r[2])-log(r[1])))
  covariance <- gradient%*%standardized$object$vcov%*%t(gradient)
  covariance <- (covariance+t(covariance))/2
  mi_validate_covariance(covariance,names(estimate),"Overall empirical risk covariance")
  se <- sqrt(diag(covariance));z <- qnorm((1+level)/2)
  lo <- hi <- estimate
  for(f in c("risk0","risk1")) {
    ci <- plogis(qlogis(estimate[f])+c(-1,1)*z*se[f]/(estimate[f]*(1-estimate[f])))
    lo[f]<-ci[1];hi[f]<-ci[2]
  }
  lo["rd"]<-estimate["rd"]-z*se["rd"];hi["rd"]<-estimate["rd"]+z*se["rd"]
  lo["log_rr"]<-exp(estimate["log_rr"]-z*se["log_rr"])
  hi["log_rr"]<-exp(estimate["log_rr"]+z*se["log_rr"])
  shown <- estimate;shown["log_rr"]<-exp(shown["log_rr"])
  tab <- data.frame(measure=c("risk0","risk1","rd","rr"),estimate=unname(shown),
    lower=unname(lo),upper=unname(hi),pooling_scale_se=unname(se),target_n=expected_target_n)
  list(summary=tab,estimate=estimate,gradient=gradient,covariance=covariance,
    age_weights=data.frame(age=d$age,weight=w),
    uncertainty_target=standardized$uncertainty_target,
    interpretation="Overall empirical target retains each record's observed age; not risk at an assigned mean age")
}
