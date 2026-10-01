# Overall nested-model contrasts conditional on the identical live reference.
overall_paired_source_risks <- function(paired,level=.95) {
  if(!inherits(paired,"sep_paired_source_comparison")||paired$contract$target!="age_specific_empirical"||
     !isTRUE(paired$audits$shared_designs_identical)||paired$independent_fit_approximation_used)
    stop("Validated paired comparison on identical live age-specific reference required")
  if(!is.numeric(level)||length(level)!=1L||!is.finite(level)||level<=0||level>=1)stop("Invalid confidence level")
  n<-paired$contract$n_reference
  left<-overall_streaming_risks(paired$live,as.numeric(n),level)
  right<-overall_streaming_risks(paired$inclusive,as.numeric(n),level)
  if(!identical(left$age_weights,right$age_weights)||!identical(names(left$estimate),names(right$estimate))||
     !identical(colnames(left$gradient),colnames(right$gradient)))stop("Overall source references/parameter order differ")
  p<-ncol(left$gradient);zero<-matrix(0,4,p)
  G<-rbind(cbind(left$gradient,zero),cbind(zero,right$gradient))
  V<-G%*%paired$beta_covariance%*%t(G);V<-(V+t(V))/2
  labels<-c(paste0("live::",names(left$estimate)),paste0("inclusive::",names(right$estimate)))
  dimnames(V)<-list(labels,labels);paired_validate_covariance(V,"Overall paired joint")
  difference<-right$estimate-left$estimate
  DG<-cbind(-left$gradient,right$gradient);DV<-DG%*%paired$beta_covariance%*%t(DG);DV<-(DV+t(DV))/2
  dimnames(DV)<-list(names(difference),names(difference))
  scale<-max(abs(V),.Machine$double.xmin)
  if(any(!is.finite(DV))||min(diag(DV))< -1e-10*scale||
     min(eigen(DV,symmetric=TRUE,only.values=TRUE)$values)< -1e-10*scale)stop("Invalid overall paired difference covariance; no repair")
  degenerate<-diag(DV)<=1e-12*scale;se<-rep(NA_real_,4);se[!degenerate]<-sqrt(diag(DV)[!degenerate])
  tab<-data.frame(measure=c("difference_risk0","difference_risk1","difference_RD","ratio_of_RRs"),
    estimate=unname(difference),lower=unname(difference)-qnorm((1+level)/2)*se,
    upper=unname(difference)+qnorm((1+level)/2)*se,
    analysis_scale=c("identity","identity","identity","log"),analysis_scale_se=se,
    numerically_degenerate=unname(degenerate),reference_n=n)
  tab[4,c("estimate","lower","upper")]<-exp(tab[4,c("estimate","lower","upper")])
  for(f in c("estimate","lower","upper"))tab[[paste0(f,"_per1000")]]<-c(1000*tab[[f]][1:3],NA_real_)
  list(live=left,inclusive=right,joint_covariance=V,difference=difference,
    difference_gradient=DG,difference_covariance=DV,summary=tab,
    contrast="inclusive minus live for risks/RD; inclusive RR divided by live RR for ratio_of_RRs",
    same_live_reference=TRUE,independent_fit_covariance_used=FALSE,
    uncertainty_target="coefficient uncertainty conditional on identical observed-age live-birth empirical reference")
}
