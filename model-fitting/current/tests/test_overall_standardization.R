source("R/04_standardization.R");source("R/06_mi_pooling.R")
source("R/11_streaming_standardization.R");source("R/15_overall_standardization.R")
set.seed(5226)
d<-data.frame(age=sample(c(20,30,40),2000,TRUE,prob=c(.2,.5,.3)),A=rbinom(2000,1,.6),z=rnorm(2000))
d$Y<-rbinom(2000,1,plogis(-2+.5*d$z+.02*(d$age-30)-.2*d$A))
spec<-lock_age_basis(d$age,knots=numeric(),boundaries=c(15,44))
f<-fit_streaming_standardized_logit(d,"Y","A","age","z",spec,"HC0")
s<-standardize_streaming_logit(f,d,c(20,30,40),"age_specific_empirical")
o<-overall_streaming_risks(s,2000)
x<-d;x[spec$columns]<-fixed_age_basis(x$age,spec)
x$A<-0;X0<-model.matrix(f$formula,x);x$A<-1;X1<-model.matrix(f$formula,x)
ref<-logit_risk_contrasts(f$beta,f$vcov,X0,X1)
stopifnot(max(abs(o$estimate-ref$estimates[names(o$estimate)]))<1e-12,
  max(abs(o$gradient-ref$gradient[rownames(o$gradient),]))<1e-12,
  max(abs(o$covariance-ref$covariance[names(o$estimate),names(o$estimate)]))<1e-12,
  abs(sum(o$age_weights$weight)-1)<1e-12,
  inherits(tryCatch(overall_streaming_risks(s,1999),error=identity),"error"))
cat("Overall empirical standardization:5 independent checks passed; no national records.\n")
