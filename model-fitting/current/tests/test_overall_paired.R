for(f in c("04_standardization","06_mi_pooling","11_streaming_standardization","13_paired_source_comparison","15_overall_standardization","20_overall_paired"))source(paste0("R/",f,".R"))
set.seed(290619);n<-900L
d<-data.frame(age=rep(15:44,30),A=rbinom(n,1,.5),z=rnorm(n))
live<-1:750;ids<-paste0(ifelse(seq_len(n)<=750,"natality:","fetal:"),seq_len(n))
d$Y<-rbinom(n,1,plogis(-1+.3*d$A+.2*d$z+.02*d$age+.3*(seq_len(n)>750)))
s<-lock_age_basis(d$age,knots=c(21,27,35));form<-Y~A*splines::ns(age,knots=c(21,27,35),Boundary.knots=c(15,44),intercept=FALSE)+z
fits<-fit_paired_source_models(d,live,ids,form,"Y","A","age",s,chunk_size=200)
pair<-paired_source_comparison(d,live,ids,fits$live,fits$inclusive,15:44,"age_specific_empirical",target_reference_id="synthetic_live",chunk_size=200)
r<-overall_paired_source_risks(pair)
checks<-0L;check<-function(x){stopifnot(isTRUE(x));checks<<-checks+1L}
check(r$same_live_reference&&!r$independent_fit_covariance_used)
check(max(abs(r$difference-(r$inclusive$estimate-r$live$estimate)))<1e-14)
X0<-model.matrix(delete.response(terms(form)),transform(d[live,],A=0))
X1<-model.matrix(delete.response(terms(form)),transform(d[live,],A=1))
manual<-function(beta){a<-mean(plogis(drop(X0%*%beta)));b<-mean(plogis(drop(X1%*%beta)));c(a,b,b-a,log(b/a))}
check(max(abs(r$live$estimate-manual(fits$live$beta)))<1e-12)
check(max(abs(r$inclusive$estimate-manual(fits$inclusive$beta)))<1e-12)
p<-length(fits$live$beta);G<-matrix(NA_real_,4,2*p)
for(j in seq_len(2*p)) {
  lp<-lm<-fits$live$beta;cp<-cm<-fits$inclusive$beta
  if(j<=p){lp[j]<-lp[j]+1e-6;lm[j]<-lm[j]-1e-6}else{cp[j-p]<-cp[j-p]+1e-6;cm[j-p]<-cm[j-p]-1e-6}
  G[,j]<-(manual(cp)-manual(lp)-manual(cm)+manual(lm))/2e-6
}
check(max(abs(G-r$difference_gradient))<1e-8)
check(max(abs(r$difference_covariance-G%*%pair$beta_covariance%*%t(G)))<1e-9)
check(abs(r$summary$estimate[4]-exp(r$difference[4]))<1e-14)
check(max(abs(r$summary$estimate_per1000[1:3]-1000*r$difference[1:3]))<1e-12)
check(!any(r$summary$numerically_degenerate))
bad<-pair;bad$contract$n_reference<-749
check(inherits(tryCatch(overall_paired_source_risks(bad),error=identity),"error"))
cat("PASS:",checks,"overall paired direct-prediction/finite-difference/covariance checks\n")
