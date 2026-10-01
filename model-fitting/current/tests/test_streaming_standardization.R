# Synthetic engineering only. No source input, clinical analysis, or coverage claim.
args<-commandArgs(FALSE);script<-sub("^--file=","",args[grepl("^--file=",args)])
# Rscript may encode spaces as ~+~ in --file when launched from another R process.
script<-gsub("~+~"," ",script,fixed=TRUE)
v2<-dirname(dirname(normalizePath(script,mustWork=TRUE)))
.libPaths(c(file.path(v2,".library"),.libPaths()))
source(file.path(v2,"R/04_standardization.R"));source(file.path(v2,"R/06_mi_pooling.R"))
source(file.path(v2,"R/11_streaming_standardization.R"))
checks<-0L;maximum_error<-numeric()
check<-function(x,label){if(!isTRUE(x))stop("FAILED: ",label);checks<<-checks+1L}
equal<-function(x,y,label,tolerance=1e-8) {
  if(is.numeric(x)&&is.numeric(y)) {
    check(length(x)==length(y)&&all(is.finite(x))&&all(is.finite(y)),paste(label,"finite shape"))
    error<-max(abs(as.numeric(x)-as.numeric(y)))
    maximum_error[[label]]<<-error
    check(error<=tolerance,paste(label,"absolute error",format(error,digits=10)))
  } else check(isTRUE(all.equal(x,y,check.attributes=FALSE,tolerance=tolerance)),label)
}
fails<-function(expr,label)check(inherits(tryCatch(force(expr),error=identity),"error"),label)

set.seed(77119)
n<-1800L
d<-data.frame(age=rep(15:44,each=60),A=rbinom(n,1,.45),z=rnorm(n),
  grp=factor(sample(c("low","mid","high"),n,replace=TRUE),levels=c("low","mid","high")),
  bmi=runif(n,18,42))
d$Y<-rbinom(n,1,plogis(-1.6+.35*d$A+.035*(d$age-30)+.28*d$z+.17*(d$grp=="high")-
  .016*d$A*(d$age-30)+.012*(d$bmi-27)+.001*(d$bmi-27)^2))
age_spec<-lock_age_basis(d$age,knots=c(20,30,40))
ref<-fit_standardized_logit(d,"Y","A","age",c("z","grp"),age_spec,"model")
for(chunk in c(17L,127L,100000L)) {
  fit<-fit_streaming_standardized_logit(d,"Y","A","age",c("z","grp"),age_spec,"model",chunk)
  check(identical(names(fit$beta),names(coef(ref$fit))),"same named coefficient design")
  equal(fit$beta,coef(ref$fit),paste("R04 coefficients",chunk),2e-8)
  # glm's returned QR uses the preceding IRLS weights; independently verify the
  # information at its final coefficients at tighter tolerance below.
  equal(fit$vcov,vcov(ref$fit),paste("R04 returned model covariance",chunk),2e-7)
  rx<-ref$fit$x;rp<-fitted(ref$fit)
  final_ref_covariance<-solve(crossprod(rx,rx*(rp*(1-rp))))
  equal(fit$vcov,final_ref_covariance,paste("R04 final-coefficient information",chunk),2e-9)
  equal(fit$deviance,ref$fit$deviance,paste("R04 deviance",chunk),1e-8)
  check(fit$converged&&fit$rank==length(fit$beta),"full rank converged")
  check(!fit$retained_full_design&&!any(c("x","model","fitted.values","y","data") %in% names(fit)),"no full model/target arrays retained")
  check(fit$largest_design_matrix_rows<=min(n,chunk),"training design obeys chunk bound")
  check(max(abs(fit$final_score))/n<1e-12,"final normalized score tolerance")
  for(target in c("age_specific_empirical","common_empirical")) {
    s<-standardize_streaming_logit(fit,d,c(20,30,40),target,chunk_size=53L)
    r<-standardize_logit(ref,d,c(20,30,40),target)
    equal(as.matrix(s$summary),as.matrix(r$summary),paste("R04 standardized summary",chunk,target),2e-8)
    components<-streaming_standardized_mi_components(s,"fixed_synthetic_target")
    rc<-standardized_mi_components(r,"fixed_synthetic_target")
    equal(components$estimate,rc$estimate,paste("R06 standardized vector",chunk,target),2e-8)
    equal(components$gradient,rc$gradient,paste("R06 joint gradients",chunk,target),2e-8)
    equal(components$covariance,rc$covariance,paste("R06 cross-age covariance",chunk,target),2e-8)
    check(identical(components$metadata,rc$metadata),"R06 metadata semantics")
    check(!s$stored_target_designs&&!"matrices" %in% names(s),"no target design storage")
    check(s$largest_target_design_rows<=53L,"target design obeys chunk bound")
    check(any(abs(components$covariance[1:4,5:8])>1e-9),"nonzero cross-age covariance retained")
  }
}

# Explicit literal dynamic BMI + age splines match glm, including factors whose
# levels are not observed in the first chunk. No precomputed nonlinear columns.
formula<-Y ~ A * splines::ns(age,knots=c(20,30,40),Boundary.knots=c(15,44),intercept=FALSE) +
  splines::ns(bmi,knots=c(23,28,34),Boundary.knots=c(13,69.9),intercept=FALSE) + z + grp
glm_ref<-glm(formula,d,family=binomial(),x=TRUE,model=TRUE,control=glm.control(epsilon=1e-12,maxit=100))
dyn<-fit_streaming_logit(d,formula,"Y","A","age",age_spec,"HC0",chunk_size=79L)
equal(dyn$beta,coef(glm_ref),"dynamic fixed spline glm coefficients",3e-8)
equal(dyn$vcov_model,vcov(glm_ref),"dynamic returned model covariance",2e-7)
if(!requireNamespace("sandwich",quietly=TRUE))stop("Existing sandwich package required for independent HC0 check.")
equal(dyn$vcov_HC0,sandwich::vcovHC(glm_ref,type="HC0"),"dynamic returned HC0 covariance",4e-7)
X<-model.matrix(glm_ref);mu<-fitted(glm_ref)
manual_bread<-solve(crossprod(X,X*(mu*(1-mu))))
manual_HC0<-manual_bread%*%crossprod(X,X*(d$Y-mu)^2)%*%manual_bread
equal(dyn$vcov_model,manual_bread,"dynamic final-coefficient information",2e-9)
equal(dyn$vcov,manual_HC0,"HC0 independent score outer products",3e-9)
equal(dyn$vcov,dyn$vcov_HC0,"requested HC0 selected")
dyn2<-fit_streaming_logit(d,formula,"Y","A","age",age_spec,"HC0",chunk_size=181L)
equal(dyn2$beta,dyn$beta,"coefficient chunk invariance",1e-10)
equal(dyn2$vcov,dyn$vcov,"HC0 chunk invariance",1e-10)
for(target in c("age_specific_empirical","common_empirical")) {
  s<-standardize_streaming_logit(dyn,d,c(20,30,40),target,chunk_size=37L)
  s2<-standardize_streaming_logit(dyn,d,c(20,30,40),target,chunk_size=119L)
  equal(s$estimate,s2$estimate,paste("target risk chunk invariance",target),1e-12)
  equal(s$gradient,s2$gradient,paste("target gradient chunk invariance",target),1e-12)
  for(i in seq_len(3L)) {
    age<-c(20,30,40)[i]
    target_d<-if(target=="age_specific_empirical")d[d$age==age,,drop=FALSE] else d
    target_d$age<-age;target_d$A<-0
    X0<-model.matrix(delete.response(terms(glm_ref)),target_d,contrasts.arg=glm_ref$contrasts,xlev=glm_ref$xlevels)
    target_d$A<-1
    X1<-model.matrix(delete.response(terms(glm_ref)),target_d,contrasts.arg=glm_ref$contrasts,xlev=glm_ref$xlevels)
    manual<-logit_risk_contrasts(dyn$beta,dyn$vcov,X0,X1)
    j<-4*(i-1)+1:4
    equal(s$estimate[j],manual$estimates[c("risk0","risk1","rd","log_rr")],paste("dynamic direct risks",target,age),1e-12)
    equal(s$gradient[j,],manual$gradient[c("risk0","risk1","rd","log_rr"),],paste("dynamic direct gradients",target,age),1e-12)
  }
  comp<-streaming_standardized_mi_components(s,"same_membership")
  pooled<-pool_standardized_mi(list(comp,comp),df_complete=n-length(dyn$beta))
  equal(pooled$pooled,comp$estimate,paste("R06 pooling direct component interface",target),1e-12)
  equal(pooled$total,comp$covariance,paste("R06 identical completion total covariance",target),1e-12)
}

# General transformed terms and a final one-row chunk must remain deterministic.
small<-d[1:181,];small$age<-rep(20:40,length.out=nrow(small))
fsmall<-Y ~ A*age + I(z^2) + grp
one<-fit_streaming_logit(small,fsmall,"Y","A","age",chunk_size=30L)
ordinary<-glm(fsmall,small,family=binomial(),control=glm.control(epsilon=1e-12,maxit=100))
equal(one$beta,coef(ordinary),"last one-row chunk coefficients",5e-8)
singleton<-standardize_streaming_logit(one,small[1,,drop=FALSE],20,"common_empirical",chunk_size=1L)
check(singleton$summary$target_n==1L&&singleton$largest_target_design_rows==1L,"singleton target works")
if(requireNamespace("data.table",quietly=TRUE)) {
  dtfit<-fit_streaming_logit(data.table::as.data.table(small),fsmall,"Y","A","age",chunk_size=30L)
  equal(dtfit$beta,one$beta,"data.table subclass safe chunk selection",1e-12)
}
check(length(ls(environment(dyn$formula),all.names=TRUE))==1L,"formula environment contains only ns, not caller data")

# Actual R10 locked formula integration, without clinical source preparation.
source(file.path(v2,"R/10_model_specification.R"))
locked<-lock_sep_model_specification("natality_main","within_prepregnancy_smokers",2019:2024,
  age_knots=c(20,28,36),bmi_knots=c(22,27,32))
dm<-d[c("Y","A","age","bmi")]
for(f in c("year_factor","race_ethnicity","prepreg_diabetes","prior_living4","education4",
  "prior_preterm","prior_cesarean","nativity","prepreg_dose5")) {
  k<-c(year_factor=6,race_ethnicity=7,prepreg_diabetes=2,prior_living4=4,education4=4,
    prior_preterm=2,prior_cesarean=2,nativity=2,prepreg_dose5=5)[[f]]
  dm[[f]]<-factor(sample(seq_len(k),n,replace=TRUE),levels=seq_len(k))
}
locked_fit<-fit_streaming_logit(dm,locked$formula,locked$outcome,locked$exposure,locked$age,
  locked$age_spec,"HC0",chunk_size=121L)
locked_ref<-glm(locked$formula,dm,family=binomial(),x=TRUE,control=glm.control(epsilon=1e-14,maxit=100))
equal(locked_fit$beta,coef(locked_ref),"R10 locked formula coefficients",2e-8)
lx<-locked_ref$x;lp<-fitted(locked_ref)
lv<-solve(crossprod(lx,lx*(lp*(1-lp))))
equal(locked_fit$vcov_model,lv,"R10 final-information covariance",3e-9)
ls<-standardize_streaming_logit(locked_fit,dm,c(20,30,40),"age_specific_empirical")
check(identical(ls$summary$target_n,rep(60L,3)),"R10 age-specific counts")
check(all(diff(locked_fit$trace$deviance)<=1e-9),"IRLS objective nonincreasing within numerical tolerance")

# Independent finite differences for selected streamed joint gradients.
base_s<-standardize_streaming_logit(dyn,d,c(20,30),"age_specific_empirical",chunk_size=19L)
for(j in c(1L,3L,8L)) {
  plus<-minus<-dyn;plus$beta[j]<-plus$beta[j]+1e-6;minus$beta[j]<-minus$beta[j]-1e-6
  numerical<-(standardize_streaming_logit(plus,d,c(20,30),"age_specific_empirical",chunk_size=19L)$estimate-
    standardize_streaming_logit(minus,d,c(20,30),"age_specific_empirical",chunk_size=19L)$estimate)/2e-6
  equal(base_s$gradient[,j],numerical,paste("streamed finite-difference gradient",j),1e-8)
}

# Failure/refusal tests: no imputed outcome, hidden exclusions, basis refitting,
# ridge fallback, unsupported weighting, extrapolation or silent factor drift.
bad<-d;bad$z[1]<-NA;fails(fit_streaming_logit(bad,formula,"Y","A","age"),"missing baseline rejected")
bad<-d;bad$Y[1]<-NA;fails(fit_streaming_logit(bad,formula,"Y","A","age"),"missing outcome rejected")
bad<-d;bad$A<-1;fails(fit_streaming_logit(bad,formula,"Y","A","age"),"one exposure arm rejected")
bad<-d;bad$Y<-0;fails(fit_streaming_logit(bad,formula,"Y","A","age"),"one outcome state rejected")
bad<-d;bad$z[1]<-Inf;fails(fit_streaming_logit(bad,formula,"Y","A","age"),"nonfinite baseline rejected")
bad<-d;bad$grp<-as.character(bad$grp);fails(fit_streaming_logit(bad,formula,"Y","A","age"),"implicit character factors rejected")
bad<-d;bad$dup<-bad$z;fails(fit_streaming_logit(bad,Y~A*age+z+dup,"Y","A","age"),"rank deficiency rejected")
bad<-d;bad$Y<-bad$A;fails(fit_streaming_logit(bad,Y~A+age,"Y","A","age"),"separation/boundary rejected")
fails(fit_streaming_logit(d,formula,"Y","A","age",maxit=1),"nonconvergence rejected")
fails(fit_streaming_logit(d,Y~A*splines::ns(age,df=4)+z,"Y","A","age"),"df-only age basis rejected")
fails(fit_streaming_logit(d,Y~A*age+scale(z),"Y","A","age"),"data-dependent scaling rejected")
fails(fit_streaming_logit(d,Y~A*age+factor(grp),"Y","A","age"),"within-formula factor recoding rejected")
fails(fit_streaming_logit(d,Y~.,"Y","A","age"),"implicit formula expansion rejected")
fails(fit_streaming_logit(d,Y~A*age+Y,"Y","A","age"),"outcome leakage on RHS rejected")
fails(fit_streaming_logit(d,Y~A*age+offset(z),"Y","A","age"),"offset explicitly unsupported")
fails(fit_streaming_logit(d,formula,"Y","A","age",chunk_size=0),"invalid chunk size rejected")
fails(fit_streaming_logit(d,formula,"Y","A","age",epsilon=NA_real_),"invalid convergence control rejected")
bad<-d;bad$bmi[1]<-70;fails(fit_streaming_logit(bad,formula,"Y","A","age"),"BMI extrapolation rejected")
fails(standardize_streaming_logit(dyn,d,45,"common_empirical"),"age extrapolation rejected")
fails(standardize_streaming_logit(dyn,d,20.5,"age_specific_empirical"),"empty exact-age target rejected")
fails(standardize_streaming_logit(dyn,d,c(30,20),"common_empirical"),"unordered age grid rejected")
bad<-d;bad$grp<-factor(rep("unseen",n),levels=c("unseen","other"))
fails(standardize_streaming_logit(dyn,bad,30,"common_empirical"),"new factor level rejected")
fails(streaming_standardized_mi_components(standardize_streaming_logit(dyn,d,30,"common_empirical"),""),"missing target identity rejected")
check(!any(c("sampling_weights","RECWT") %in% names(formals(fit_streaming_logit))),"no undocumented weight interface")
cat("Passed",checks,"synthetic streaming-standardization checks. No source/national records read.\n")
cat("Largest absolute reference-comparison errors:\n")
print(head(sort(maximum_error,decreasing=TRUE),8))
