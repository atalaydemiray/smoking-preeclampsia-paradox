# Chunked unweighted binary-logistic estimating equations and standardization.
# Source R/09_standardization.R for fixed-basis/parameter validation utilities.
# No source I/O, imputations, regularization, sampling, or national fit here.

streaming_check_chunk <- function(chunk_size) {
  if(length(chunk_size)!=1L||!is.numeric(chunk_size)||!is.finite(chunk_size)||
    chunk_size<1||chunk_size!=floor(chunk_size)||chunk_size>100000L)
    stop("chunk_size must be an integer1-100000.")
  as.integer(chunk_size)
}

streaming_check_columns <- function(data,columns) {
  if(!is.data.frame(data)||!nrow(data)||anyDuplicated(names(data))||any(!columns %in% names(data)))
    stop("A nonempty frame with unique required columns is required.")
  for(f in columns) {
    x<-data[[f]]
    if(is.matrix(x)||is.list(x)||is.character(x)||(!is.numeric(x)&&!is.factor(x)&&!is.logical(x)))
      stop("Model columns must be numeric/logical or explicitly leveled factors: ",f)
    if(anyNA(x))stop("Missing analysis values: no implicit complete-case deletion: ",f)
    if(is.numeric(x)&&any(!is.finite(x)))stop("Nonfinite analysis value: ",f)
    if(is.factor(x)&&(nlevels(x)<2L||anyNA(levels(x))))stop("Invalid factor levels: ",f)
  }
  invisible(TRUE)
}

streaming_formula_spec <- function(formula,outcome,exposure,age,age_spec=NULL) {
  if(!inherits(formula,"formula")||length(formula)!=3L||!is.symbol(formula[[2]])||
    as.character(formula[[2]])!=outcome)stop("Formula requires an explicit untransformed named binary outcome.")
  if(outcome %in% all.vars(formula[[3]]))stop("Outcome cannot also appear on the predictor side.")
  bounds<-list()
  call_name<-function(x) {
    h<-x[[1]]
    if(is.symbol(h))return(as.character(h))
    if(is.call(h)&&identical(h[[1]],as.name("::"))&&as.character(h[[2]])=="splines"&&as.character(h[[3]])=="ns")return("ns")
    stop("Unsupported formula function; use explicit fixed transformations.")
  }
  literal<-function(x) {
    if(is.numeric(x)||is.logical(x))return(as.numeric(x))
    if(is.call(x)&&as.character(x[[1]]) %in% c("c","+","-")) {
      z<-lapply(as.list(x)[-1],literal)
      if(as.character(x[[1]])=="c")return(unlist(z,use.names=FALSE))
      if(length(z)==1L)return(if(as.character(x[[1]])=="-")-z[[1]] else z[[1]])
      if(length(z)==2L)return(if(as.character(x[[1]])=="-")z[[1]]-z[[2]] else z[[1]]+z[[2]])
    }
    stop("Spline knots/bounds must be literal numeric constants, not data-dependent expressions.")
  }
  inspect<-function(x) {
    if(is.symbol(x)) {if(as.character(x)==".")stop("Explicit formula terms required; dot expansion forbidden.");return(invisible(NULL))}
    if(!is.call(x))return(invisible(NULL))
    name<-call_name(x)
    if(name=="ns") {
      a<-as.list(x)[-1];nms<-names(a)
      if(is.null(nms)||!all(c("knots","Boundary.knots") %in% nms)||"df" %in% nms||!is.symbol(a[[1]]))
        stop("ns requires a named data column and explicit fixed knots/Boundary.knots; no df-only basis.")
      knots<-literal(a[["knots"]]);boundary<-literal(a[["Boundary.knots"]])
      if(length(boundary)!=2L||any(!is.finite(boundary))||boundary[1]>=boundary[2]||
        any(!is.finite(knots))||is.unsorted(knots,strictly=TRUE)||any(knots<=boundary[1]|knots>=boundary[2]))
        stop("Invalid fixed spline constants.")
      if("intercept" %in% nms) {
        iv<-literal(a[["intercept"]]);if(length(iv)!=1L||!iv %in% 0:1)stop("Spline intercept must be a literal logical.")
      }
      f<-as.character(a[[1]])
      if(!is.null(bounds[[f]])&&!identical(bounds[[f]],boundary))stop("Conflicting spline boundaries for one variable.")
      bounds[[f]]<<-boundary;return(invisible(NULL))
    }
    if(!name %in% c("~","+","-","*","/",":","^","(","I","log","log1p","sqrt","exp","abs"))
      stop("Unsupported/data-dependent formula operation: ",name)
    lapply(as.list(x)[-1],inspect);invisible(NULL)
  }
  inspect(formula)
  vars<-all.vars(formula)
  if(anyDuplicated(c(outcome,exposure,age))||any(make.names(c(outcome,exposure,age))!=c(outcome,exposure,age))||
    !exposure %in% vars)stop("Distinct named outcome/exposure/age and explicit exposure term required.")
  generated<-!is.null(age_spec)&&any(age_spec$columns %in% vars)
  if(generated&&!all(age_spec$columns %in% vars))stop("Partial generated age basis is not supported.")
  if(!age %in% vars&&!generated)stop("Age must appear explicitly or through the complete locked age basis.")
  columns<-unique(c(setdiff(vars,if(generated)age_spec$columns else character()),age))
  # Do not retain a caller environment containing data or stale model matrices.
  env<-new.env(parent=baseenv());env$ns<-splines::ns;lockEnvironment(env,bindings=TRUE)
  formula<-formula;environment(formula)<-env
  list(formula=formula,columns=columns,generated_age_basis=generated,age_spec=age_spec,
    bounded_variables=bounds,formula_signature=paste(deparse(formula,width.cutoff=500L),collapse=" "))
}

streaming_raw_chunk <- function(data,rows,spec,prediction=FALSE,age_value=NULL,exposure_value=NULL) {
  columns<-if(prediction)setdiff(spec$columns,c(spec$outcome,spec$exposure)) else spec$columns
  d<-as.data.frame(setNames(lapply(columns,function(f)data[[f]][rows]),columns),stringsAsFactors=FALSE)
  if(prediction) {d[[spec$age]]<-rep(age_value,nrow(d));d[[spec$exposure]]<-rep(exposure_value,nrow(d))}
  for(f in names(spec$bounded_variables)) {
    b<-spec$bounded_variables[[f]]
    if(!is.numeric(d[[f]])||any(!is.finite(d[[f]]))||any(d[[f]]<b[1]|d[[f]]>b[2]))
      stop("Values outside locked spline boundaries: ",f)
  }
  if(!is.null(spec$age_spec))fixed_age_basis(d[[spec$age]],spec$age_spec)
  if(isTRUE(spec$generated_age_basis))d[spec$age_spec$columns]<-fixed_age_basis(d[[spec$age]],spec$age_spec)
  d
}

streaming_design <- function(data,rows,spec,prediction=FALSE,age_value=NULL,exposure_value=NULL) {
  d<-streaming_raw_chunk(data,rows,spec,prediction,age_value,exposure_value)
  tt<-if(prediction)stats::delete.response(spec$terms) else spec$terms
  mf<-stats::model.frame(tt,data=d,na.action=stats::na.fail,xlev=spec$xlevels,drop.unused.levels=FALSE)
  X<-stats::model.matrix(tt,mf,contrasts.arg=spec$contrasts)
  if(!identical(colnames(X),spec$coefficient_names)||any(!is.finite(X)))stop("Prediction/training design changed or is nonfinite.")
  list(X=X,y=if(prediction)NULL else stats::model.response(mf))
}

streaming_information_inverse <- function(H,rank_tol) {
  if(any(!is.finite(H))||any(diag(H)<=0))stop("Nonfinite or rank-deficient information matrix.")
  scale<-sqrt(diag(H));R<-H/outer(scale,scale);R<-(R+t(R))/2
  reciprocal_condition<-rcond(R)
  if(!is.finite(reciprocal_condition)||reciprocal_condition<rank_tol)
    stop("Rank-deficient or ill-conditioned scaled information; no ridge/pseudoinverse fallback.")
  ch<-tryCatch(chol(R),error=function(e)stop("Rank-deficient information: ",conditionMessage(e)))
  inverse<-chol2inv(ch)/outer(scale,scale)
  dimnames(inverse)<-dimnames(H)
  list(inverse=inverse,reciprocal_condition=reciprocal_condition)
}

streaming_logistic_pass <- function(data,spec,beta,chunk_size,with_meat=FALSE) {
  p<-length(beta);H<-meat<-matrix(0,p,p,dimnames=list(names(beta),names(beta)))
  score<-setNames(numeric(p),names(beta));deviance<-0;min_mu<-1;max_mu<-0;largest_matrix_rows<-0L
  for(first in seq.int(1L,nrow(data),by=chunk_size)) {
    rows<-seq.int(first,min(nrow(data),first+chunk_size-1L))
    z<-streaming_design(data,rows,spec);X<-z$X;y<-z$y
    eta<-drop(X%*%beta);if(any(!is.finite(eta)))stop("Nonfinite logistic linear predictor.")
    mu<-stats::plogis(eta);w<-mu*(1-mu);residual<-y-mu
    # Stable binary deviance without clipping eta, mu, or residuals.
    deviance<-deviance+2*sum(pmax(eta,0)+log1p(exp(-abs(eta)))-y*eta)
    score<-score+drop(crossprod(X,residual));H<-H+crossprod(X,X*w)
    if(with_meat)meat<-meat+crossprod(X,X*residual^2)
    min_mu<-min(min_mu,min(mu));max_mu<-max(max_mu,max(mu));largest_matrix_rows<-max(largest_matrix_rows,nrow(X))
  }
  if(!is.finite(deviance)||any(!is.finite(score))||any(!is.finite(H)))stop("Nonfinite accumulated logistic quantities.")
  list(H=H,score=score,deviance=deviance,meat=if(with_meat)meat else NULL,
    fitted_probability_range=c(min_mu,max_mu),largest_matrix_rows=largest_matrix_rows)
}

fit_streaming_logit <- function(data,formula,outcome,exposure,age,age_spec=NULL,
  covariance=c("model","HC0"),chunk_size=25000L,epsilon=1e-12,maxit=100L,rank_tol=1e-12,boundary=1e-10) {
  covariance<-match.arg(covariance);chunk_size<-streaming_check_chunk(chunk_size)
  for(z in list(epsilon=epsilon,rank_tol=rank_tol,boundary=boundary))
    if(!is.numeric(z)||length(z)!=1L||!is.finite(z)||z<=0||z>=.5)stop("Invalid numeric convergence/rank/boundary control.")
  if(length(maxit)!=1L||!is.numeric(maxit)||!is.finite(maxit)||maxit<1||maxit!=floor(maxit)||maxit>1000)
    stop("Invalid maximum iteration count.")
  spec<-streaming_formula_spec(formula,outcome,exposure,age,age_spec)
  spec$outcome<-outcome;spec$exposure<-exposure;spec$age<-age
  streaming_check_columns(data,spec$columns)
  if(spec$generated_age_basis&&any(age_spec$columns %in% names(data)))stop("Reserved generated age-basis columns collide with input data.")
  for(f in c(outcome,exposure))if(!is.numeric(data[[f]])||!all(data[[f]] %in% 0:1)||length(unique(data[[f]]))!=2L)
    stop("Outcome and exposure must each contain both numeric binary states.")
  seed<-streaming_raw_chunk(data,seq_len(min(nrow(data),chunk_size)),spec)
  mf<-stats::model.frame(spec$formula,data=seed,na.action=stats::na.fail,drop.unused.levels=FALSE)
  tt<-attr(mf,"terms")
  if(length(attr(tt,"offset")))stop("Offset terms are not supported.")
  X<-stats::model.matrix(tt,mf)
  spec$terms<-tt;spec$xlevels<-lapply(mf[vapply(mf,is.factor,logical(1))],levels)
  spec$contrasts<-attr(X,"contrasts");spec$coefficient_names<-colnames(X)
  if(!ncol(X)||nrow(data)<=ncol(X)||any(!is.finite(X)))stop("Invalid model dimension/design.")
  p<-ncol(X);beta<-setNames(numeric(p),colnames(X))
  if("(Intercept)" %in% names(beta))beta["(Intercept)"]<-stats::qlogis(mean(data[[outcome]]))
  rm(X,mf,seed)
  state<-streaming_logistic_pass(data,spec,beta,chunk_size);trace<-list();converged<-FALSE
  for(iteration in seq_len(as.integer(maxit))) {
    inversion<-streaming_information_inverse(state$H,rank_tol)
    step<-drop(inversion$inverse%*%state$score);names(step)<-names(beta)
    if(any(!is.finite(step)))stop("Nonfinite IRLS step.")
    fraction<-1;halvings<-0L
    repeat {
      candidate_beta<-beta+fraction*step
      candidate<-streaming_logistic_pass(data,spec,candidate_beta,chunk_size)
      if(candidate$deviance<=state$deviance+1e-12*(1+abs(state$deviance)))break
      halvings<-halvings+1L;fraction<-fraction/2
      if(halvings>25L)stop("IRLS step-halving failed; no fallback estimator.")
    }
    change<-abs(candidate$deviance-state$deviance)/(0.1+abs(candidate$deviance))
    step_relative<-max(abs(candidate_beta-beta))/(1+max(abs(candidate_beta)))
    score_per_record<-max(abs(candidate$score))/nrow(data)
    trace[[iteration]]<-data.frame(iteration=iteration,deviance=candidate$deviance,
      relative_deviance_change=change,relative_coefficient_step=step_relative,
      max_score_per_record=score_per_record,step_halvings=halvings,
      reciprocal_scaled_condition=inversion$reciprocal_condition)
    beta<-candidate_beta;state<-candidate
    if(change<epsilon&&score_per_record<epsilon&&step_relative<sqrt(epsilon)) {converged<-TRUE;break}
  }
  if(!converged)stop("Logistic IRLS did not converge under all explicit tolerances.")
  final<-streaming_logistic_pass(data,spec,beta,chunk_size,with_meat=TRUE)
  if(final$fitted_probability_range[1]<boundary||final$fitted_probability_range[2]>1-boundary)
    stop("Near-boundary fitted probabilities require separation diagnostics; no clipping.")
  inverse<-streaming_information_inverse(final$H,rank_tol)
  model<-inverse$inverse;hc0<-model%*%final$meat%*%model;hc0<-(hc0+t(hc0))/2
  sep_validate_parameters(beta,model);sep_validate_parameters(beta,hc0)
  structure(list(beta=beta,vcov=if(covariance=="model")model else hc0,vcov_model=model,vcov_HC0=hc0,
    covariance_type=covariance,formula=spec$formula,terms=spec$terms,xlevels=spec$xlevels,
    contrasts=spec$contrasts,design_spec=spec,outcome=outcome,exposure=exposure,age=age,age_spec=age_spec,
    covariates=setdiff(spec$columns,c(outcome,exposure,age)),nobs=nrow(data),rank=p,
    converged=TRUE,iterations=iteration,deviance=final$deviance,log_likelihood=-final$deviance/2,
    trace=do.call(rbind,trace),final_score=final$score,
    fitted_probability_range=final$fitted_probability_range,
    reciprocal_scaled_condition=inverse$reciprocal_condition,chunk_size=chunk_size,
    largest_design_matrix_rows=final$largest_matrix_rows,retained_full_design=FALSE,
    weights="unit record weights; no prior, survey, frequency, linkage or target weights",
    uncertainty_target="coefficient uncertainty conditional on empirical target covariates"),class="sep_streaming_logit_fit")
}

fit_streaming_standardized_logit <- function(data,outcome,exposure,age,covariates=character(),age_spec,
  covariance=c("model","HC0"),chunk_size=25000L,...) {
  if(anyDuplicated(c(outcome,exposure,age,covariates))||any(make.names(covariates)!=covariates))stop("Distinct syntactic covariate names required.")
  rhs<-c(paste0(exposure," * (",paste(age_spec$columns,collapse=" + "),")"),covariates)
  fit_streaming_logit(data,stats::reformulate(rhs,response=outcome),outcome,exposure,age,age_spec,
    covariance,chunk_size,...)
}

standardize_streaming_logit <- function(object,target_data,ages,target,level=.95,chunk_size=object$chunk_size) {
  if(!inherits(object,"sep_streaming_logit_fit"))stop("A streaming logistic fit is required.")
  sep_validate_parameters(object$beta,object$vcov)
  target<-match.arg(target,c("age_specific_empirical","common_empirical"));chunk_size<-streaming_check_chunk(chunk_size)
  sep_numeric(ages,"ages")
  if(is.unsorted(ages,strictly=TRUE))stop("Ages must be unique and increasing.")
  if(!is.numeric(level)||length(level)!=1L||!is.finite(level)||level<=0||level>=1)stop("Invalid confidence level.")
  spec<-object$design_spec
  streaming_check_columns(target_data,setdiff(spec$columns,c(object$outcome,object$exposure)))
  if(!is.null(object$age_spec))fixed_age_basis(ages,object$age_spec)
  tab<-gradients<-values<-metadata<-vector("list",length(ages));z<-stats::qnorm((1+level)/2)
  maximum_rows<-0L
  for(i in seq_along(ages)) {
    rows<-if(target=="age_specific_empirical")which(target_data[[object$age]]==ages[i]) else seq_len(nrow(target_data))
    n<-length(rows);if(!n)stop("No empirical target rows at age ",ages[i],"; no fallback/interpolation.")
    risk_sums<-c(0,0);G<-matrix(0,2,length(object$beta),dimnames=list(c("risk0","risk1"),names(object$beta)))
    for(first in seq.int(1L,n,by=chunk_size)) {
      at<-rows[seq.int(first,min(n,first+chunk_size-1L))]
      for(a in 0:1) {
        X<-streaming_design(target_data,at,spec,TRUE,ages[i],a)$X
        p<-stats::plogis(drop(X%*%object$beta))
        if(any(!is.finite(p)))stop("Nonfinite target predictions.")
        risk_sums[a+1L]<-risk_sums[a+1L]+sum(p)
        G[a+1L,]<-G[a+1L,]+colSums(X*(p*(1-p)))
        maximum_rows<-max(maximum_rows,nrow(X))
      }
    }
    r<-risk_sums/n;G<-G/n
    if(any(r<=0|r>=1))stop("Boundary standardized risk; no clipping.")
    log_rr<-log(r[2])-log(r[1]);lr_gradient<-G[2,]/r[2]-G[1,]/r[1]
    gradient<-rbind(risk0=G[1,],risk1=G[2,],rd=G[2,]-G[1,],log_rr=lr_gradient)
    estimate<-c(risk0=r[1],risk1=r[2],rd=r[2]-r[1],log_rr=log_rr)
    variance<-gradient%*%object$vcov%*%t(gradient)
    if(any(diag(variance)< -1e-12))stop("Negative target delta variance.")
    se<-sqrt(pmax(diag(variance),0))
    ci<-function(p,s)stats::plogis(stats::qlogis(p)+c(-1,1)*z*s/(p*(1-p)))
    ci0<-ci(r[1],se[1]);ci1<-ci(r[2],se[2])
    tab[[i]]<-data.frame(age=ages[i],target_n=n,risk0=r[1],risk1=r[2],rd=estimate["rd"],rr=exp(log_rr),
      se_risk0=se[1],se_risk1=se[2],se_rd=se[3],se_log_rr=se[4],risk0_lower=ci0[1],risk0_upper=ci0[2],
      risk1_lower=ci1[1],risk1_upper=ci1[2],rd_lower=estimate["rd"]-z*se[3],rd_upper=estimate["rd"]+z*se[3],
      rr_lower=exp(log_rr-z*se[4]),rr_upper=exp(log_rr+z*se[4]),row.names=NULL)
    labels<-paste0(names(estimate),"@age=",format(ages[i],scientific=FALSE,trim=TRUE,digits=15))
    names(estimate)<-rownames(gradient)<-labels
    values[[i]]<-estimate;gradients[[i]]<-gradient
    metadata[[i]]<-data.frame(estimand=labels,age=ages[i],target_n=n,measure=c("risk0","risk1","rd","rr"),
      scale=c("identity","identity","identity","log"),row.names=NULL)
  }
  estimate<-do.call(c,values);gradient<-do.call(rbind,gradients)
  if(anyDuplicated(names(estimate)))stop("Nonunique age/estimand labels.")
  covariance<-gradient%*%object$vcov%*%t(gradient)
  covariance<-(covariance+t(covariance))/2
  summary<-do.call(rbind,tab)
  if(any(!is.finite(as.matrix(summary))))stop("Nonfinite standardized estimate or interval; no clipping.")
  structure(list(summary=summary,estimate=estimate,gradient=gradient,covariance=covariance,
    metadata=do.call(rbind,metadata),object=object,target=target,level=level,
    largest_target_design_rows=maximum_rows,stored_target_designs=FALSE,
    uncertainty_target=object$uncertainty_target),class="sep_streaming_standardized_risks")
}

streaming_standardized_mi_components <- function(standardized,target_reference_id) {
  if(!inherits(standardized,"sep_streaming_standardized_risks"))stop("Streaming standardized risks required.")
  if(!is.character(target_reference_id)||length(target_reference_id)!=1L||is.na(target_reference_id)||!nzchar(target_reference_id))
    stop("Explicit nonempty fixed-membership target reference ID required.")
  object<-standardized$object;tab<-standardized$summary
  if(!exists("mi_validate_covariance",mode="function"))stop("Source R/11_mi_pooling.R first.")
  mi_validate_covariance(standardized$covariance,names(standardized$estimate),"Streaming standardized joint vector")
  list(estimate=standardized$estimate,covariance=standardized$covariance,gradient=standardized$gradient,
    metadata=standardized$metadata,contract=list(target_reference_id=target_reference_id,target=standardized$target,
      ages=tab$age,target_n=tab$target_n,age_spec=object$age_spec,outcome=object$outcome,exposure=object$exposure,
      age=object$age,covariates=object$covariates,coefficient_names=names(object$beta),
      covariance_type=object$covariance_type,fit_n=object$nobs,uncertainty_target=object$uncertainty_target,
      formula_signature=object$design_spec$formula_signature,xlevels=object$xlevels,contrasts=object$contrasts,
      estimation_engine="streaming_unweighted_binary_logit"))
}
