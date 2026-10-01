# Conditional-on-reference paired live-only versus fetal-inclusive comparison.
# Source R04, R06, R11 first. No source reads, imputation or national execution.

paired_sha <- function(x)digest::digest(serialize(x,NULL,version=3),algo="sha256",serialize=FALSE)

paired_check_indices <- function(indices,n,label) {
  if(!is.numeric(indices)||!length(indices)||anyNA(indices)||any(!is.finite(indices))||
    any(indices!=floor(indices))||any(indices<1|indices>n)||is.unsorted(indices,strictly=TRUE))
    stop(label," requires unique increasing in-range row indices.")
  as.integer(indices)
}

paired_check_identity <- function(data,row_ids) {
  if(!is.data.frame(data)||!nrow(data))stop("Nonempty completed combined data required.")
  if(!is.character(row_ids)||length(row_ids)!=nrow(data)||anyNA(row_ids)||
    any(!nzchar(row_ids))||anyDuplicated(row_ids))stop("Explicit unique nonmissing source-qualified row IDs required.")
  invisible(TRUE)
}

paired_input_signature <- function(data,row_ids,indices,spec) {
  paired_check_identity(data,row_ids)
  indices<-paired_check_indices(indices,nrow(data),"Signature")
  streaming_check_columns(data,spec$columns)
  schema<-lapply(spec$columns,function(f)list(field=f,class=class(data[[f]]),
    levels=if(is.factor(data[[f]]))levels(data[[f]]) else NULL))
  # Fixed fingerprint chunks, independent of numerical model chunk size.
  state<-paired_sha(list(version="paired_input_signature_v1",serialization=3L,
    formula=spec$formula_signature,columns=schema,n=length(indices)))
  id_state<-paired_sha(list(version="ordered_source_ids_v1",n=length(indices)))
  for(first in seq.int(1L,length(indices),by=10000L)) {
    at<-indices[seq.int(first,min(length(indices),first+9999L))]
    values<-setNames(lapply(spec$columns,function(f)data[[f]][at]),spec$columns)
    state<-paired_sha(list(previous=state,source_ids=row_ids[at],model_values=values))
    id_state<-paired_sha(list(previous=id_state,source_ids=row_ids[at]))
  }
  list(version="paired_input_signature_v1",n=length(indices),model_values_and_ids_sha256=state,
    ordered_ids_sha256=id_state,formula_signature=spec$formula_signature,
    columns=schema,fingerprint_chunk_size=10000L,serialization_version=3L)
}

paired_fit_parameter_hash <- function(fit) {
  paired_sha(list(beta=fit$beta,selected_vcov=fit$vcov,vcov_model=fit$vcov_model,vcov_HC0=fit$vcov_HC0,
    nobs=fit$nobs,formula=fit$design_spec$formula_signature,xlevels=fit$xlevels,
    contrasts=fit$contrasts,outcome=fit$outcome,exposure=fit$exposure,age=fit$age,
    age_spec=fit$age_spec,covariance_type=fit$covariance_type))
}

fit_paired_source_models <- function(combined_data,live_indices,row_ids,formula,
  outcome,exposure,age,age_spec=NULL,chunk_size=25000L,...) {
  paired_check_identity(combined_data,row_ids)
  live_indices<-paired_check_indices(live_indices,nrow(combined_data),"Live subset")
  spec<-streaming_formula_spec(formula,outcome,exposure,age,age_spec)
  before_live<-paired_input_signature(combined_data,row_ids,live_indices,spec)
  before_inclusive<-paired_input_signature(combined_data,row_ids,seq_len(nrow(combined_data)),spec)
  # Only compact model columns are copied for the smaller fit. No raw-record
  # frame, source-ID vector or completed data is retained in the returned fits.
  live<-as.data.frame(setNames(lapply(spec$columns,function(f)combined_data[[f]][live_indices]),spec$columns))
  fit_live<-fit_streaming_logit(live,formula,outcome,exposure,age,age_spec,"HC0",chunk_size,...)
  rm(live)
  fit_inclusive<-fit_streaming_logit(combined_data,formula,outcome,exposure,age,age_spec,"HC0",chunk_size,...)
  if(!identical(before_live,paired_input_signature(combined_data,row_ids,live_indices,spec))||
    !identical(before_inclusive,paired_input_signature(combined_data,row_ids,seq_len(nrow(combined_data)),spec)))
    stop("Model values/source identities changed during paired fitting.")
  fit_live$paired_source_provenance<-list(role="live",input=before_live,
    fit_parameter_sha256=paired_fit_parameter_hash(fit_live),created_by="fit_paired_source_models")
  fit_inclusive$paired_source_provenance<-list(role="inclusive",input=before_inclusive,
    fit_parameter_sha256=paired_fit_parameter_hash(fit_inclusive),created_by="fit_paired_source_models")
  list(live=fit_live,inclusive=fit_inclusive,clinical_model_ready=FALSE,
    interpretation="Completed-model-input engineering; source labels and eligibility require upstream source receipts.")
}

paired_indexed_pass <- function(data,indices,fit,chunk_size) {
  p<-length(fit$beta);H<-meat<-matrix(0,p,p,dimnames=list(names(fit$beta),names(fit$beta)))
  score<-setNames(numeric(p),names(fit$beta));deviance<-0
  for(first in seq.int(1L,length(indices),by=chunk_size)) {
    at<-indices[seq.int(first,min(length(indices),first+chunk_size-1L))]
    z<-streaming_design(data,at,fit$design_spec);eta<-drop(z$X%*%fit$beta)
    mu<-plogis(eta);r<-z$y-mu
    H<-H+crossprod(z$X,z$X*(mu*(1-mu)));meat<-meat+crossprod(z$X,z$X*r^2)
    score<-score+drop(crossprod(z$X,r))
    deviance<-deviance+2*sum(pmax(eta,0)+log1p(exp(-abs(eta)))-z$y*eta)
  }
  bread<-streaming_information_inverse(H,1e-12)$inverse
  hc0<-bread%*%meat%*%bread;hc0<-(hc0+t(hc0))/2
  list(model=bread,HC0=hc0,score=score,deviance=deviance)
}

paired_near <- function(x,y,tolerance=1e-8) {
  is.numeric(x)&&is.numeric(y)&&identical(dim(x),dim(y))&&length(x)==length(y)&&
    all(is.finite(x))&&all(is.finite(y))&&max(abs(x-y))<=tolerance*max(1,abs(x),abs(y))
}

paired_validate_fit <- function(data,row_ids,indices,fit,role,chunk_size) {
  if(!inherits(fit,"sep_streaming_logit_fit")||fit$covariance_type!="HC0"||!isTRUE(fit$converged))
    stop("Both paired fits must be converged R11 fits with HC0 selected.")
  if(!identical(fit$vcov,fit$vcov_HC0))stop("Selected fit covariance must equal its HC0 covariance.")
  provenance<-fit$paired_source_provenance
  if(is.null(provenance)||!identical(provenance$role,role)||
    !identical(provenance$created_by,"fit_paired_source_models"))
    stop("Fit lacks a pre-fit checked paired source/input provenance contract; bare fits cannot prove source identity.")
  if(!identical(provenance$fit_parameter_sha256,paired_fit_parameter_hash(fit)))stop("Paired fitted parameters/contract changed.")
  actual<-paired_input_signature(data,row_ids,indices,fit$design_spec)
  if(!identical(provenance$input,actual)||fit$nobs!=length(indices))
    stop("Paired observed outcomes/exposures/covariates/source identities differ from fitted input.")
  recheck<-paired_indexed_pass(data,indices,fit,chunk_size)
  if(!paired_near(recheck$model,fit$vcov_model)||!paired_near(recheck$HC0,fit$vcov_HC0)||
    !paired_near(recheck$score,fit$final_score)||!paired_near(recheck$deviance,fit$deviance))
    stop("Supplied fit does not reconcile with indexed completed-data score/information/deviance.")
  list(role=role,signature=actual,score_information_deviance_rechecked=TRUE)
}

paired_validate_covariance <- function(V,label,tolerance=1e-10) {
  if(!is.matrix(V)||!is.numeric(V)||nrow(V)!=ncol(V)||any(!is.finite(V))||
    is.null(rownames(V))||!identical(rownames(V),colnames(V)))stop(label,": invalid covariance shape/names.")
  scale<-max(abs(V),.Machine$double.xmin)
  if(max(abs(V-t(V)))>tolerance*scale||min(diag(V))< -tolerance*scale||
    min(eigen((V+t(V))/2,symmetric=TRUE,only.values=TRUE)$values)< -tolerance*scale)
    stop(label,": covariance is substantively asymmetric or not PSD; no repair.")
  invisible(TRUE)
}

paired_source_comparison <- function(combined_data,live_indices,row_ids,fit_live,fit_inclusive,
  ages,target,reference_indices=live_indices,target_reference_id,chunk_size=25000L,level=.95) {
  paired_check_identity(combined_data,row_ids);chunk_size<-streaming_check_chunk(chunk_size)
  live_indices<-paired_check_indices(live_indices,nrow(combined_data),"Live subset")
  reference_indices<-paired_check_indices(reference_indices,nrow(combined_data),"Live reference")
  if(any(!reference_indices %in% live_indices))stop("The common reference must contain only completed live-birth rows.")
  if(!is.character(target_reference_id)||length(target_reference_id)!=1L||is.na(target_reference_id)||!nzchar(target_reference_id))
    stop("Explicit frozen live-reference ID required.")
  contracts<-c("outcome","exposure","age","age_spec","covariates","xlevels","contrasts","covariance_type")
  if(!all(vapply(contracts,function(f)identical(fit_live[[f]],fit_inclusive[[f]]),logical(1)))||
    !identical(fit_live$design_spec$formula_signature,fit_inclusive$design_spec$formula_signature)||
    !identical(names(fit_live$beta),names(fit_inclusive$beta)))stop("Paired outcome/model/basis/factor contracts must be identical.")
  audit_live<-paired_validate_fit(combined_data,row_ids,live_indices,fit_live,"live",chunk_size)
  audit_inclusive<-paired_validate_fit(combined_data,row_ids,seq_len(nrow(combined_data)),fit_inclusive,"inclusive",chunk_size)
  p<-length(fit_live$beta);cross_meat<-matrix(0,p,p,dimnames=list(names(fit_live$beta),names(fit_inclusive$beta)))
  max_rows<-0L
  for(first in seq.int(1L,length(live_indices),by=chunk_size)) {
    at<-live_indices[seq.int(first,min(length(live_indices),first+chunk_size-1L))]
    left<-streaming_design(combined_data,at,fit_live$design_spec)
    right<-streaming_design(combined_data,at,fit_inclusive$design_spec)
    if(!identical(left$y,right$y)||!identical(left$X,right$X))
      stop("Shared records have different observed outcome/covariate/exposure designs.")
    rl<-left$y-plogis(drop(left$X%*%fit_live$beta))
    rc<-right$y-plogis(drop(right$X%*%fit_inclusive$beta))
    cross_meat<-cross_meat+crossprod(left$X,right$X*(rl*rc))
    max_rows<-max(max_rows,nrow(left$X))
  }
  cross_beta<-fit_live$vcov_model%*%cross_meat%*%fit_inclusive$vcov_model
  beta_cov<-rbind(cbind(fit_live$vcov_HC0,cross_beta),cbind(t(cross_beta),fit_inclusive$vcov_HC0))
  beta_names<-c(paste0("live::",names(fit_live$beta)),paste0("inclusive::",names(fit_inclusive$beta)))
  dimnames(beta_cov)<-list(beta_names,beta_names);paired_validate_covariance(beta_cov,"Stacked coefficient")
  fields<-setdiff(fit_live$design_spec$columns,c(fit_live$outcome,fit_live$exposure))
  reference<-as.data.frame(setNames(lapply(fields,function(f)combined_data[[f]][reference_indices]),fields))
  live<-standardize_streaming_logit(fit_live,reference,ages,target,level,chunk_size)
  inclusive<-standardize_streaming_logit(fit_inclusive,reference,ages,target,level,chunk_size)
  if(!identical(live$metadata,inclusive$metadata))stop("Standardized target/age metadata drifted between paired fits.")
  ref_signature<-paired_input_signature(combined_data,row_ids,reference_indices,fit_live$design_spec)
  rm(reference)
  k<-length(live$estimate);zero<-matrix(0,k,p)
  joint_gradient<-rbind(cbind(live$gradient,zero),cbind(zero,inclusive$gradient))
  joint_names<-c(paste0("live::",names(live$estimate)),paste0("inclusive::",names(inclusive$estimate)))
  dimnames(joint_gradient)<-list(joint_names,beta_names)
  joint_estimate<-setNames(c(unname(live$estimate),unname(inclusive$estimate)),joint_names)
  joint_cov<-joint_gradient%*%beta_cov%*%t(joint_gradient);joint_cov<-(joint_cov+t(joint_cov))/2
  paired_validate_covariance(joint_cov,"Joint standardized")
  D<-cbind(-diag(k),diag(k));delta_gradient<-D%*%joint_gradient
  delta_names<-paste0("inclusive_minus_live::",names(live$estimate))
  rownames(delta_gradient)<-delta_names
  delta_estimate<-setNames(unname(inclusive$estimate-live$estimate),delta_names)
  delta_cov<-D%*%joint_cov%*%t(D);delta_cov<-(delta_cov+t(delta_cov))/2
  dimnames(delta_cov)<-list(delta_names,delta_names)
  # For near-identical estimators, validate cancellation against the undifferenced
  # variance scale; never truncate or replace a tiny negative variance with zero.
  delta_scale<-max(abs(joint_cov),.Machine$double.xmin)
  if(any(!is.finite(delta_cov))||min(diag(delta_cov))< -1e-10*delta_scale||
    min(eigen(delta_cov,symmetric=TRUE,only.values=TRUE)$values)< -1e-10*delta_scale)
    stop("Paired difference covariance is not PSD beyond floating-point tolerance.")
  degenerate<-diag(delta_cov)<=1e-12*delta_scale
  se<-rep(NA_real_,k);se[!degenerate]<-sqrt(diag(delta_cov)[!degenerate])
  delta_metadata<-live$metadata;delta_metadata$estimand<-delta_names
  delta_metadata$measure<-paste0("difference_",c("risk0","risk1","rd","log_rr"))[rep(1:4,length.out=k)]
  delta_metadata$scale<-"identity"
  delta_summary<-cbind(delta_metadata,estimate=unname(delta_estimate),variance=unname(diag(delta_cov)),
    se=se,lower=unname(delta_estimate)-qnorm((1+level)/2)*se,
    upper=unname(delta_estimate)+qnorm((1+level)/2)*se,numerically_degenerate=unname(degenerate))
  all_gradient<-rbind(joint_gradient,delta_gradient)
  T<-rbind(diag(2*k),D);all_cov<-T%*%joint_cov%*%t(T);all_cov<-(all_cov+t(all_cov))/2
  all_estimate<-c(joint_estimate,delta_estimate);dimnames(all_cov)<-list(names(all_estimate),names(all_estimate))
  metadata_live<-live$metadata;metadata_live$estimand<-joint_names[seq_len(k)];metadata_live$comparison<-"live"
  metadata_inclusive<-inclusive$metadata;metadata_inclusive$estimand<-joint_names[k+seq_len(k)];metadata_inclusive$comparison<-"inclusive"
  delta_metadata$comparison<-"inclusive_minus_live"
  structure(list(live=live,inclusive=inclusive,cross_score_meat=cross_meat,cross_beta_covariance=cross_beta,
    beta_covariance=beta_cov,joint_estimate=joint_estimate,joint_gradient=joint_gradient,joint_covariance=joint_cov,
    paired_estimate=delta_estimate,paired_gradient=delta_gradient,paired_covariance=delta_cov,paired_summary=delta_summary,
    all_estimate=all_estimate,all_gradient=all_gradient,all_covariance=all_cov,
    all_metadata=rbind(metadata_live,metadata_inclusive,delta_metadata),
    audits=list(live=audit_live,inclusive=audit_inclusive,shared_designs_identical=TRUE,reference_signature=ref_signature),
    contract=list(target_reference_id=target_reference_id,reference_ordered_ids_sha256=ref_signature$ordered_ids_sha256,
      target=live$target,ages=live$summary$age,target_n=live$summary$target_n,
      live_ordered_ids_sha256=audit_live$signature$ordered_ids_sha256,
      inclusive_ordered_ids_sha256=audit_inclusive$signature$ordered_ids_sha256,
      n_live=length(live_indices),n_inclusive=nrow(combined_data),n_reference=length(reference_indices),
      formula_signature=fit_live$design_spec$formula_signature,age_spec=fit_live$age_spec,
      xlevels=fit_live$xlevels,contrasts=fit_live$contrasts,coefficient_names=names(fit_live$beta),
      outcome=fit_live$outcome,exposure=fit_live$exposure,age=fit_live$age,covariance_type="paired_stacked_HC0",
      uncertainty_target="coefficient uncertainty conditional on the same completed live-birth empirical reference"),
    maximum_shared_design_rows=max_rows,stored_model_or_target_rows=FALSE,
    independent_fit_approximation_used=FALSE,level=level,
    interpretation="Difference between nested recorded-outcome associations on the same live-birth covariate reference. Fetal inclusion does not eliminate unobserved early-loss, ascertainment or confounding bias."),
    class="sep_paired_source_comparison")
}

paired_source_mi_components <- function(paired) {
  if(!inherits(paired,"sep_paired_source_comparison"))stop("Paired source comparison required.")
  if(any(paired$paired_summary$numerically_degenerate)||any(diag(paired$all_covariance)<=0))
    stop("Degenerate paired targets cannot use ordinary positive-variance Rubin pooling; no variance repair.")
  mi_validate_covariance(paired$all_covariance,names(paired$all_estimate),"Paired MI joint vector")
  list(estimate=paired$all_estimate,covariance=paired$all_covariance,gradient=paired$all_gradient,
    metadata=paired$all_metadata,contract=paired$contract)
}
