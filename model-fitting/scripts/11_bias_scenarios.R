#!/usr/bin/env Rscript
# Aggregate-only fixed-assumption diagnostics, not an identified correction.
args<-commandArgs(TRUE)
if(any(!grepl("^--(execute$|run-id=[A-Za-z0-9_-]+$)",args))||anyDuplicated(sub("=.*","",args)))stop("Invalid options")
id<-sub("^--run-id=","",args[startsWith(args,"--run-id=")])
source("R/15_bias_helpers.R");source("R/16_primary_bias_sensitivity.R")
sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
base<-"outputs/models/primary_main";rp<-file.path(base,"receipt.json");r<-jsonlite::fromJSON(rp)
if(r$status!="complete_validated_age15_45_fit")stop("Completed age-45 refit required")
files<-file.path(base,c("HC0_age_standardized.csv","HC0_overall_standardized.csv"))
for(p in files){e<-r$outputs[basename(r$outputs$path)==basename(p),];if(nrow(e)!=1L||sha(p)!=e$sha256)stop("Aggregate result hash mismatch")}
ep<-"config/outcome_validation_candidates.json";evidence<-jsonlite::fromJSON(ep,simplifyVector=FALSE)
anchors<-primary_validation_anchors(evidence)
code<-c("scripts/11_bias_scenarios.R","R/15_bias_helpers.R","R/16_primary_bias_sensitivity.R","tests/test_primary_bias_sensitivity.R")
pins<-as.list(vapply(c(code,files,rp,ep),sha,""))
plan<-list(status="plan_only_no_bias_calculation",source_result="Age-15-45 full-main complete-case refit",
  target="Within women with recorded smoking before pregnancy, current eligible CC empirical reference;32targets: overall and ages15-45",
  anchors=anchors,imputed_or_causal_result=FALSE,code_and_source_sha256=pins,
  outcome_error="Each validation sample separately transported as a fixed assumption; constant Se/Sp in each exposure arm across C; recorded exposure treated as true for this scenario only",
  differential_sensitivity_multipliers=c(.5,.75,1,1.25,1.5),differential_false_positive_multipliers=c(.5,1,2),
  scenario_evidence_status="Stress multipliers are illustrative assumptions, not empirical plausible ranges or priors; do not pool validation studies",
  confounding_strengths=c(1,1.25,1.5,2,3,5),confounding_scope="marginal_rr_benchmark: classic algebra, not validated marginal causal bound",
  no_intervals_for_scenarios=TRUE,no_selection_or_early_loss_correction=TRUE,no_joint_bias_correction=TRUE,
  null_tipping="Assume each separate anchor for A0 and same specificity for both arms; solve the A1 sensitivity yielding equal latent risks; not an estimated error mechanism")
cat(jsonlite::toJSON(plan,pretty=TRUE,auto_unbox=TRUE,digits=15),"\n")
if(!"--execute" %in% args)quit(status=0)
if(length(id)!=1L)stop("Fresh run ID required")
out<-file.path("outputs/bias",id);if(file.exists(out))stop("No overwrite")
dir.create(out,recursive=TRUE);jsonlite::write_json(plan,file.path(out,"plan.json"),pretty=TRUE,auto_unbox=TRUE,digits=15)
tryCatch({
  a<-read.csv(files[1]);o<-read.csv(files[2]);at<-function(m,f)o[[f]][o$measure==m]
  targets<-rbind(data.frame(target="overall",age=NA_integer_,n=at("rr","target_n"),q0=at("risk0","estimate"),q1=at("risk1","estimate"),
    rr=at("rr","estimate"),rr_lower=at("rr","lower"),rr_upper=at("rr","upper")),
    data.frame(target=paste0("age_",a$age),age=a$age,n=a$target_n,q0=a$risk0,q1=a$risk1,rr=a$rr,rr_lower=a$rr_lower,rr_upper=a$rr_upper))
  stopifnot(nrow(targets)==32,all(abs(targets$q1/targets$q0-targets$rr)<1e-12))
  ev<-bounds<-scenarios<-tipping<-list();si<-bi<-ti<-0L
  for(k in seq_len(nrow(targets))) {
    t<-targets[k,];target_id<-paste0("main_CC_2016_2024_",t$target)
    v<-primary_evalue_rr(t$rr,"marginal_rr_benchmark",target_id,t$rr_lower,t$rr_upper)
    ev[[k]]<-data.frame(target=t$target,rr=t$rr,evalue_point_algebraic=v$evalue_point,
      nearest_CI_limit_threshold=v$evalue_ci_limit,exposure_reversed=v$exposure_reversed,validated_marginal_causal_bound=FALSE)
    for(au in plan$confounding_strengths)for(uy in plan$confounding_strengths) {
      b<-primary_confounding_bound(t$rr,au,uy,"marginal_rr_benchmark",target_id);bi<-bi+1L
      bounds[[bi]]<-data.frame(target=t$target,rr_au=au,rr_uy=uy,bounding_factor=b$bounding_factor,
        directional_RR_algebraic_benchmark=b$directional_rr_bound_or_benchmark,
        null_reached_by_algebra=b$null_not_excluded_by_this_bound,validated_marginal_causal_bound=FALSE)
    }
    candidate<-rbind(data.frame(id="no_error",sensitivity=1,specificity=1),anchors[c("id","sensitivity","specificity")])
    for(j in seq_len(nrow(candidate))) {
      anchor<-candidate[j,]
      grid<-if(anchor$id=="no_error")data.frame(se_mult=1,fpr_mult=1)else expand.grid(se_mult=plan$differential_sensitivity_multipliers,fpr_mult=plan$differential_false_positive_multipliers)
      for(h in seq_len(nrow(grid))) {
        se<-c(A0=anchor$sensitivity,A1=anchor$sensitivity*grid$se_mult[h])
        sp<-c(A0=anchor$specificity,A1=1-(1-anchor$specificity)*grid$fpr_mult[h])
        result<-tryCatch(primary_outcome_measurement_scenario(t$q0,t$q1,se,sp,target_id,TRUE,TRUE),error=identity)
        valid<-!inherits(result,"error");si<-si+1L
        scenarios[[si]]<-data.frame(target=t$target,anchor=anchor$id,sensitivity_A0=se[1],sensitivity_A1=se[2],
          specificity_A0=sp[1],specificity_A1=sp[2],sensitivity_multiplier=grid$se_mult[h],false_positive_multiplier=grid$fpr_mult[h],
          valid=valid,incompatibility=if(valid)""else conditionMessage(result),
          risk0=if(valid)result$risk[1]else NA_real_,risk1=if(valid)result$risk[2]else NA_real_,
          RR=if(valid)result$rr else NA_real_,RD_per1000=if(valid)1000*result$rd else NA_real_,
          status="Fixed-assumption diagnostic; no sampling or scenario uncertainty interval")
      }
      if(anchor$id!="no_error") {
        fpr<-1-anchor$specificity;p0<-correct_outcome_risk(t$q0,anchor$sensitivity,anchor$specificity)
        required<-if(p0>0)fpr+(t$q1-fpr)/p0 else NA_real_
        valid<-is.finite(required)&&required>=0&&required<=1&&required+anchor$specificity>1
        if(valid) {
          chk<-primary_outcome_measurement_scenario(t$q0,t$q1,c(A0=anchor$sensitivity,A1=required),
            c(A0=anchor$specificity,A1=anchor$specificity),target_id,TRUE,TRUE)
          if(abs(chk$rd)>1e-12)stop("Outcome sensitivity tipping check failed")
        }
        ti<-ti+1L;tipping[[ti]]<-data.frame(target=t$target,anchor=anchor$id,valid=valid,
          assumed_sensitivity_A0=anchor$sensitivity,assumed_common_specificity=anchor$specificity,
          required_sensitivity_A1=required,required_A1_to_A0_sensitivity_ratio=required/anchor$sensitivity,
          equal_risk_under_assumptions=p0,evidence_status="Illustrative exact tipping condition; no empirical smoking-stratified validation")
      }
    }
  }
  tables<-list(targets=targets,validation_anchors=anchors,Evalue_algebraic_NOT_validated_marginal_bound=do.call(rbind,ev),
    confounding_algebraic_benchmarks=do.call(rbind,bounds),outcome_measurement_scenarios=do.call(rbind,scenarios),
    outcome_sensitivity_null_tipping=do.call(rbind,tipping))
  for(n in names(tables))write.csv(tables[[n]],file.path(out,paste0(n,".csv")),row.names=FALSE)
  if(!identical(pins,as.list(vapply(names(pins),sha,""))))stop("Bias calculation source/code drift")
  paths<-list.files(out,full.names=TRUE)
  jsonlite::write_json(list(status="complete_fixed_assumption_diagnostics_not_causal_or_final",targets=nrow(targets),
    outcome_scenarios=nrow(tables$outcome_measurement_scenarios),invalid_scenarios=sum(!tables$outcome_measurement_scenarios$valid),
    no_priors_no_joint_bias_no_early_loss_correction=TRUE,source_hashes=pins,
    outputs=data.frame(path=paths,bytes=file.info(paths)$size,sha256=vapply(paths,sha,""))),
    file.path(out,"receipt.json"),pretty=TRUE,auto_unbox=TRUE,digits=15)
},error=function(e){jsonlite::write_json(list(status="incomplete_error",error=conditionMessage(e)),file.path(out,"failure.json"),auto_unbox=TRUE,pretty=TRUE);stop(e)})
