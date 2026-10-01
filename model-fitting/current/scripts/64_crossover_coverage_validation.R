#!/usr/bin/env Rscript
# Known-truth synthetic Bernoulli regressions; no study records or saved models.
crossover_coverage_scenarios <- function() data.frame(
  scenario=c("global_null","constant_positive","constant_negative","varying_positive","strong_reversal","moderate_reversal","weak_reversal","near_boundary_reversal"),
  h_at30=c(0,.2,-.2,.35,0,0,0,.58),slope=c(0,0,0,.01,.06,.03,.003,.04),
  fixed_null_age=c(30,NA,NA,NA,30,30,30,15.5),
  true_reversal=c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE),stringsAsFactors=FALSE)

crossover_in_set <- function(age,components,tol=1e-7) {
  if(!nrow(components))return(FALSE)
  any(age>=components$lower-tol&age<=components$upper+tol)
}

crossover_coverage_one <- function(s,replication,n=6000L,seed=640000L) {
  set.seed(seed+replication)
  d<-data.frame(age=sample(15:44,n,TRUE),Z=rnorm(n))
  d$A<-rbinom(n,1,plogis(-1+.015*(d$age-30)+.1*d$Z))
  h<-s$h_at30+s$slope*(d$age-30)
  d$Y<-rbinom(n,1,plogis(-2.1+.012*(d$age-30)+.2*d$Z+d$A*h))
  B<-splines::ns(d$age,knots=c(21,27,35),Boundary.knots=c(15,44))
  X<-cbind(1,d$Z,B,d$A,d$A*B);colnames(X)<-paste0("x",seq_len(ncol(X)))
  fit<-glm.fit(X,d$Y,family=binomial(),control=glm.control(epsilon=1e-10,maxit=60L))
  if(!isTRUE(fit$converged)||fit$rank!=ncol(X)||any(!is.finite(fit$coefficients)))stop("Synthetic fit failed convergence/rank")
  mu<-fit$fitted.values
  H<-crossprod(X,X*(mu*(1-mu)));bread<-solve(H)
  V<-bread%*%crossprod(X,X*(d$Y-mu)^2)%*%bread;V<-(V+t(V))/2
  ii<-tail(seq_len(ncol(X)),5L);theta<-fit$coefficients[ii];v<-V[ii,ii,drop=FALSE]
  a<-analyze_age_crossover(theta,v,c(21,27,35))
  null_age<-s$fixed_null_age
  # The true root is analytically defined by h_at30+slope*(a-30), not by our root solver.
  pc<-sc<-dc<-NA
  if(is.finite(null_age)) {
    pc<-crossover_in_set(null_age,a$pointwise$confidence_set)
    sc<-crossover_in_set(null_age,a$simultaneous$confidence_set)
    if(s$true_reversal&&nrow(a$roots)==1L&&isTRUE(a$roots$regular_delta))dc<-null_age>=a$roots$delta_lower&&null_age<=a$roots$delta_upper
  }
  whole_null_covered<-if(s$scenario=="global_null")any(a$simultaneous$confidence_set$lower<=15&
    a$simultaneous$confidence_set$upper>=44&a$simultaneous$confidence_set$lower_closed&a$simultaneous$confidence_set$upper_closed)else NA
  # Independent direct evaluation tests whether inversion membership agrees with
  # the scalar inequality at all integer ages and the analytically known null.
  ages<-sort(unique(c(15:44,if(is.finite(null_age))null_age)))
  ZZ<-cbind(1,splines::ns(ages,knots=c(21,27,35),Boundary.knots=c(15,44)))
  hh<-drop(ZZ%*%theta);vv<-rowSums((ZZ%*%v)*ZZ)
  for(pair in list(list(name="pointwise",c=qnorm(.975)),list(name="simultaneous",c=sqrt(qchisq(.95,5))))) {
    direct<-hh^2<=pair$c^2*vv
    returned<-vapply(ages,crossover_in_set,TRUE,components=a[[pair$name]]$confidence_set)
    if(!identical(direct,returned))stop("Synthetic direct inequality and returned confidence-set membership disagree")
  }
  data.frame(scenario=s$scenario,replication=replication,n=n,status="complete",error="",events=sum(d$Y),
    fixed_null_age=null_age,true_reversal=s$true_reversal,estimated_roots=nrow(a$roots),
    pointwise_fixed_root_covered=pc,simultaneous_fixed_root_covered=sc,local_delta_covered_when_available=dc,
    true_unique_regular_root=s$true_reversal,global_null_whole_domain_covered=whole_null_covered,
    local_delta_available=s$true_reversal&&nrow(a$roots)==1L&&isTRUE(a$roots$regular_delta),
    simultaneous_reversal_detected=a$simultaneous$both_signs_demonstrated,
    omnibus_rejected=a$omnibus$p_value<.05,stringsAsFactors=FALSE)
}

crossover_coverage_main <- function(args) {
  if(any(!grepl("^--(execute$|run-id=[A-Za-z0-9_-]+$|replications=[0-9]+$)",args))||anyDuplicated(sub("=.*","",args)))stop("Invalid or duplicate options")
  get<-function(key,default){a<-args[startsWith(args,paste0("--",key,"="))];if(length(a))sub(paste0("^--",key,"="),"",a)else default}
  reps<-as.integer(get("replications","1000"));if(is.na(reps)||!reps%in%c(2L,10L,100L,1000L))stop("Declared replication counts only:2,10,100,1000")
  execute<-"--execute"%in%args;id<-get("run-id","");if(execute&&!nzchar(id))stop("Fresh run ID required")
  scenarios<-crossover_coverage_scenarios()
  plan<-list(status="plan_only_known_truth_crossover_validation",scenarios=scenarios,n_per_replication=6000L,
    replications_per_scenario=reps,total_replications=reps*nrow(scenarios),seed_base=640000L,
    method="Independent base glm.fit Bernoulli simulations with explicit HC0 sandwich, fixed ns(age) interactions and analytically known linear h(a)",
    coverage="Pointwise inversion coverage of each fixed null age; simultaneous outer-set inclusion at that null; false qualitative-reversal detection for no-reversal DGPS",
    caveats="Not MI coverage or systematic-bias correction. Pointwise inversion is not all-root joint coverage. Global null uses fixed null age30 for pointwise testing and also checks simultaneous outer-set coverage of the entire closed domain. The other linear DGPs have at most one root; isolated multiple/tangent-root numerical tests are separate, not a multiroot repeated-sampling coverage claim",
    delta_caveat="Report availability and conditional coverage separately; do not hide no/multiple-root replications",
    failure_policy="Keep all attempted replications, failed statuses and warnings; any failure prevents clean release; no successful-only denominator claims",
    clinical_records=FALSE,maximum_elapsed_seconds=1800L)
  if(!execute){cat(jsonlite::toJSON(plan,pretty=TRUE,auto_unbox=TRUE),"\n");return(invisible(plan))}
  source("R/26_age_crossover.R")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  code<-c("scripts/64_crossover_coverage_validation.R","tests/test_crossover_coverage_validation.R","R/26_age_crossover.R","tests/test_age_crossover.R")
  pins<-as.list(vapply(code,sha,""));out<-file.path("outputs/qa/crossover_coverage",id)
  if(file.exists(out))stop("No overwrite or implicit restart")
  dir.create(out,recursive=TRUE)
  writej<-function(x,p)jsonlite::write_json(x,p,pretty=TRUE,auto_unbox=TRUE,digits=16,na="null",null="null")
  writej(plan,file.path(out,"plan.json"));results<-list();warnings<-list();started<-proc.time()[["elapsed"]];stage<-"start"
  tryCatch({
    setTimeLimit(cpu=Inf,elapsed=1800,transient=TRUE)
    for(k in seq_len(nrow(scenarios))) {
      s<-scenarios[k,,drop=FALSE]
      for(r in seq_len(reps)) {
        if(proc.time()[["elapsed"]]-started>1800)stop("Declared overall elapsed limit exceeded")
        stage<-paste(s$scenario,r);ww<-character()
        result<-tryCatch(withCallingHandlers(crossover_coverage_one(s,r,6000L,640000L+10000L*k),
          warning=function(w){ww<<-c(ww,conditionMessage(w));invokeRestart("muffleWarning")}),error=identity)
        if(inherits(result,"error")) {
          if(grepl("elapsed time limit",conditionMessage(result),fixed=TRUE))stop(result)
          result<-data.frame(scenario=s$scenario,replication=r,n=6000L,status="failed",error=conditionMessage(result),events=NA_integer_,
            fixed_null_age=s$fixed_null_age,true_reversal=s$true_reversal,estimated_roots=NA_integer_,
            pointwise_fixed_root_covered=NA,simultaneous_fixed_root_covered=NA,local_delta_covered_when_available=NA,
            true_unique_regular_root=s$true_reversal,global_null_whole_domain_covered=NA,
            local_delta_available=FALSE,simultaneous_reversal_detected=NA,omnibus_rejected=NA)
        }
        result$warning_count<-length(ww);results[[length(results)+1L]]<-result
        if(length(ww))warnings[[stage]]<-ww
        if(r%%10L==0L||r==reps)write.csv(do.call(rbind,results),file.path(out,"replications_checkpoint.csv"),row.names=FALSE,na="")
      }
      message("Crossover synthetic scenario",s$scenario,"finished",reps,"attempts")
    }
    setTimeLimit(cpu=Inf,elapsed=Inf,transient=FALSE)
    x<-do.call(rbind,results);write.csv(x,file.path(out,"replications.csv"),row.names=FALSE,na="")
    writej(warnings,file.path(out,"warnings.json"))
    for(p in names(pins))if(sha(p)!=pins[[p]])stop("Code changed during simulation")
    paths<-file.path(out,c("plan.json","replications.csv","replications_checkpoint.csv","warnings.json"))
    writej(list(status="complete_requested_known_truth_crossover_simulations",attempts=nrow(x),failed=sum(x$status!="complete"),warnings=sum(x$warning_count),
      elapsed_seconds=proc.time()[["elapsed"]]-started,code_sha256=pins,
      outputs=data.frame(path=paths,bytes=file.info(paths)$size,sha256=vapply(paths,sha,"")),
      not_final_MI_validation=TRUE),file.path(out,"completion.json"))
  },error=function(e){writej(list(status="incomplete_error",stage=stage,error=conditionMessage(e),attempts_completed=length(results)),file.path(out,"failure.json"));stop(e)},
    finally=setTimeLimit(cpu=Inf,elapsed=Inf,transient=FALSE))
}
if(sys.nframe()==0L)crossover_coverage_main(commandArgs(TRUE))
