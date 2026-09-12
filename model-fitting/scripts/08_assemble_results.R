# Aggregate validated age-45 refits. No fitting, imputation or source mutation.
main <- function() {
  source("R/17_fit_helpers.R");age45_initialize()
  base<-"outputs/models";out<-"outputs/report"
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  old<-"reference"
  read<-function(p)read.csv(p,check.names=FALSE,stringsAsFactors=FALSE)
  put<-function(x,name)data.table::fwrite(x,file.path(out,name),na="")
  oldlabels<-read(file.path(old,"crossover_labels.csv"))
  mapping<-setNames(oldlabels$model_id,oldlabels$model_id)
  mapping[c("primary_core_same_cc","primary_augmented_same_cc","primary_core_available","primary_recent")]<-
    c("core_common_cc","augmented_common_cc","core_available_cc","main_2018_2024_cc")
  aa<-grep("^age_spec_",names(mapping));mapping[aa]<-sub("^age_spec_","",names(mapping)[aa])
  paired<-grep("^(all_years|reporting_years)_shared_",names(mapping))
  mapping[paired]<-sub("_(live|inclusive)$","/\\1",names(mapping)[paired])
  verify<-function(id) {
    rp<-file.path(base,sub("/(live|inclusive)$","",id),"receipt.json")
    if(!file.exists(rp))stop("Selected model has not completed: ",id)
    r<-jsonlite::fromJSON(rp);stopifnot(startsWith(r$status,"complete_"))
    for(i in seq_len(nrow(r$outputs)))stopifnot(age45_sha(r$outputs$path[i])==r$outputs$sha256[i])
    if(!is.null(r$code))for(i in seq_len(nrow(r$code)))
      stopifnot(age45_sha(r$code$path[i])==r$code$sha256[i])
    r
  }
  fmtset<-function(x)if(!nrow(x))"Empty"else paste0(ifelse(x$lower_closed,"[","("),
    sprintf("%.3f",x$lower),", ",sprintf("%.3f",x$upper),ifelse(x$upper_closed,"]",")"),collapse=" U ")
  roots<-summaries<-sets<-signs<-objects<-list()
  oldsummary<-read(file.path(old,"crossover_summary.csv"))
  for(id in names(mapping)) {
    path<-file.path(base,mapping[[id]]);r<-verify(mapping[[id]])
    fit<-readRDS(file.path(path,"model.rds"));stopifnot(fit$converged)
    for(cv in c("HC0","model")) {
      key<-paste(id,cv,sep="__");z<-readRDS(file.path(path,paste0(cv,"_crossover.rds")))
      stopifnot(identical(as.numeric(z$bounds),c(15,45)))
      objects[[key]]<-z
      root<-z$roots;root$model_id<-id;root$covariance<-cv;roots[[key]]<-root
      row<-oldsummary[oldsummary$model_id==id&oldsummary$covariance==cv,]
      stopifnot(nrow(row)==1)
      age<-read(file.path(path,paste0(cv,"_age_standardized.csv")))
      stopifnot(identical(age$age,15:45),all(sign(evaluate_age_crossover(z,age$age)$h)==sign(age$rd)))
      row$fit_n<-fit$nobs;row$reference_n<-sum(age$target_n)
      row$fitted_null_ages<-paste(sprintf("%.6f",z$roots$age),collapse="; ")
      row$n_isolated_roots<-nrow(z$roots);row$has_zero_intervals<-nrow(z$zero_intervals)>0
      row$unique_regular_interior_root<-z$root_summary$unique_regular_interior_root
      row$full_95_fixed_null_age_set<-fmtset(z$pointwise$confidence_set)
      row$fixed_null_age_set_components<-nrow(z$pointwise$confidence_set)
      row$full_95_simultaneous_outer_set<-fmtset(z$simultaneous$confidence_set)
      row$simultaneous_outer_set_components<-nrow(z$simultaneous$confidence_set)
      row$simultaneous_strict_negative_intervals<-fmtset(subset(z$simultaneous$sign_regions,classification=="strict_negative"))
      row$simultaneous_strict_positive_intervals<-fmtset(subset(z$simultaneous$sign_regions,classification=="strict_positive"))
      row$simultaneous_reversal<-z$simultaneous$both_signs_demonstrated
      row$omnibus_age_interaction_statistic<-z$omnibus$statistic;row$omnibus_df<-z$omnibus$df
      row$omnibus_p<-z$omnibus$p_value
      row$omnibus_log_p<-pchisq(z$omnibus$statistic,z$omnibus$df,lower.tail=FALSE,log.p=TRUE)
      row$age_knots<-paste(z$knots,collapse=",")
      row$formula<-paste(deparse(fit$formula,width.cutoff=500),collapse=" ")
      summaries[[key]]<-row
      for(m in c("pointwise","simultaneous")) {
        q<-z[[m]]$confidence_set;q$model_id<-id;q$covariance<-cv;q$method<-m
        sets[[paste(key,m)]]<-q
      }
      q<-z$simultaneous$sign_regions;q$model_id<-id;q$covariance<-cv;signs[[key]]<-q
    }
  }
  saveRDS(objects,file.path(out,"crossover_objects.rds"))
  put(oldlabels,"crossover_labels.csv");put(data.table::rbindlist(roots),"root_reference.csv")
  put(data.table::rbindlist(summaries),"crossover_summary.csv")
  put(data.table::rbindlist(sets),"crossover_full_null_age_sets.csv")
  put(data.table::rbindlist(signs),"crossover_simultaneous_sign_regions.csv")
  previous_roots<-read(file.path(old,"root_reference.csv"))
  newroots<-as.data.frame(data.table::rbindlist(roots))
  keep<-c("model_id","covariance","age","delta_lower","delta_upper")
  central_old<-subset(previous_roots,covariance=="HC0" & age>=20 & age<=40)[keep]
  central_new<-subset(newroots,covariance=="HC0" & age>=20 & age<=40)[keep]
  stopifnot(nrow(central_old)==21,nrow(central_new)==21,
    !anyDuplicated(central_old$model_id),!anyDuplicated(central_new$model_id))
  cr<-merge(central_old,central_new,by=c("model_id","covariance"),suffixes=c("_previous","_age45"))
  cr$age_change<-cr$age_age45-cr$age_previous
  put(cr,"age45_vs_previous_crossovers.csv")
  oldoverall<-read(file.path(old,"main_overall.csv"));allages<-alloverall<-list()
  for(ct in c("primary","broad","prepregnancy")) {
    id<-paste0(ct,"_main");r<-verify(id);path<-file.path(base,id)
    row<-oldoverall[oldoverall$contrast==ct,]
    row$eligible_n<-r$eligible_n;row$n<-r$fit_n;row$events<-r$events;row$A0<-r$A0;row$A1<-r$A1
    row$excluded_n<-r$eligible_n-r$fit_n
    ov<-read(file.path(path,"HC0_overall_standardized.csv"))
    for(m in c("risk0","risk1","rr","rd"))for(l in c("estimate","lower","upper")) {
      suffix<-if(l=="estimate")""else paste0("_",l)
      name<-paste0(m,suffix,if(m!="rr")"_per1000"else"")
      row[[name]]<-ov[[l]][ov$measure==m]*if(m!="rr")1000 else 1
    }
    alloverall[[ct]]<-row
    for(cv in c("HC0","model")) {
      a<-read(file.path(path,paste0(cv,"_age_standardized.csv")))
      stopifnot(sum(a$target_n)==r$fit_n,identical(a$age,15:45))
      a$contrast<-ct;a$covariance<-cv;a$model_id<-id;a$fit_n<-r$fit_n
      allages[[paste(ct,cv)]]<-a
    }
  }
  alloverall<-data.table::rbindlist(alloverall);allages<-data.table::rbindlist(allages)
  put(alloverall,"main_overall.csv");put(allages,"main_age_estimates.csv")
  interactions<-interaction_overall<-list()
  for(ct in c("primary","broad","prepregnancy"))
    for(sc in if(ct=="primary")c("original","smoking_by_bmi","smoking_and_age_by_year","combined")else c("original","combined")) {
      id<-paste(ct,if(sc=="original")"main"else sc,sep="_");r<-verify(id)
      q<-read(file.path(base,id,"HC0_age_standardized.csv"))
      q$contrast<-ct;q$model<-sc
      q$rd_grid_lower<-q$rd_bonferroni_lower_per1000/1000
      q$rd_grid_upper<-q$rd_bonferroni_upper_per1000/1000
      q$negative_grid_supported<-q$rd_grid_upper<0;q$positive_grid_supported<-q$rd_grid_lower>0
      interactions[[id]]<-q
      v<-read(file.path(base,id,"HC0_overall_standardized.csv"));v$contrast<-ct;v$model<-sc
      v$fit_n<-r$fit_n;v$reference_n<-r$reference_n;interaction_overall[[id]]<-v
    }
  ia<-data.table::rbindlist(interactions)
  put(ia,"interaction_age_all_models.csv")
  put(ia[model%in%c("original","combined")],"interaction_curves.csv")
  put(data.table::rbindlist(interaction_overall),"interaction_overall.csv")
  ep<-list()
  ids<-list.files(base,pattern="^(linked|fetal)_")
  stopifnot(length(ids)==14)
  for(id in ids) {
    r<-verify(id);family<-if(startsWith(id,"linked"))"linked"else"fetal"
    scenario<-sub("^(linked|fetal)_","",id)
    years<-if(family=="linked")if(grepl("_later$",id))2019:2023 else 2018:2023 else
      if(grepl("flag_supported",id))c(2019,2020,2021,2024)else 2018:2024
    for(cv in c("HC0","model")) {
      q<-read(file.path(base,id,paste0(cv,"_overall_standardized.csv")))
      q$family<-family;q$scenario<-scenario;q$covariance<-cv;q$years<-paste(years,collapse=",")
      q$fit_n<-r$fit_n;q$reference_n<-r$reference_n;q$events<-r$events;q$A0<-r$A0;q$A1<-r$A1
      q$source_receipt<-file.path(base,id,"receipt.json");q$source_receipt_sha256<-age45_sha(q$source_receipt[1])
      q$formula<-r$formula;q$GH_required<-FALSE;q$GH_adjusted<-FALSE
      q$interpretation<-if(family=="fetal")"Exploratory retained fetal-source probability ratio; not population fetal-mortality RR"else
        "Observed linked death endpoint; unlinked deaths not corrected"
      ep[[paste(id,cv)]]<-q
    }
  }
  put(data.table::rbindlist(ep),"source_specific_endpoints.csv")
  pd<-list()
  for(id in c("all_years_shared_core","all_years_shared_augmented","reporting_years_shared_core","reporting_years_shared_augmented")) {
    r<-verify(id);q<-read(file.path(base,id,"paired_overall_differences.csv"))
    q$model<-id;q$live_n<-r$live_n;q$fetal_n<-r$fetal_n;pd[[id]]<-q
  }
  put(data.table::rbindlist(pd),"paired_GH_overall_differences.csv")
  # Version comparison isolates this age-range amendment, not the original
  # any-trimester redesign. Boundary movement is part of the present amendment.
  co<-merge(oldoverall,as.data.frame(alloverall),by="contrast",suffixes=c("_previous","_age45"))
  for(v in c("eligible_n","n","events","rr","rd_per1000"))co[[paste0(v,"_change")]]<-co[[paste0(v,"_age45")]]-co[[paste0(v,"_previous")]]
  put(co,"age45_vs_previous_overall.csv")
  oldage<-read(file.path(old,"main_age_estimates.csv"))
  cmp<-merge(oldage[oldage$covariance=="HC0",c("contrast","age","rr","rd")],
    as.data.frame(allages[covariance=="HC0",c("contrast","age","rr","rd")]),by=c("contrast","age"),all=TRUE,
    suffixes=c("_previous","_age45"))
  cmp$rr_change<-cmp$rr_age45-cmp$rr_previous;cmp$rd_change_per1000<-1000*(cmp$rd_age45-cmp$rd_previous)
  put(cmp,"age45_vs_previous_by_age.csv")
  inf<-merge(oldage[oldage$covariance=="HC0",c("contrast","age","rr_lower","rr_upper")],
    as.data.frame(allages[covariance=="HC0",c("contrast","age","rr_lower","rr_upper",
      "rd_bonferroni_lower_per1000","rd_bonferroni_upper_per1000")]),by=c("contrast","age"),
    suffixes=c("_previous","_age45"))
  classify<-function(lo,hi,null)ifelse(hi<null,"lower",ifelse(lo>null,"higher","includes_null"))
  inf$pointwise_previous<-classify(inf$rr_lower_previous,inf$rr_upper_previous,1)
  inf$pointwise_age45<-classify(inf$rr_lower_age45,inf$rr_upper_age45,1)
  inf$Bonferroni_age45<-classify(inf$rd_bonferroni_lower_per1000,inf$rd_bonferroni_upper_per1000,0)
  put(inf,"age45_vs_previous_pointwise_inference.csv")
  age45_json(list(status="completed_all_40_model_results_assembled",models=40,
    crossover_models=21,ages=15:45,no_imputation=TRUE),file.path(out,"receipt.json"))
}
if(sys.nframe()==0L)main()
