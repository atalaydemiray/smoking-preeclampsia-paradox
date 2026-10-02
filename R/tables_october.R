# October tables from aggregate estimates. Reference tables are tests, not inputs.
oct_read <- function(...) utils::read.csv(file.path("publication/Code", ...), check.names=FALSE, stringsAsFactors=FALSE, na.strings="NA")
oct_matrix <- function(path) as.matrix(utils::read.csv(path,header=FALSE,colClasses="character",check.names=FALSE,na.strings=NULL))
oct_layout <- function(key, values) {
  layout<-oct_matrix(file.path("publication/layouts",paste0(key,".csv")));values<-as.matrix(values)
  stopifnot(nrow(layout)==nrow(values)+1L,ncol(layout)==ncol(values))
  values[,1]<-layout[-1,1] # Static headings/labels only; never estimate cells.
  out<-rbind(layout[1,],values);dimnames(out)<-NULL;out
}
oct_ci <- function(e,lo,hi,digits) sprintf(paste0("%.",digits,"f (%.",digits,"f, %.",digits,"f)"),e,lo,hi)
oct_n <- function(x) formatC(as.numeric(x),format="f",digits=0,big.mark=",")
oct_set <- function(x) {x<-ifelse(is.na(x)|x%in%c("","Empty"),"none",x);gsub(" U "," and ",x,fixed=TRUE)}
oct_joint <- function(name) oct_read("supplementary/aggregate_outputs/joint_three_group",paste0("summary_",name,".csv"))
oct_one <- function(d,key,value) {r<-d[d[[key]]==value,,drop=FALSE];stopifnot(nrow(r)==1L);r}
oct_risk <- function(r,key,digits=1) oct_ci(r[[paste0(key,"_per1000")]],r[[paste0(key,"_lower_per1000")]],r[[paste0(key,"_upper_per1000")]],digits)
oct_summary_rows <- function(d) do.call(rbind,lapply(seq_len(nrow(d)),function(i){r<-d[i,];c(r$model_id,
  oct_ci(r$crossover_age,r$delta_lower,r$delta_upper,1),oct_set(r$full_95_fixed_null_age_set),
  oct_set(r$simultaneous_strict_negative),oct_set(r$simultaneous_strict_positive),if(isTRUE(r$simultaneous_reversal))"Yes" else "No")}))

# Select the confidence-set component containing the fitted root, not simply the
# first interval. Complete sets, including boundary uncertainty, remain in S19.
oct_root_component <- function(confidence_set, root) {
  if (length(confidence_set) != 1L || is.na(confidence_set) ||
      length(root) != 1L || !is.finite(root)) {
    stop("A finite crossover and a nonmissing null-age confidence set are required.")
  }
  components <- trimws(strsplit(confidence_set, " U ", fixed = TRUE)[[1]])
  contains_root <- vapply(components, function(component) {
    if (!substr(component, 1L, 1L) %in% c("[", "(") ||
        !substr(component, nchar(component), nchar(component)) %in% c("]", ")")) {
      stop("Malformed null-age interval: ", component)
    }
    bounds <- regmatches(component, gregexpr("-?[0-9]+(?:[.][0-9]+)?", component, perl = TRUE))[[1]]
    if (length(bounds) != 2L) stop("Malformed null-age interval: ", component)
    bounds <- as.numeric(bounds)
    if (bounds[1] > bounds[2]) stop("Reversed null-age interval: ", component)
    lower_ok <- if (startsWith(component, "[")) root >= bounds[1] else root > bounds[1]
    upper_ok <- if (endsWith(component, "]")) root <= bounds[2] else root < bounds[2]
    lower_ok && upper_ok
  }, logical(1))
  if (sum(contains_root) != 1L) stop("Expected exactly one confidence-set component containing the crossover.")
  components[contains_root]
}

build_october_tables <- function() {
  # Unchanged sensitivity calculations are reused in a private environment.
  # No bias-scenario or imputation routine is invoked.
  legacy<-new.env(parent=globalenv())
  for(f in c("paths","format","labels","tables_retained","tables_specification","row_formatting")) sys.source(file.path("R",paste0(f,".R")),legacy)
  out<-list();joint<-oct_joint("main_summary")
  main<-joint[match(c("sn_vs_ss","nn_vs_ss","n_vs_s"),joint$model_id),]
  stopifnot(nrow(main)==3L,!anyNA(main$model_id))
  desc<-oct_read("table_inputs/table1_by_outcome_long.csv")
  vars<-list(pattern=c("SS","SN","NN","other"),pre_smoking=c("S","N"),age="",
    age_group=c("15-19","20-24","25-29","30-34","35-39","40-45"),bmi="",
    race_ethnicity=c("Non-Hispanic White","Non-Hispanic Black","Non-Hispanic AIAN","Non-Hispanic Asian","Non-Hispanic NHOPI","Non-Hispanic multiple race","Hispanic"),
    education4=c("Less than high school","High school or GED","Some college or associate","Bachelor or higher"),
    prior_living4=c("0","1","2","3 or more"),prepreg_diabetes=c("No","Yes"),prior_preterm=c("No","Yes"),prior_cesarean=c("No","Yes"),
    nativity=c("Born in 50 US states","Born elsewhere including territories"),year_factor=as.character(2016:2024))
  body<-list()
  for(v in names(vars))for(level in vars[[v]]) {
    rr<-desc[desc$variable==v&desc$level==level,];rr<-rr[match(c("gh_pe","no_gh_pe","total"),rr$column),]
    stopifnot(nrow(rr)==3L,!anyNA(rr$column));body[[length(body)+1L]]<-c(v,rr$display)
  }
  out$Table_1<-oct_layout("Table_1",do.call(rbind,body))
  groups<-oct_read("supplementary/aggregate_outputs/joint_three_group/age_group_support.csv")
  pre<-oct_read("supplementary/aggregate_outputs/main_models/prepregnancy_main_age_arm_support.csv")
  totals<-aggregate(cbind(n,events)~group,groups,sum);pretot<-aggregate(cbind(n,events)~A,pre,sum)
  rrow<-function(label,n,ev,r,ref)c(label,oct_n(n),oct_n(ev),oct_risk(r,if(ref)"risk0" else "risk1"),
    if(ref)"1 (reference)" else oct_ci(r$rr,r$rr_lower,r$rr_upper,3),if(ref)"0 (reference)" else oct_risk(r,"rd",2))
  gg<-function(g)oct_one(totals,"group",g)
  stopifnot(sum(totals$n)==main$fit_n[1],sum(pretot$n)==main$fit_n[3],abs(main$risk0_per1000[1]-main$risk0_per1000[2])<1e-9)
  out$Table_2<-oct_layout("Table_2",rbind(c("primary",rep("",5)),rrow("SS",gg("SS")$n,gg("SS")$events,main[1,],TRUE),
    rrow("SN",gg("SN")$n,gg("SN")$events,main[1,],FALSE),rrow("NN",gg("NN")$n,gg("NN")$events,main[2,],FALSE),c("secondary",rep("",5)),
    rrow("S",pretot$n[pretot$A==1],pretot$events[pretot$A==1],main[3,],TRUE),rrow("N",pretot$n[pretot$A==0],pretot$events[pretot$A==0],main[3,],FALSE)))
  roots <- oct_summary_rows(main)
  central_sets <- vapply(seq_len(nrow(main)), function(i)
    oct_root_component(main$full_95_fixed_null_age_set[i], main$crossover_age[i]), character(1))
  out$Table_3 <- oct_layout("Table_3", cbind(roots[, 1:2], central_sets, roots[, 4:5]))
  out$Table_S19 <- oct_layout("Table_S19", roots)
  for(k in c("S1","S6a","S6b","S7","S8","S9","S10"))out[[paste0("Table_",k)]]<-oct_layout(paste0("Table_",k),get(paste0("table_",tolower(k)),legacy)()[-1,,drop=FALSE])
  pops<-oct_read("interaction_tests/population_overall.csv");pops<-pops[match(c("joint","prepregnancy"),pops$population),]
  body<-lapply(seq_len(nrow(pops)),function(i){r<-pops[i,];c(r$population,oct_n(r$eligible_n),oct_n(r$complete_case_n),
    sprintf("%s (%.2f%%)",oct_n(r$excluded_for_covariates_n),100*r$excluded_for_covariates_n/r$eligible_n),oct_n(r$events),sprintf("%.2f",1000*r$events/r$complete_case_n))})
  out$Table_S2<-oct_layout("Table_S2",do.call(rbind,body))
  covbody<-list()
  for(label in c("SN_vs_SS","NN_vs_SS","N_vs_S"))for(cv in c("HC0","model")) {
    d<-oct_read("table_inputs/covariance",paste0(label,"_",cv,"_overall_standardized.csv"));r<-oct_one(d,"measure","rr");rd<-oct_one(d,"measure","rd")
    risk<-function(key){r<-oct_one(d,"measure",key);oct_ci(r$estimate_per1000,r$lower_per1000,r$upper_per1000,2)}
    covbody[[length(covbody)+1]]<-c(label,if(cv=="HC0")"HC0 sandwich" else "Model based",risk("risk1"),risk("risk0"),
      oct_ci(1/r$estimate,1/r$upper,1/r$lower,3),oct_ci(-rd$estimate_per1000,-rd$upper_per1000,-rd$lower_per1000,2))
  }
  out$Table_S3<-oct_layout("Table_S3",do.call(rbind,covbody));ages<-oct_joint("main_age_estimates")
  mapping<-c(S4a="sn_vs_ss",S4b="nn_vs_ss",S4c="n_vs_s",S4e="t1_only")
  for(suffix in names(mapping)) {
    d<-ages[ages$model_id==mapping[[suffix]],];d<-d[order(d$age),];stopifnot(identical(as.integer(d$age),15:45))
    values<-cbind(as.character(d$age),oct_n(d$target_n),sprintf("%.2f",d$risk0_per1000),sprintf("%.2f",d$risk1_per1000),
      oct_ci(d$rr,d$rr_lower,d$rr_upper,4),oct_ci(d$rd_per1000,d$rd_lower_per1000,d$rd_upper_per1000,2))
    out[[paste0("Table_",suffix)]]<-oct_layout(paste0("Table_",suffix),values)
  }
  out$Table_S4d<-oct_layout("Table_S4d",legacy$age_specific_table("primary","Table_S4d")[-1,])
  older<-legacy$table_s5()[-1,,drop=FALSE]
  # The approved inherited table reduces its two-decimal display to one decimal.
  # Preserve that display convention; full-precision roots remain in source CSVs.
  older[,2]<-vapply(older[,2],function(s){m<-gregexpr("[0-9]+[.][0-9]+",s);regmatches(s,m)<-lapply(regmatches(s,m),function(x)sprintf("%.1f",as.numeric(x)));s},"")
  extra<-joint[match(c("sn_vs_ss","nn_vs_ss","joint_sn_vs_nn","t1_only"),joint$model_id),]
  out$Table_S5<-oct_layout("Table_S5",rbind(oct_summary_rows(extra),older))
  fits<-oct_read("supplementary/aggregate_outputs/supp_summary.csv")
  ids<-list(S11=c("supp_strict_continued_vs_stopped","supp_clean_prepregnancy","supp_clean_broad"),
    S12=c(paste0("supp_t1dose_",c("1_5","6_10","11_20","21plus")),"supp_t1change_reduced","supp_t1change_same_or_more",paste0("supp_pdose_",c("1_5","6_10","11_20","21plus"))),
    S13=c("supp_t3_through_vs_none","supp_timing_stopped_by_T1","supp_timing_stopped_in_T2","supp_timing_stopped_in_T3","supp_strict_relapse_vs_stopped"),
    S14=c(paste0("supp_preterm_",c("primary","broad","prepregnancy")),paste0("supp_race_primary_",c("nhw","nhb","hisp","other")),paste0("supp_race_broad_",c("nhw","nhb","hisp","other"))))
  for(key in names(ids)){stopifnot(all(ids[[key]]%in%fits$model_id));body<-do.call(rbind,lapply(ids[[key]],function(id)legacy$sens_row(fits,id,id)));out[[paste0("Table_",key)]]<-oct_layout(paste0("Table_",key),body)}
  out$Table_S15<-oct_matrix("publication/Code/table_inputs/exposure_definitions.csv")
  missing<-oct_read("interaction_tests/missingness_overall.csv");variables<-unique(missing$variable)
  body<-list(c("eligible",oct_n(pops$eligible_n)))
  for(v in variables){d<-missing[missing$variable==v,];d<-d[match(pops$population,d$population),];stopifnot(identical(as.numeric(d$eligible_n),as.numeric(pops$eligible_n)));body[[length(body)+1]]<-c(v,sprintf("%s (%.2f%%)",oct_n(d$unknown_n),100*d$unknown_n/d$eligible_n))}
  body[[length(body)+1]]<-c("excluded",sprintf("%s (%.2f%%)",oct_n(pops$excluded_for_covariates_n),100*pops$excluded_for_covariates_n/pops$eligible_n))
  body[[length(body)+1]]<-c("complete",oct_n(pops$complete_case_n));out$Table_S17<-oct_layout("Table_S17",do.call(rbind,body))
  poporder<-c("joint","joint","joint","prepregnancy","prepregnancy");go<-c("SS","SN","NN","S","N");counts<-continuous<-categorical<-list()
  for(p in unique(poporder)){counts[[p]]<-oct_read("interaction_tests",p,"group_counts.csv");continuous[[p]]<-oct_read("interaction_tests",p,"descriptive_continuous.csv");categorical[[p]]<-oct_read("interaction_tests",p,"descriptive_categorical.csv")}
  body<-list(c("events",vapply(1:5,function(i){r<-oct_one(counts[[poporder[i]]],"group",go[i]);sprintf("%s (%.1f%%)",oct_n(r$events),100*r$events/r$n)},"")))
  for(v in c("age","bmi"))body[[length(body)+1]]<-c(v,vapply(1:5,function(i){d<-continuous[[poporder[i]]];r<-d[d$group==go[i]&d$variable==v,];stopifnot(nrow(r)==1);sprintf("%.1f (%.1f)",r$mean,r$sd)},""))
  for(v in setdiff(variables,"bmi")){
    levels<-vars[[v]];stopifnot(length(levels)>0)
    for(i in 1:5){d<-categorical[[poporder[i]]];r<-d[d$group==go[i]&d$variable==v,];stopifnot(sum(r$n)==oct_one(counts[[poporder[i]]],"group",go[i])$n)}
    for(level in levels)body[[length(body)+1]]<-c(v,vapply(1:5,function(i){d<-categorical[[poporder[i]]];r<-d[d$group==go[i]&d$variable==v&d$level==level,];if(!nrow(r))return("0 (0.0%)");stopifnot(nrow(r)==1);sprintf("%s (%.1f%%)",oct_n(r$n),r$percent)},""))
  }
  out$Table_S16<-oct_layout("Table_S16",do.call(rbind,body))
  tests<-oct_read("interaction_tests/interaction_tests.csv");tests<-tests[order(grepl("exploratory",tests$test),match(tests$population,c("joint","prepregnancy")),seq_len(nrow(tests))),]
  body<-cbind(tests$population,tests$test,tests$hypothesis,oct_n(tests$n),sprintf("%.2f",tests$statistic),as.character(tests$df),tests$p_display,tests$holm_p_display)
  out$Table_S18<-oct_layout("Table_S18",body);stopifnot(length(out)==27L);out
}
