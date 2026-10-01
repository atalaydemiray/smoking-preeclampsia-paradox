# Current fitting entry point. Sourcing starts no fits and changes no data.
# Use from the repository root. The destination must be new and outside the repo.
current_analysis_plan <- function() {
  scripts<-args<-stages<-character()
  add<-function(stage,script,arg=""){stages<<-c(stages,stage);scripts<<-c(scripts,script);args<<-c(args,arg)}
  add("main and sensitivity fits","03_fit_natality.R","primary")
  add("historical sensitivity","03_fit_natality.R","historical")
  add("mortality and fetal-inclusive fits","06_fit_adjunct_models.R","all")
  for(ct in c("broad","prepregnancy"))add("large-cohort fits","14_large_cohort_refits.R",ct)
  for(ct in c("primary","broad","prepregnancy"))add("independent full-record checks","09_independent_numerical_validation.R",ct)
  add("assemble base results","08_assemble_results.R")
  add("descriptive aggregates","10_descriptive_tables.R")
  ids<-c(paste0("supp_t1dose_",c("1_5","6_10","11_20","21plus")),
    "supp_t1change_reduced","supp_t1change_same_or_more","supp_strict_continued_vs_stopped",
    "supp_strict_relapse_vs_stopped","supp_timing_stopped_by_T1","supp_timing_stopped_in_T2","supp_timing_stopped_in_T3",
    "supp_preterm_primary",paste0("supp_race_primary_",c("nhw","nhb","hisp","other")),
    "supp_3lvl_stopped_vs_none","supp_3lvl_continued_vs_none","supp_clean_prepregnancy","supp_clean_broad",
    paste0("supp_pdose_",c("1_5","6_10","11_20","21plus")),"supp_t3_through_vs_none",
    "supp_preterm_broad","supp_preterm_prepregnancy",paste0("supp_race_broad_",c("nhw","nhb","hisp","other")))
  for(id in ids)add("supplementary fits","21_fit_supplementary.R",paste("only",id))
  for(s in c("24_assemble_supplementary.R","32_table1_by_outcome.R","33_fit_joint_three_group.R","33b_derive_ss_vs_sn.R",
             "36_diagnostic_within_s_no_dose.R","38_fit_t1_only.R","39_flip_reference.R","34_summarize_joint_three_group.R"))add("joint model and reporting",s)
  for(p in c("joint","prepregnancy"))add("formal interaction tests","40_interaction_revision.R",p)
  add("interaction collection","40_collect_interactions.R")
  add("final release validation","40_validate_interaction_release.R")
  data.frame(step=seq_along(scripts),stage=stages,script=paste0("age45_revision/scripts/",scripts),arguments=args,stringsAsFactors=FALSE)
}

current_preflight <- function(input_workspace, check_packages=TRUE) {
  input_workspace<-normalizePath(input_workspace,mustWork=TRUE)
  prepared<-file.path(input_workspace,"age45_revision/derived")
  files<-character()
  for(ct in c("primary","broad","prepregnancy"))for(y in if(ct=="primary")2014:2024 else 2016:2024)
    files<-c(files,file.path(prepared,"model_inputs",paste0(y,"_",ct,c(".rds","_receipt.json"))))
  for(ct in c("within_ext","prepregnancy_ext"))for(y in 2016:2024)
    files<-c(files,file.path(prepared,"supplementary_inputs",paste0(y,"_",ct,c(".rds","_receipt.json"))))
  files<-c(files,file.path(prepared,"natality",paste0(2016:2024,"_ledger.csv")))
  adjunct<-file.path(prepared,"adjunct_models")
  stems<-paste0("linked_",c("infant","neonatal","early_neonatal"))
  for(prefix in c("gh","fetal_endpoint"))for(tier in c("shared_core","shared_augmented"))
    for(src in c("natality","fetal_death"))for(y in 2018:2024)
      stems<-c(stems,paste(prefix,y,src,tier,sep="_"))
  for(stem in stems)files<-c(files,file.path(adjunct,paste0(stem,c(".rds","_receipt.json"))))
  missing<-files[!file.exists(files)]
  if(length(missing))stop("Prepared-input prerequisites missing:\n",paste(missing,collapse="\n"),
    "\nRaw archives alone are insufficient for this prepared-data refit route. See model-fitting/README.md.")
  if(check_packages) {
    needed<-c("data.table","jsonlite","digest")
    absent<-needed[!vapply(needed,requireNamespace,TRUE,quietly=TRUE)]
    if(length(absent))stop("Install required packages before fitting: ",paste(absent,collapse=", "))
  }
  invisible(list(input_workspace=input_workspace,prepared=prepared,required_files=length(files),
    check="Existence and package preflight only; each model validates hashes during execution."))
}

prepare_current_workspace <- function(destination,input_workspace,repository=getwd(),check_packages=TRUE) {
  repository<-normalizePath(repository,mustWork=TRUE)
  stopifnot(file.exists(file.path(repository,"run_all.R")),!file.exists(destination),dir.exists(dirname(destination)))
  resolved_destination<-file.path(normalizePath(dirname(destination),mustWork=TRUE),basename(destination))
  if(startsWith(paste0(resolved_destination,"/"),paste0(repository,"/")))stop("Place the record-level rerun outside the repository.")
  gate<-current_preflight(input_workspace,check_packages=check_packages)
  src<-file.path(repository,"model-fitting/current")
  files<-list.files(src,recursive=TRUE,full.names=TRUE,all.files=TRUE,no..=TRUE)
  files<-files[!dir.exists(files)]
  dir.create(destination);destination<-normalizePath(destination,mustWork=TRUE)
  for(f in files){rel<-substring(f,nchar(src)+2L);to<-file.path(destination,rel);dir.create(dirname(to),recursive=TRUE,showWarnings=FALSE);stopifnot(file.copy(f,to))}
  # Only prepared data are linked. Outputs and executable code are private copies.
  stopifnot(file.symlink(gate$prepared,file.path(destination,"age45_revision/derived")))
  for(d in c("age45_revision/outputs","age45_revision/library","reproducibility/library"))dir.create(file.path(destination,d),recursive=TRUE,showWarnings=FALSE)
  writeLines("refit",file.path(destination,"AGE45_RERUN_MODE.txt"))
  writeLines(c("October repository refit; inputs linked read-only by convention.",
    "No import/preparation driver may write against the input link.",paste0("Input workspace: ",gate$input_workspace)),file.path(destination,"RERUN_INPUTS.txt"))
  write.csv(current_analysis_plan(),file.path(destination,"RUN_PLAN.csv"),row.names=FALSE)
  message("Prepared isolated workspace; no model has run: ",destination)
  invisible(destination)
}

run_current_analysis <- function(workspace,execute=FALSE) {
  workspace<-normalizePath(workspace,mustWork=TRUE)
  stopifnot(file.exists(file.path(workspace,"AGE45_RERUN_MODE.txt")),
    identical(readLines(file.path(workspace,"AGE45_RERUN_MODE.txt")),"refit"))
  plan<-current_analysis_plan()
  stopifnot(all(file.exists(file.path(workspace,plan$script))))
  if(!isTRUE(execute)){print(plan,row.names=FALSE);message("Plan only. Use execute=TRUE to start the serial record-level analysis, which can take many hours.");return(invisible(plan))}
  old<-setwd(workspace);on.exit(setwd(old),add=TRUE)
  for(i in seq_len(nrow(plan))) {
    message("Step ",i,"/",nrow(plan),": ",plan$stage[i])
    args<-if(nzchar(plan$arguments[i]))strsplit(plan$arguments[i]," ",fixed=TRUE)[[1]] else character()
    status<-system2(file.path(R.home("bin"),"Rscript"),c("--vanilla",shQuote(plan$script[i]),shQuote(args)))
    if(status!=0L)stop("Stopped at step ",i,": ",plan$script[i],". Inspect its log; no later step was run.")
  }
  message("Statistical plan completed. Results remain isolated in ",workspace)
  invisible(workspace)
}
