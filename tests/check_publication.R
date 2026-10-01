# Independent aggregate checks, in addition to exact publication-table comparison.
check_publication <- function(output) {
  read<-function(...)read.csv(file.path("publication/Code",...),stringsAsFactors=FALSE,check.names=FALSE)
  tests<-read("interaction_tests/global_interaction_tests.csv")
  stopifnot(nrow(tests)==2L,identical(tests$population,c("joint","prepregnancy")),
    identical(as.integer(tests$n),c(30076524L,30097165L)),identical(as.integer(tests$events),c(2496317L,2498098L)),
    identical(as.integer(tests$lrt_df),c(8L,4L)))
  for(pop in tests$population) {
    d<-read("interaction_tests",pop,"interaction_tests.csv");r<-d[d$test=="Likelihood ratio (model based)",]
    stopifnot(nrow(r)==1L,abs(r$statistic-2*(r$full_log_likelihood-r$reduced_log_likelihood))<1e-6,
      r$df==r$full_parameters-r$reduced_parameters,
      abs(pchisq(r$statistic,r$df,lower.tail=FALSE,log.p=TRUE)-r$log_p_value)<1e-8)
    v<-read("interaction_tests",pop,"validation.csv");stopifnot(all(v$passed),all(v$absolute_difference<=v$tolerance))
  }
  # Recompute Holm on the log scale, rather than treating underflow zeros as P=0.
  for(prefix in c("lrt","wald_hc0")) {
    lp<-tests[[paste0(prefix,"_log_p")]];ord<-order(lp)
    adj<-pmin(0,cummax(lp[ord]+log(rev(seq_along(lp)))))
    stopifnot(all(is.finite(lp)),max(abs(adj-tests[[paste0(prefix,"_holm_log_p")]][ord]))<1e-8)
  }
  summary<-read("supplementary/aggregate_outputs/joint_three_group/summary_main_summary.csv")
  ages<-read("supplementary/aggregate_outputs/joint_three_group/summary_main_age_estimates.csv")
  for(pair in list(c("sn_vs_ss","joint_ss_vs_sn"),c("nn_vs_ss","joint_ss_vs_nn"),c("n_vs_s","prepregnancy_main"))) {
    a<-summary[summary$model_id==pair[1],];b<-summary[summary$model_id==pair[2],]
    stopifnot(nrow(a)==1,nrow(b)==1,abs(a$rr*b$rr-1)<1e-12,abs(a$rd_per1000+b$rd_per1000)<1e-10,
      abs(a$rr_lower*b$rr_upper-1)<1e-12,abs(a$crossover_age-b$crossover_age)<1e-12)
    aa<-ages[ages$model_id==pair[1],];stopifnot(identical(sort(as.integer(aa$age)),15:45),
      all(abs(aa$rr-aa$risk1_per1000/aa$risk0_per1000)<1e-10),
      all(abs(aa$rd_per1000-(aa$risk1_per1000-aa$risk0_per1000))<1e-10))
  }
  pop<-read("interaction_tests/population_overall.csv")
  stopifnot(all(pop$eligible_n-pop$excluded_for_covariates_n==pop$complete_case_n))
  missing<-read("interaction_tests/missingness_overall.csv")
  stopifnot(all(missing$unknown_n>=0),all(missing$unknown_n<=missing$eligible_n),
    max(abs(missing$unknown_percent-100*missing$unknown_n/missing$eligible_n))<1e-10)
  pdfs<-list.files(file.path(output,"figures"),"[.]pdf$",full.names=TRUE)
  stopifnot(length(pdfs)==6L,all(file.size(pdfs)>1000),
    all(vapply(pdfs,function(p){con<-file(p,"rb");on.exit(close(con));rawToChar(readBin(con,"raw",5))=="%PDF-"},TRUE)))
  stopifnot(length(list.files(file.path(output,"tables"),"^Table_.*[.]csv$"))==27L)
  message("PASS: likelihood/df/log-P/Holm, inverse contrasts, ages 15-45, denominators, and six PDFs.")
  invisible(TRUE)
}
