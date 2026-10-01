source("scripts/64_crossover_coverage_validation.R");source("R/26_age_crossover.R")
checks<-0L;check<-function(x){stopifnot(isTRUE(x));checks<<-checks+1L}
fails<-function(x)check(inherits(tryCatch(force(x),error=identity),"error"))
s<-crossover_coverage_scenarios();check(nrow(s)==8L);check(!anyDuplicated(s$scenario))
check(sum(s$true_reversal)==4L)
for(i in which(s$true_reversal))check(abs(s$h_at30[i]+s$slope[i]*(s$fixed_null_age[i]-30))<1e-14)
check(!s$true_reversal[s$scenario=="varying_positive"])
check(s$h_at30[4]+s$slope[4]*(15-30)>0)
empty<-data.frame(lower=numeric(),upper=numeric());check(!crossover_in_set(30,empty))
sets<-data.frame(lower=c(15,29),upper=c(16,32));check(crossover_in_set(15,sets));check(crossover_in_set(31,sets));check(!crossover_in_set(20,sets))
invisible(capture.output(plan<-crossover_coverage_main(character())));check(plan$total_replications==8000L);check(!plan$clinical_records)
fails(crossover_coverage_main("--execute"));fails(crossover_coverage_main("--replications=25"))
fails(crossover_coverage_main(c("--replications=2","--replications=10")))
for(i in c(1L,4L,5L,7L,8L)) {
  one<-crossover_coverage_one(s[i,,drop=FALSE],1,6000L,640000L+10000L*i)
  check(one$status=="complete");check(one$events>0&&one$events<6000)
  check(is.logical(one$simultaneous_reversal_detected));check(is.finite(one$estimated_roots))
  if(i==1L){check(!one$local_delta_available);check(is.na(one$local_delta_covered_when_available));check(is.logical(one$global_null_whole_domain_covered))}
  if(i==5L)check(identical(one,crossover_coverage_one(s[i,,drop=FALSE],1,6000L,640000L+10000L*i)))
}
cat("PASS:",checks,"crossover coverage-driver synthetic checks; no clinical inputs\n")
