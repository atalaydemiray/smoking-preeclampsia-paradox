# Run the natality package test suite and the synthetic age-boundary fixtures, and
# record both in outputs/package_validation. Run from model-fitting/.
# NATALITY_PACKAGE_SOURCE overrides where the package source tree is read from.
main <- function() {
  out<-"outputs/package_validation"
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  library(natality)
  testpath<-Sys.getenv("NATALITY_PACKAGE_SOURCE",unset="vendor/natality")
  # The package ships as a frozen tarball, so unpack it the first time the tests run.
  if(!dir.exists(testpath)&&file.exists("vendor/natality_0.4.0.9003.tar.gz"))
    utils::untar("vendor/natality_0.4.0.9003.tar.gz",exdir=dirname(testpath))
  stopifnot(file.exists(file.path(testpath,"DESCRIPTION")),
    as.character(packageVersion("natality"))=="0.4.0.9003",
    read.dcf(file.path(testpath,"DESCRIPTION"))[1,"Version"]=="0.4.0.9003")
  result<-as.data.frame(testthat::test_local(testpath,reporter="summary"))
  result$result<-NULL
  write.csv(result,file.path(out,"package_unit_tests.csv"),row.names=FALSE)
  stopifnot(sum(result$nb)>=89L,sum(result$failed)==0L,!any(result$error),!any(result$skipped))
  for(f in c("01_measurement_helpers","02_natality_adapter","05_model_specification"))
    source(paste0("R/",f,".R"),local=environment())
  fixture<-function(year=2024L) {
    ages<-c(14,15,20,30,40,44,45,46)
    fields<-natality_selected_columns(year,TRUE);n<-length(ages)
    d<-as.data.frame(setNames(rep(list(rep(1,n)),length(fields)),fields))
    d$dob_yy<-year;d$mager<-ages;d$oegest_comb<-39
    d$imp_plur<-d$mage_impflg<-""
    for(f in natality_provenance_columns())d[[f]]<-""
    for(f in c("rf_phype","rf_ghype","rf_pdiab","rf_ppterm","rf_cesar","rf_inftr"))d[[f]]<-"N"
    d$rf_artec<-"X";d$bmi<-24.1;d$priorlive<-d$priordead<-0
    for(f in paste0("cig_",0:3))d[[f]]<-10
    d$cig_1<-rep(c(0,5),4)
    d
  }
  checks<-list()
  check<-function(label,pass) {
    stopifnot(isTRUE(pass))
    checks[[length(checks)+1L]]<<-data.frame(check=label,passed=pass)
  }
  for(y in 2014:2024) {
    d<-fixture(y);p<-natality_input_chunk(d,y)
    check(paste(y,"15 and45 retained;14 and46 excluded"),
      identical(as.numeric(p$data$age),c(15,20,30,40,44,45)))
    sp<-lock_sep_model_specification("natality_main","within_prepregnancy_smokers",
      y,c(21,27,35),c(19.6,26.1,37.8))
    z<-prepare_sep_model_data(p$data,sp,"natality")
    check(paste(y,"model preparation includes45"),45%in%z$data$age)
    d$rf_phype[d$mager==45]<-"Y"
    check(paste(y,"age45 CHTN exclusion retained"),
      !45%in%natality_input_chunk(d,y)$data$age)
    d<-fixture(y);d$f_cigs_1[d$mager==45]<-0
    a<-natality_input_chunk(d,y)$data
    check(paste(y,"unknownT1 not recoded as abstinence"),
      !a$primary_member[a$age==45])
    sp<-lock_sep_model_specification("natality_main","prepregnancy_only",
      y,c(21,27,35),c(19.6,26.1,37.8))
    z<-prepare_sep_model_data(a,sp,"natality")
    check(paste(y,"prepregnancy contrast retains unknownT1 at45"),45%in%z$data$age)
  }
  write.csv(do.call(rbind,checks),file.path(out,"age45_boundary_tests.csv"),row.names=FALSE)
  jsonlite::write_json(list(status="PASS",package_version=as.character(packageVersion("natality")),
    package_assertions=sum(result$nb),boundary_checks=length(checks),
    no_data_downloads=TRUE,no_imputation=TRUE),
    file.path(out,"package_and_boundary_test_receipt.json"),pretty=TRUE,auto_unbox=TRUE)
}
if(sys.nframe()==0L)main()
