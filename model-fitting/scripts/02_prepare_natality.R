# Build the three prepared model inputs per year from the decoded clinical files.
# Source from model-fitting/, or run Rscript --vanilla with optional comma-separated years.
main <- function(years = 2024:2014, contrasts_to_prepare=c("primary","broad","prepregnancy")) {
  library(data.table); setDTthreads(1L)
  for (f in c("01_measurement_helpers", "05_model_specification"))
    source(file.path("R", paste0(f, ".R")), local = environment())
  sha <- function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE)
  writej <- function(x,p) jsonlite::write_json(x,p,pretty=TRUE,auto_unbox=TRUE,digits=16,null="null")
  out <- "derived/model_inputs"
  dir.create(out, recursive=TRUE, showWarnings=FALSE)
  contrasts <- c(primary="within_prepregnancy_smokers",
    broad="complementary", prepregnancy="prepregnancy_only")
  for (y in years) {
    rp <- file.path("derived/natality",paste0(y,"_receipt.json"))
    waiting <- 0L
    while(!file.exists(rp)) {
      if(waiting>=1800L)stop("Timed out waiting for a validated annual package receipt: ",y)
      if(waiting%%60L==0L)message("Waiting for validated package import: ",y)
      Sys.sleep(15);waiting<-waiting+15L
    }
    r <- jsonlite::fromJSON(rp)
    stopifnot(r$status=="complete_package_import_old_age_reconciled",
      isTRUE(r$all_old_derived_fields_identical)||isTRUE(r$all_model_fields_identical),
      r$original_record_positions_identical)
    file <- r$outputs$path[grepl("clinical[.]rds$", r$outputs$path)]
    stopifnot(length(file)==1L, sha(file)==r$outputs$sha256[match(file,r$outputs$path)])
    d <- readRDS(file)$data
    period <- if(y<=2015)2014:2015 else 2016:2024
    for(ct in intersect(if(y<=2015)"primary" else names(contrasts),contrasts_to_prepare)) {
      stem <- file.path(out,paste(y,ct,sep="_"))
      if(file.exists(paste0(stem,"_receipt.json"))) {
        old <- jsonlite::fromJSON(paste0(stem,"_receipt.json"))
        stopifnot(old$input_sha256==sha(file),sha(paste0(stem,".rds"))==old$output_sha256)
        next
      }
      spec <- lock_sep_model_specification("natality_main",contrasts[[ct]],period,
        c(21,27,35),c(19.6,26.1,37.8))
      p <- prepare_sep_model_data(d,spec,"natality")
      at <- p$identity$input_row
      p$identity$oe_weeks <- d$oe_weeks[at]
      p$identity$prepregnancy_dose_raw <- as.integer(as.character(d$source_cig_0[at]))
      # Retained for old/new membership auditing and the no-T1-condition check.
      p$identity$T1_known <- as.character(d$source_f_cigs_1[at])%in%"1" &
        as.integer(as.character(d$source_cig_1[at]))%in%0:98
      cc <- complete.cases(p$data[spec$covariates])
      support <- data.table(age=p$data$age,A=p$data$A,Y=p$data$Y,cc=cc)[,
        .(eligible_n=.N,eligible_events=sum(Y),cc_n=sum(cc),cc_events=sum(Y[cc])),by=.(age,A)]
      stopifnot(any(p$data$age==45),!anyDuplicated(p$identity[c("source_type","year","source_row")]),
        all(p$data$age%in%15:45),p$baseline_records_dropped==0)
      p$construction <- list(source_package="natality",source_package_version="0.4.0.9002",
        input_sha256=sha(file),amendment="15-45 years; no other eligibility change",
        no_imputation=TRUE)
      path <- paste0(stem,".rds"); saveRDS(p,path,compress="gzip")
      stopifnot(isTRUE(all.equal(readRDS(path),p,tolerance=0,check.attributes=TRUE)))
      fwrite(support,paste0(stem,"_support.csv"))
      fwrite(p$missingness_counts,paste0(stem,"_missingness.csv"))
      counts <- list(eligible_n=nrow(p$data),cc_n=sum(cc),cc_events=sum(p$data$Y[cc]),
        added_eligible_n=sum(p$data$age==45),added_cc_n=sum(cc&p$data$age==45),
        added_cc_events=sum(p$data$Y[cc&p$data$age==45]))
      writej(list(status="complete_prepared_age15_45",year=y,contrast=ct,counts=counts,
        input_sha256=sha(file),output_sha256=sha(path),no_imputation=TRUE,
        age_boundaries=spec$age_spec$boundaries,age_knots=spec$age_spec$knots,
        model_specification_sha256=sha("R/05_model_specification.R")),
        paste0(stem,"_receipt.json"))
      message(y," ",ct,": eligible=",counts$eligible_n,"; CC=",counts$cc_n,
        "; age45 CC=",counts$added_cc_n)
      rm(p); gc(FALSE)
    }
    rm(d); gc(FALSE)
  }
}
if(sys.nframe()==0L) {
  a<-commandArgs(TRUE)
  main(if(length(a))as.integer(strsplit(a[1],",",fixed=TRUE)[[1]]) else 2024:2014)
}
