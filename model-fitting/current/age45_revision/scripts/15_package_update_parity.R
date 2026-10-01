# Verify the frozen 0.4.0.9003 reader against every 0.4.0.9002-derived
# clinical field in all eleven amended annual cohorts. No outputs replaced.
main<-function() {
  .libPaths(c(normalizePath("age45_revision/library"),.libPaths()))
  stopifnot(as.character(packageVersion("natality"))=="0.4.0.9003")
  data.table::setDTthreads(1L);arrow::set_cpu_count(1L)
  source("age45_revision/R/01_measurement_helpers.R")
  source("age45_revision/R/03_natality_adapter.R")
  source("age45_revision/R/10_model_specification.R")
  out<-"age45_revision/outputs/package_validation/version_9003_parity"
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  files<-list.files(system.file(package="natality"),full.names=TRUE,recursive=TRUE)
  pins<-vapply(files,sha,"")
  for(y in 2024:2014) {
    r<-jsonlite::fromJSON(file.path("age45_revision/derived/natality",paste0(y,"_receipt.json")))
    raw<-natality::read_natality(y,natality_selected_columns(y,TRUE),population="all_occurrence",download=FALSE)
    provenance<-attr(raw,"natality_provenance")
    stopifnot(provenance$artifact_sha256==r$package_provenance$artifact_sha256,
      nrow(raw)==r$counts$source_n,provenance$package_version=="0.4.0.9003")
    names(raw)<-tolower(names(raw));raw$year<-NULL;parts<-list()
    for(first in seq.int(1L,nrow(raw),by=100000L)) {
      ii<-first:min(nrow(raw),first+99999L)
      parts[[length(parts)+1L]]<-natality_input_chunk(raw[ii,,drop=FALSE],y,first-1L)$data
    }
    d<-data.table::rbindlist(parts);rm(raw,parts);gc(FALSE)
    path<-r$outputs$path[grepl("clinical[.]rds$",r$outputs$path)]
    stopifnot(sha(path)==r$outputs$sha256[match(path,r$outputs$path)])
    old<-readRDS(path)$data
    stopifnot(identical(names(d),names(old)),nrow(d)==nrow(old))
    checks<-data.frame(year=y,field=names(d),identical_values=vapply(names(d),function(f)identical(d[[f]],old[[f]]),TRUE))
    data.table::fwrite(checks,file.path(out,paste0(y,"_fields.csv")))
    stopifnot(all(checks$identical_values))
    jsonlite::write_json(list(status="PASS_all_clinical_fields_identical",year=y,n=nrow(d),
      old_package="0.4.0.9002",new_package="0.4.0.9003",old_input_sha256=sha(path),
      source_artifact_sha256=provenance$artifact_sha256,new_provenance=provenance),
      file.path(out,paste0(y,"_receipt.json")),pretty=TRUE,auto_unbox=TRUE)
    message(y,": package 9003 versus 9002 PASS all ",ncol(d)," fields, N=",nrow(d))
    rm(old,d);gc(FALSE)
  }
  stopifnot(identical(pins,vapply(files,sha,"")))
  jsonlite::write_json(list(status="PASS_all_11_years",model_inputs_unchanged=TRUE,
    new_package="0.4.0.9003",code_sha256=as.list(pins)),file.path(out,"receipt.json"),pretty=TRUE,auto_unbox=TRUE)
}
if(sys.nframe()==0L)main()
