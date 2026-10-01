# October publication reconstruction from aggregates, not record-level fitting.
# Terminal: Rscript --vanilla run_all.R [new-output-directory]
# RStudio: source('run_all.R'); reproduce_publication()
reproduce_publication <- function(output = "output/2026-10-01") {
  stopifnot(file.exists("publication/source_manifest.csv"))
  source("R/tables_october.R",local=TRUE)
  if(dir.exists(output)&&length(list.files(output,all.files=TRUE,no..=TRUE)))
    stop("Output is not empty. Choose a new directory; existing results are never deleted.")
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(output,"tables"));dir.create(file.path(output,"figures"))
  tables<-build_october_tables()
  for(name in names(tables)) {
    expected<-oct_matrix(file.path("publication/reference_tables",paste0(name,".csv")))
    actual<-tables[[name]]
    if(!identical(dim(actual),dim(expected))||!all(actual==expected)) {
      if(identical(dim(actual),dim(expected))) {
        delta<-which(actual!=expected,arr.ind=TRUE)
        for(i in seq_len(min(nrow(delta),8L))){rc<-delta[i,];message(name," row ",rc[1]," col ",rc[2],": ",actual[rc[1],rc[2]]," != ",expected[rc[1],rc[2]])}
      }
      stop("Reconstructed table differs from October reference: ",name)
    }
    write.table(actual,file.path(output,"tables",paste0(name,".csv")),sep=",",row.names=FALSE,col.names=FALSE,qmethod="double",na="")
  }
  stopifnot(file.copy("publication/table_metadata.csv",file.path(output,"tables/table_metadata.csv")))
  source("publication/Code/reproduce_figures.R",local=TRUE)
  reproduce_figures("publication",normalizePath(file.path(output,"figures")))
  source("tests/check_publication.R",local=TRUE)
  check_publication(output)
  writeLines(c("October publication: 27 CSV tables (3 main, 19 supplementary numbers with lettered parts) and 6 figures.",
    "Every table cell matches the approved October package. Aggregate checks are not a new record-level analysis.",
    capture.output(sessionInfo())),file.path(output,"session_info.txt"))
  message("PASS: publication reconstruction and numerical checks. Output: ",normalizePath(output))
  invisible(normalizePath(output))
}
if(sys.nframe()==0L){args<-commandArgs(TRUE);reproduce_publication(if(length(args))args[1] else "output/2026-10-01")}
