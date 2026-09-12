# Extend source-specific mortality inputs. Natality package has no full fetal/
# linked-cohort interface; retain the already audited parsers and source gates.
main <- function(kind="all") {
  # Source archives, dictionaries and schemas are read relative to this directory.
  root<-"."
  library(data.table);setDTthreads(1L)
  for(f in c("01_measurement_helpers","03_linked_clinical_adapter","04_fetal_clinical_adapter"))
    source(file.path("R",paste0(f,".R")),local=environment())
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  writej<-function(x,p)jsonlite::write_json(x,p,pretty=TRUE,auto_unbox=TRUE,digits=16,null="null")
  if(kind%in%c("all","fetal")) {
    schema<-fetal_schema_with_provenance_r(jsonlite::fromJSON("config/fetal_schema.json",simplifyVector=FALSE))
    out<-"derived/fetal";dir.create(out,recursive=TRUE,showWarnings=FALSE)
    for(y in 2018:2024) {
      rp<-file.path(out,paste0(y,"_receipt.json"))
      if(file.exists(rp)) {r<-jsonlite::fromJSON(rp);stopifnot(sha(r$output_path)==r$output_sha256);next}
      cfg<-schema$years[[as.character(y)]]
      archive<-file.path(root,cfg$archive$relative_path)
      stopifnot(sha(archive)==cfg$archive$sha256,
        sha(file.path(root,cfg$guide$relative_path))==cfg$guide$sha256)
      con<-unz(archive,cfg$exact_member,open="r")
      lines<-readLines(con,warn=FALSE);close(con)
      raw<-fetal_parse_lines_r(lines,schema,y)
      a<-adapt_fetal_r(raw,schema,y);d<-a$data[a$data$clinical_member,,drop=FALSE]
      oldrp<-file.path("derived/fetal_inputs/full_2018_2024_20260906_step1c",paste0(y,"_receipt.json"))
      oldr<-jsonlite::fromJSON(oldrp)
      ent<-oldr$outputs[basename(oldr$outputs$path)==paste0(y,"_clinical.rds"),]
      stopifnot(nrow(ent)==1L,sha(ent$path)==ent$sha256)
      old<-as.data.frame(readRDS(ent$path)$data)
      legacy<-d[d$age<=44,,drop=FALSE];rownames(legacy)<-rownames(old)<-NULL
      # Columns are decoded by the same parser, now with only the age gate changed.
      compare <- lapply(names(old),function(f) {
        a<-legacy[[f]];b<-old[[f]]
        if(is.factor(a))a<-as.character(a)
        if(is.factor(b))b<-as.character(b)
        data.frame(field=f,pass=isTRUE(all.equal(a,b,tolerance=0,check.attributes=FALSE)))
      })
      compare<-do.call(rbind,compare)
      fwrite(compare,file.path(out,paste0(y,"_legacy_reconciliation.csv")))
      if(!all(compare$pass))stop("Fetal legacy mismatch: ",paste(compare$field[!compare$pass],collapse=", "))
      stopifnot(nrow(legacy)==nrow(old),setequal(names(legacy),names(old)),
        !anyDuplicated(d$source_row),all(d$age%in%15:45))
      path<-file.path(out,paste0(y,"_clinical.rds"))
      z<-list(data=d,metadata=list(age_domain=c(15,45),archive_sha256=sha(archive)))
      saveRDS(z,path);stopifnot(identical(readRDS(path),z))
      fwrite(a$ledger,file.path(out,paste0(y,"_ledger.csv")))
      writej(list(status="complete_fetal_age15_45_old_rows_unchanged",year=y,
        source_n=nrow(raw),clinical_n=nrow(d),age45_n=sum(d$age==45),
        old_input_sha256=ent$sha256,output_path=path,output_sha256=sha(path)),rp)
      message("Fetal ",y,": added clinical age45=",sum(d$age==45))
    }
  }
  if(kind%in%c("all","linked")) {
    out<-"derived/linked_age45";dir.create(out,recursive=TRUE,showWarnings=FALSE)
    for(y in 2018:2023) {
      rp<-file.path(out,paste0(y,"_receipt.json"))
      if(file.exists(rp)) {r<-jsonlite::fromJSON(rp);stopifnot(sha(r$output_path)==r$output_sha256);next}
      row<-linked_cohort_config(root,y);gate<-linked_source_gate(root,row,TRUE)
      stopifnot(gate$ready)
      oldrp<-file.path("derived/linked_inputs/full_2018_2023_20260906_step1b",paste0(y,"_receipt.json"))
      oldr<-jsonlite::fromJSON(oldrp,simplifyVector=FALSE)
      stopifnot(oldr$full_source,all(unlist(oldr$full_reconciliation)))
      ent<-oldr$outputs$numerator_index
      stopifnot(sha(ent$path)==ent$sha256,oldr$source_gate$archive_sha256==gate$archive_sha256)
      numobj<-readRDS(ent$path)
      stopifnot(numobj$metadata$full_numerator_search)
      num<-numobj$data
      inv<-utils::unzip(gate$archive,list=TRUE)
      stopifnot(sum(inv$Name==row$denominator)==1L)
      expected<-inv$Length[inv$Name==row$denominator]/1348
      sid<-paste(basename(gate$archive),row$denominator,sep="/")
      con<-unz(gate$archive,row$denominator,open="rb")
      n<-n45<-0L;pieces<-list();matched<-integer()
      repeat {
        lines<-readLines(con,n=10000L,warn=FALSE,encoding="ASCII")
        if(!length(lines))break
        stopifnot(all(nchar(lines,type="bytes")==1346L),
          all(substr(lines,9,12)==as.character(y)))
        at<-which(substr(lines,75,76)=="45")
        if(length(at)) {
          p<-linked_parse_lines(lines[at],y,row$release_year,"denominator",sid)
          # Preserve the actual ordinal in the original full denominator, not
          # the ordinal in this age-filtered parsing batch.
          p$source_record_rows<-as.integer(n+at)
          p$values$record_source_id<-paste(sid,p$source_record_rows,sep=":row:")
          stopifnot(all(p$values$maternal_age==45),
            !any(unlist(p$raw_invalid)),all(linked_invalid_reporting_flags(p)==0))
          z<-linked_input_chunk(p,num,TRUE)
          stopifnot(all(unlist(z$join$semantic_clinical_disagreements)==0),
            !any(z$join$data$mortality_linkage_status%in%c("valid_key_without_numerator","invalid_denominator_link_key")))
          matched<-c(matched,z$join$matched_numerator_indices)
          pieces[[length(pieces)+1L]]<-z$data
        }
        n45<-n45+length(at);n<-n+length(lines)
      }
      close(con)
      stopifnot(n==expected,!anyDuplicated(matched))
      # Every recorded age45 numerator must occur in its authoritative
      # cohort-ready denominator once, including nonclinical records.
      expected_matches<-which(num$values$maternal_age==45)
      stopifnot(setequal(matched,expected_matches))
      d<-as.data.frame(rbindlist(pieces,use.names=TRUE))
      stopifnot(all(d$age==45),!anyDuplicated(d$source_row),
        !is.unsorted(d$source_row,strictly=TRUE))
      path<-file.path(out,paste0(y,"_clinical.rds"))
      obj<-list(data=d,metadata=list(complete_age45_denominator_scan=TRUE,
        full_numerator_index_sha256=ent$sha256,source_gate=gate))
      saveRDS(obj,path);stopifnot(identical(readRDS(path),obj))
      writej(list(status="complete_age45_linked_source_extension",year=y,
        source_n=n,source_age45_n=n45,clinical_age45_n=nrow(d),
        linked_deaths_in_all_age45_source=length(matched),
        full_numerator_search=TRUE,all_age45_numerator_links_reconciled=TRUE,
        archive_sha256=gate$archive_sha256,numerator_index_sha256=ent$sha256,
        output_path=path,output_sha256=sha(path)),rp)
      message("Linked ",y,": full source scan; age45 clinical=",nrow(d))
      rm(numobj,num,pieces,d,obj);gc(FALSE)
    }
  }
}
if(sys.nframe()==0L) {
  a<-commandArgs(TRUE);main(if(length(a))a[1]else"all")
}
