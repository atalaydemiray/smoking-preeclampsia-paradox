# Decode the annual natality public-use files into the clinical inputs the models read.
# Run from model-fitting/. Reads through natality; never downloads or edits sources.
# An annual receipt is written only after old-age reconciliation and readback.
main <- function(years = 2024:2014) {
  stopifnot(file.exists("R/02_natality_adapter.R"))
  for (p in c("natality", "data.table", "jsonlite", "digest"))
    stopifnot(requireNamespace(p, quietly = TRUE))
  package_version<-as.character(utils::packageVersion("natality"))
  stopifnot(package_version %in% c("0.4.0.9002","0.4.0.9003"))
  data.table::setDTthreads(1L)
  arrow::set_cpu_count(1L)
  for (f in c("01_measurement_helpers", "02_natality_adapter", "05_model_specification"))
    source(file.path("R", paste0(f, ".R")), local = environment())
  sha <- function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE)
  writej <- function(x, p) jsonlite::write_json(x, p, auto_unbox = TRUE,
    pretty = TRUE, null = "null", na = "null", digits = 16)
  out <- "derived/natality"
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  dictdir <- "outputs/package_validation"
  dir.create(dictdir, recursive = TRUE, showWarnings = FALSE)
  files <- c("scripts/01_package_natality.R",
    file.path("R",paste0(c("01_measurement_helpers","02_natality_adapter","05_model_specification"),".R")),
    list.files(system.file(package = "natality"), full.names = TRUE, recursive = TRUE))
  pins <- setNames(vapply(files, sha, ""), files)
  writej(list(status = "running", years = years, package = "natality",
    package_version = as.character(utils::packageVersion("natality")),
    no_downloads = TRUE, no_imputation = TRUE, code_sha256 = as.list(pins)),
    file.path(out, "plan.json"))
  for (year in years) {
    rp <- file.path(out, paste0(year, "_receipt.json"))
    if (file.exists(rp)) {
      prior <- jsonlite::fromJSON(rp)
      stopifnot(prior$status == "complete_package_import_old_age_reconciled")
      if(prior$package_provenance$package_version!=package_version) {
        parity<-jsonlite::fromJSON(file.path(dictdir,"version_9003_parity",paste0(year,"_receipt.json")))
        previous_data<-prior$outputs$sha256[grepl("clinical[.]rds$",prior$outputs$path)]
        stopifnot(package_version=="0.4.0.9003",parity$status=="PASS_all_clinical_fields_identical",
          parity$old_input_sha256==previous_data,
          parity$source_artifact_sha256==prior$package_provenance$artifact_sha256)
      }
      # Earlier receipts also pinned unrelated fitting modules. Only the
      # three sourced decoding modules and package files affect this import.
      for(f in intersect(names(prior$code_sha256),files)) {
        allowed <- sha(f)
        if(basename(f)=="01_package_natality.R")
          allowed<-c(allowed,sha(file.path("protocol","01_package_natality_before_provenance_audit.R")),
            sha(file.path("protocol","01_package_natality_before_dependency_scope.R")),
            sha(file.path("protocol","01_package_natality_before_frozen_9003.R")))
        stopifnot(prior$code_sha256[[f]]%in%allowed)
      }
      for (i in seq_len(nrow(prior$outputs)))
        stopifnot(sha(prior$outputs$path[i]) == prior$outputs$sha256[i])
      message(year, ": verified checkpoint reused"); next
    }
    beg <- proc.time()[["elapsed"]]
    message(year, ": package read starting")
    vars <- natality_selected_columns(year, TRUE)
    books <- lapply(vars, function(f) natality::natality_codebook(f, year)$years)
    book <- data.table::rbindlist(books, fill = TRUE)
    stopifnot(nrow(book) == length(vars), !anyDuplicated(tolower(book$source_field)))
    agebook <- book[tolower(source_field) == "mager"]
    stopifnot(nrow(agebook) == 1L, grepl("45[[:space:]]+45 years", agebook$rules_text))
    data.table::fwrite(book, file.path(dictdir, paste0(year, "_analysis_codebook.csv")))
    raw <- natality::read_natality(year, vars, population = "all_occurrence", download = FALSE)
    provenance <- attr(raw, "natality_provenance")
    stopifnot(provenance$package_version == package_version,
      grepl("artifact verified", provenance$source_status),
      nrow(raw) == provenance$input_rows, all(raw$year == year))
    names(raw) <- tolower(names(raw))
    raw$year <- NULL
    stopifnot(setequal(names(raw), vars), all(vapply(raw, is.character, TRUE)))
    # Original strings remain in the package artifact and source_* fields.
    # Existing decoders explicitly trim whitespace and parse numeric codes.
    rawage <- as.integer(trimws(raw$mager))
    olddir <- if (year <= 2015) "full_2014_2015_20260906_step1" else "full_2016_2024_20260906_step1"
    oldrp <- file.path("derived/natality_inputs", olddir, paste0(year, "_receipt.json"))
    oldreceipt <- jsonlite::fromJSON(oldrp)
    oldentry <- oldreceipt$outputs[basename(oldreceipt$outputs$path) == paste0(year, "_clinical.rds"), ]
    stopifnot(nrow(oldentry) == 1L, sha(oldentry$path) == oldentry$sha256)
    pieces <- ledgers <- list()
    for (start in seq.int(1L, nrow(raw), by = 100000L)) {
      ii <- start:min(nrow(raw), start + 99999L)
      p <- natality_input_chunk(raw[ii, , drop = FALSE], year, start - 1L)
      pieces[[length(pieces) + 1L]] <- p$data
      ledgers[[length(ledgers) + 1L]] <- p$ledger
    }
    nsource <- nrow(raw)
    source_age45 <- sum(rawage == 45)
    rm(raw, rawage); gc(FALSE)
    d <- data.table::rbindlist(pieces, use.names = TRUE)
    ledger <- data.table::rbindlist(ledgers)[, lapply(.SD, sum),
      by = c("year", "oe_threshold", "stage"),
      .SDcols = c("n_entering", "n_retained", "n_excluded")]
    rm(pieces, ledgers, p); gc(FALSE)
    old <- readRDS(oldentry$path)$data
    at <- which(d$age <= 44)
    stopifnot(length(at) == nrow(old),
      identical(as.numeric(d$source_row[at]), as.numeric(old$source_row)),
      !anyDuplicated(d$source_row), !is.unsorted(d$source_row, strictly = TRUE),
      all(d$age %in% 15:45))
    # Compare every derived clinical, exposure, baseline and status column.
    # Source formatting is intentionally not equated with NBER's imported types.
    fields <- setdiff(names(old), grep("^source_", names(old), value = TRUE))
    provenance_fields <- c(age_imputation_status="mage_impflg",
      plurality_imputation_status="imp_plur",mage_repflg_status="mage_repflg",
      compgst_imp_status="compgst_imp",obgest_flg_status="obgest_flg",lmpused_status="lmpused")
    checks <- lapply(fields, function(f) {
      a <- d[[f]][at]; b <- old[[f]]
      if (is.factor(a)) a <- as.character(a)
      if (is.factor(b)) b <- as.character(b)
      ok <- isTRUE(all.equal(a, b, tolerance = 0, check.attributes = FALSE))
      recovery <- FALSE;changed_n<-0L
      if(!ok && f%in%names(provenance_fields)) {
        changed<-which(a!=b)
        source<-paste0("source_",provenance_fields[[f]])
        expected<-if(f=="age_imputation_status")"not_imputed" else "raw_blank"
        recovery<-length(changed)>0 && all(b[changed]=="unknown_source_na") &&
          all(a[changed]==expected) && all(is.na(old[[source]][changed])) &&
          all(trimws(as.character(d[[source]][at[changed]]))=="")
        changed_n<-length(changed)
      }
      data.frame(year = year, field = f, n = length(a), identical_values = ok,
        accepted_source_blank_recovery=recovery,changed_provenance_n=changed_n,
        model_field_values_unchanged=ok||recovery)
    })
    checks <- do.call(rbind, checks)
    data.table::fwrite(checks, file.path(dictdir, paste0(year, "_legacy_reconciliation.csv")))
    if (!all(checks$model_field_values_unchanged))
      stop("Package versus frozen decoded values differ: ",
           paste(checks$field[!checks$model_field_values_unchanged], collapse = ", "))
    rm(old); gc(FALSE)
    path <- file.path(out, paste0(year, "_clinical.rds"))
    z <- list(data = d, metadata = list(year = year, age_domain = c(15, 45),
      package_provenance = provenance, source_record_order = "all_occurrence original order",
      prior_clinical_sha256 = oldentry$sha256))
    saveRDS(z, path, compress = "gzip")
    readback <- readRDS(path)
    # data.table's internal self-reference is an external pointer, not data.
    # Require bit-exact saved column vectors and exact metadata separately.
    stopifnot(identical(as.list(readback$data), as.list(z$data)),
      identical(readback$metadata, z$metadata))
    rm(readback); gc(FALSE)
    lp <- file.path(out, paste0(year, "_ledger.csv"))
    data.table::fwrite(ledger, lp)
    counts <- list(source_n = nsource, source_age45 = source_age45,
      clinical_n = nrow(d), old_clinical_n = length(at),
      added_age45_clinical_n = sum(d$age == 45),
      primary_n = sum(d$primary_member), primary_age45_n = sum(d$primary_member & d$age == 45),
      broader_n = sum(d$complementary_member),
      broader_age45_n = sum(d$complementary_member & d$age == 45))
    writej(list(status = "complete_package_import_old_age_reconciled",
      year = year, counts = counts, package_provenance = provenance,
      code_sha256 = as.list(pins), original_receipt_sha256 = sha(oldrp),
      all_old_derived_fields_identical = all(checks$identical_values),
      all_model_fields_identical = TRUE,
      recovered_provenance_fields = checks[checks$accepted_source_blank_recovery,],
      original_record_positions_identical = TRUE,
      outputs = data.frame(path = c(path, lp), sha256 = vapply(c(path, lp), sha, "")),
      elapsed_seconds = proc.time()[["elapsed"]] - beg), rp)
    message(year, ": PASS; added age-45 clinical=", counts$added_age45_clinical_n,
      "; primary=", counts$primary_age45_n, "; seconds=", round(proc.time()[["elapsed"]] - beg))
    rm(d, z, ledger); gc(FALSE)
  }
  stopifnot(identical(pins, setNames(vapply(files, sha, ""), files)))
  writej(list(status = "complete_requested_package_imports", years = years,
    no_imputation = TRUE, code_sha256 = as.list(pins)), file.path(out, "completion.json"))
  writeLines(capture.output(sessionInfo()), file.path(dictdir, "sessionInfo.txt"))
}
if (sys.nframe() == 0L) {
  args <- commandArgs(TRUE)
  years <- if (length(args)) as.integer(strsplit(args[1], ",", fixed = TRUE)[[1]]) else 2024:2014
  tryCatch(main(years), error = function(e) {
    message("STOPPED: ", conditionMessage(e)); quit(status = 1L)
  })
}
