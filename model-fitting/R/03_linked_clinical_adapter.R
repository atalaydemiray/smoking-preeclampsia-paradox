# Linked birth-cohort clinical parsing; no effect estimation or download actions.
# Denominator birth data are authoritative. RECWT is period-only, never used.

linked_clinical_fields <- function(source_year) {
  if (length(source_year) != 1L || !source_year %in% 2018:2024) stop("Unsupported source year.")
  pos <- list(DOB_YY=c(9,12), MAGEIMP=c(73,73), MAGEREP=c(74,74), MAGER=c(75,76),
    MBSTATE_REC=c(84,84), RESTATUS=c(104,104), MRACEIMP=c(111,111), F_MHISP=c(116,116),
    MRACEHISP=c(117,117), MEDUC=c(124,124), F_MEDUC=c(126,126),
    PRIORLIVE=c(171,172), PRIORDEAD=c(173,174), LBO_REC=c(179,179),
    CIG_0=c(253,254), CIG_1=c(255,256), F_CIGS_0=c(265,265), F_CIGS_1=c(266,266),
    F_M_HT=c(282,282), BMI=c(283,286), RF_PDIAB=c(313,313), RF_PHYPE=c(315,315),
    RF_GHYPE=c(316,316), RF_EHYPE=c(317,317), RF_PPB=c(318,318), F_RF_PDIAB=c(319,319),
    F_RF_PHYPE=c(321,321), F_RF_GHYPE=c(322,322), F_RF_EHYPE=c(323,323), F_RF_PPB=c(324,324),
    RF_INFT=c(325,325), RF_ART=c(327,327), F_RF_INFT=c(328,328), F_RF_ART=c(330,330),
    RF_CESAR=c(331,331), F_RF_CESAR=c(335,335), CO_SEQNUM=c(365,371), CO_YOD=c(372,375),
    DPLURAL=c(454,454), IMP_PLUR=c(456,456), COMPGST_IMP=c(487,487),
    COMBGST_IMP=c(488,488), OBGEST_FLG=c(489,489), COMBGEST=c(490,491),
    LMPUSED=c(498,498), OEGest_Comb=c(499,500), FLGND=c(1346,1346),
    AGED=c(1356,1358), AGER5=c(1359,1359), AGER22=c(1360,1361), RECWT=c(1377,1384))
  f <- data.frame(field=names(pos), start=vapply(pos, `[`, numeric(1), 1),
    end=vapply(pos, `[`, numeric(1), 2), source_label=names(pos), stringsAsFactors=FALSE)
  f$source_label[f$field == "CO_YOD"] <- if (source_year <= 2019) "CO_DODYY" else "CO_YOD"
  if (source_year >= 2019) f$source_label[f$field %in% c("AGED","AGER5","AGER22")] <- c("AGEDX","AGER5X","AGER22X")
  if (source_year <= 2019) f$source_label[f$field == "F_RF_INFT"] <- "FILLER30"
  f$availability <- ifelse(f$field == "F_RF_INFT" & source_year <= 2019, "layout_filler_reporting_unavailable", "documented_field")
  f$role <- ifelse(f$start > 1346, "numerator_only", "both")
  f
}

linked_guide_name <- function(year) sprintf("%02dPE%02dCO_linkedUG.pdf", year-2000, year-2001)

# Birth-year natality denominators and death-year linked numerators do not share
# the same 2019 topcode. See the linked-plurality reconciliation note of 6 September 2026.
linked_plurality_definition <- function(birth_year,source_year,role) {
  role<-match.arg(role,c("denominator","numerator"))
  if(length(birth_year)!=1L||!birth_year %in% 2018:2023||length(source_year)!=1L||
     !source_year %in% 2018:2024)stop("Unsupported plurality source definition.")
  if(role=="denominator"&&source_year!=birth_year+1L)stop("Denominator release must follow birth year.")
  if(role=="numerator"&&!source_year %in% c(birth_year,birth_year+1L))stop("Numerator death year outside cohort.")
  five<-if(role=="denominator")birth_year<=2019L else source_year<=2018L
  list(max_code=if(five)5L else 4L,topcode=if(five)"5=quintuplet_or_higher" else "4=quadruplet_or_higher",
    coding_year=if(role=="denominator")birth_year else source_year,
    coding_source=if(role=="denominator")"birth_year_natality" else "death_year_linked_numerator")
}

linked_verify_denominator_plurality_guide <- function(v2,birth_year) {
  definition<-linked_plurality_definition(birth_year,birth_year+1L,"denominator")
  path<-file.path(v2,"data/dictionaries",paste0("natality",birth_year,".pdf"))
  if(!file.exists(path))stop("Birth-year natality plurality guide absent.")
  txt<-system2("pdftotext",c("-layout",shQuote(path),"-"),stdout=TRUE)
  if(!is.null(attr(txt,"status")))stop("Natality plurality guide extraction failed.")
  pages<-strsplit(paste(txt,collapse="\n"),"\f",fixed=TRUE)[[1]]
  at<-grep("DPLURAL",pages,fixed=TRUE)
  if(length(at)!=1L||!grepl("454[[:space:]]+1[[:space:]]+DPLURAL",pages[[at]]))stop("Natality plurality position not uniquely verified.")
  required<-if(definition$max_code==5L)"5[[:space:]]+Quintuplet or higher" else "4[[:space:]]+Quadruplet or higher"
  if(!grepl(required,pages[[at]]))stop("Natality plurality topcode differs from explicit birth-year schema.")
  list(birth_year=birth_year,guide=path,sha256=digest::digest(file=path,algo="sha256"),
    pdf_page=at,definition=definition,
    documentation_note="2019 natality 1-5 versus linked numerator 1-4+ is confirmed by the 2019 forensic receipt; raw source values are never replaced.")
}

linked_verify_guide <- function(v2, source_year) {
  file <- file.path(v2,"data/dictionaries/linked_birth_infant_death",linked_guide_name(source_year))
  if (!file.exists(file)) stop("Annual linked guide absent.")
  text <- system2("pdftotext",c("-layout",shQuote(file),"-"),stdout=TRUE)
  if (!is.null(attr(text,"status"))) stop("Annual PDF text extraction failed.")
  pages <- strsplit(paste(text,collapse="\n"),"\f",fixed=TRUE)[[1]]
  fields <- linked_clinical_fields(source_year); evidence <- vector("list",nrow(fields))
  for (i in seq_len(nrow(fields))) {
    f <- fields[i,]; position <- if(f$start==f$end) as.character(f$start) else paste(f$start,f$end,sep="-")
    pattern <- paste0("(?m)^\\s*",position,"\\s+",f$end-f$start+1,"\\s+(?:P,G\\s+)?",f$source_label,"\\b[^\\n]*")
    matches <- unlist(lapply(pages,function(p) regmatches(p,gregexpr(pattern,p,perl=TRUE))),use.names=FALSE)
    if(length(matches)!=1L) stop("Annual guide field not uniquely verified: ",source_year," ",f$field)
    evidence[[i]] <- list(field=f$field,position=position,source_label=f$source_label,
      availability=f$availability,pdf_page=which(vapply(pages,grepl,logical(1),pattern=pattern,perl=TRUE)),line=matches)
  }
  joined <- paste(text,collapse=" ")
  if (!grepl("For cohort file use:.*do not apply",joined)) stop("Cohort no-weight instruction not found.")
  plurality_page<-pages[[which(vapply(pages,grepl,logical(1),pattern="DPLURAL",fixed=TRUE))]]
  plurality_topcode<-if(source_year<=2018L)"5[[:space:]]+Quintuplet or higher" else "4[[:space:]]+Quadruplet or higher"
  if(!grepl(plurality_topcode,plurality_page))stop("Linked numerator plurality topcode is not guide verified.")
  list(source_year=source_year,guide=file,sha256=digest::digest(file=file,algo="sha256"),
    official_url=paste0("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/DVS/period-cohort-linked/",basename(file)),
    field_evidence=evidence,cohort_period_weight_forbidden=TRUE,
    reporting_flag_definition=if(source_year<=2023)"1 reported both years; 0 not reported in either previous or current year" else "1 reported in birth year; 0 not reported in birth year",
    death_age_documented_max_days=if(source_year<=2020)365L else 364L,
    numerator_plurality_max_code=if(source_year<=2018L)5L else 4L,
    infertility_parent_flag=if(source_year<=2019)"328 printed as filler; no substitute flag assumed" else "dedicated 328, conflicting legacy prose319 retained as caveat")
}

linked_key <- function(sequence,death_year,birth_year,expected_death_year=NULL) {
  if(length(sequence)!=length(death_year)||length(birth_year)!=1L)stop("Linkage fields must have equal lengths and one cohort year.")
  sequence <- trimws(sequence); death_year <- trimws(death_year)
  blank_s <- is.na(sequence)|sequence==""; blank_y <- is.na(death_year)|death_year==""
  status <- rep("valid_nonmissing_key",length(sequence))
  status[blank_s & blank_y] <- "both_key_fields_missing"
  status[xor(blank_s,blank_y)] <- "partially_missing_key"
  syntax <- !blank_s & !blank_y & grepl("^[0-9]+$",sequence) & grepl("^[0-9]{4}$",death_year)
  status[!blank_s & !blank_y & !syntax] <- "unsupported_key_syntax"
  s <- suppressWarnings(as.integer(sequence)); y <- suppressWarnings(as.integer(death_year))
  status[syntax & s==0L] <- "zero_sequence_requires_dictionary_review"
  allowed <- if(is.null(expected_death_year)) c(birth_year,birth_year+1L) else expected_death_year
  status[syntax & s>0L & !y %in% allowed] <- "unexpected_death_year"
  key <- rep(NA_character_,length(sequence)); ok <- status=="valid_nonmissing_key"
  key[ok] <- paste(birth_year,y[ok],s[ok],sep=":")
  list(key=key,status=status)
}

linked_numeric <- function(raw,minimum,maximum,unknown=character(),flag=NULL,decimal=FALSE) {
  blank <- is.na(raw)|raw==""; value <- suppressWarnings(as.numeric(raw))
  syntax <- if(decimal) grepl("^[0-9]+[.][0-9]$",raw) else grepl("^[0-9]+$",raw)
  status <- rep("observed",length(raw)); status[blank] <- "unknown_blank"
  status[!blank & raw %in% unknown] <- "unknown_code"
  invalid <- !blank & !raw %in% unknown & (!syntax | is.na(value) | value<minimum | value>maximum)
  status[invalid] <- "invalid_code"
  if(!is.null(flag)) {
    status[is.na(flag)|flag==""] <- "unknown_reporting_status"
    status[!is.na(flag)&flag=="0"] <- "not_reported"
    status[!is.na(flag)&!flag %in% c("","0","1")] <- "invalid_reporting_flag"
  }
  value[status!="observed"] <- NA_real_
  list(value=value,status=status,raw_invalid=invalid)
}

linked_ynu <- function(raw,flag) {
  status <- rep("invalid_code",length(raw)); status[is.na(raw)|raw==""] <- "unknown_blank"
  status[raw %in% "U"] <- "unknown_codeU"; status[raw %in% c("Y","N")] <- "observed"
  invalid <- status=="invalid_code"
  status[is.na(flag)|flag==""] <- "unknown_reporting_status"
  status[flag %in% "0"] <- "not_reported"
  status[!is.na(flag)&!flag %in% c("","0","1")] <- "invalid_reporting_flag"
  value <- ifelse(raw=="Y",1L,0L); value[status!="observed"] <- NA_integer_
  list(value=value,status=status,raw_invalid=invalid)
}

linked_death_age <- function(raw,source_year) {
  days <- linked_numeric(raw$AGED,0,if(source_year<=2020) 365 else 364)
  five <- linked_numeric(raw$AGER5,1,5); twenty_two <- linked_numeric(raw$AGER22,1,22)
  by_days <- ifelse(is.na(days$value),NA_integer_,ifelse(days$value==0,0L,ifelse(days$value<7,3L,ifelse(days$value<28,4L,5L))))
  conflict <- !is.na(five$value)&!is.na(days$value)&
    ((days$value==0 & !five$value %in% 1:2) | (days$value>0 & five$value!=by_days))
  a22_group <- ifelse(twenty_two$value<=2,twenty_two$value,
    ifelse(twenty_two$value<=8,3L,ifelse(twenty_two$value<=11,4L,5L)))
  conflict22 <- !is.na(five$value)&!is.na(twenty_two$value)&five$value!=a22_group
  invalid <- days$raw_invalid | five$raw_invalid | twenty_two$raw_invalid
  usable <- !is.na(five$value)&!conflict&!conflict22&!invalid
  early <- neonatal <- rep(NA_integer_,nrow(raw)); early[usable] <- as.integer(five$value[usable]<=3)
  neonatal[usable] <- as.integer(five$value[usable]<=4)
  list(days=days,five=five,twenty_two=twenty_two,early=early,neonatal=neonatal,
    conflict=conflict|conflict22|invalid,classification_status=ifelse(invalid,"invalid_age_code",ifelse(conflict|conflict22,"age_recodes_disagree",
      ifelse(usable,"classified_from_AGER5","unknown_age_subtype"))),
    definition=if(source_year==2018) "mortality_certificate_age_2018" else "birth_datetime_revised_age_2019plus")
}

linked_parse_lines <- function(lines,birth_year,source_year,role,source_id,row_offset=0L,death_year=NULL) {
  role <- match.arg(role,c("denominator","numerator")); width <- if(role=="denominator") 1346L else 1743L
  if(role=="numerator" && (length(death_year)!=1L||source_year!=death_year))stop("Numerator source layout must match its explicit death-year member.")
  if(role=="denominator" && source_year!=birth_year+1L)stop("Only the next-release cohort-ready denominator layout is supported.")
  if(any(nchar(lines,type="bytes")!=width)) stop("Unexpected linked record byte width; no padding.")
  if(any(grepl("[^ -~]",lines))) stop("Non-ASCII printable bytes in linked payload.")
  fields <- linked_clinical_fields(source_year); fields <- fields[fields$end<=width,]
  raw <- as.data.frame(setNames(lapply(seq_len(nrow(fields)),function(i) trimws(substr(lines,fields$start[i],fields$end[i]))),fields$field),stringsAsFactors=FALSE)
  year <- suppressWarnings(as.integer(raw$DOB_YY)); allowed <- if(role=="denominator") birth_year else c(death_year-1L,death_year)
  if(anyNA(year)||any(!year %in% allowed)) stop("Unexpected source birth year.")
  selected <- year==birth_year; record_rows <- row_offset+which(selected); raw <- raw[selected,,drop=FALSE]
  n <- nrow(raw); key <- linked_key(raw$CO_SEQNUM,raw$CO_YOD,birth_year,if(role=="numerator") death_year else NULL)
  decoded <- list()
  numeric_field <- function(name,lo,hi,unknown=character(),flag=NULL,decimal=FALSE)
    linked_numeric(raw[[name]],lo,hi,unknown,if(is.null(flag))NULL else raw[[flag]],decimal)
  decoded$restatus <- numeric_field("RESTATUS",1,4)
  decoded$maternal_age <- numeric_field("MAGER",12,50)
  plurality_definition<-linked_plurality_definition(birth_year,source_year,role)
  decoded$plurality <- numeric_field("DPLURAL",1,plurality_definition$max_code)
  decoded$oe_week <- numeric_field("OEGest_Comb",17,47,"99")
  decoded$cig0_count_lowerbound <- numeric_field("CIG_0",0,98,"99","F_CIGS_0")
  decoded$cig1_count_lowerbound <- numeric_field("CIG_1",0,98,"99","F_CIGS_1")
  yn_fields <- list(chtn=c("RF_PHYPE","F_RF_PHYPE"),ghpe=c("RF_GHYPE","F_RF_GHYPE"),
    eclampsia=c("RF_EHYPE","F_RF_EHYPE"),prepregnancy_diabetes=c("RF_PDIAB","F_RF_PDIAB"),
    previous_preterm_birth=c("RF_PPB","F_RF_PPB"),previous_cesarean=c("RF_CESAR","F_RF_CESAR"))
  for(name in names(yn_fields)) { f<-yn_fields[[name]];decoded[[name]]<-linked_ynu(raw[[f[1]]],raw[[f[2]]]) }
  decoded$race_hispanic_origin <- numeric_field("MRACEHISP",1,7,"8","F_MHISP")
  decoded$education <- numeric_field("MEDUC",1,8,"9","F_MEDUC")
  decoded$nativity <- numeric_field("MBSTATE_REC",1,2,"3")
  decoded$prior_liveborn_children_now_living <- numeric_field("PRIORLIVE",0,30,"99")
  decoded$prior_liveborn_children_now_dead <- numeric_field("PRIORDEAD",0,30,"99")
  decoded$live_birth_order_recode <- numeric_field("LBO_REC",1,8,"9")
  decoded$prepregnancy_bmi <- numeric_field("BMI",13,69.9,"99.9","F_M_HT",TRUE)
  parent_flag <- if(source_year<=2019) rep(NA_character_,n) else raw$F_RF_INFT
  decoded$infertility_treatment <- linked_ynu(raw$RF_INFT,parent_flag)
  if(source_year<=2019) decoded$infertility_treatment$status[] <- "layout_filler_reporting_unavailable"
  art_value <- rep(NA_integer_,n); art_status <- rep("unknown_parent_or_reporting",n)
  usable_flags <- !is.na(parent_flag)&parent_flag=="1"&raw$F_RF_ART=="1"
  neg <- usable_flags & raw$RF_INFT=="N" & raw$RF_ART %in% c("N","X")
  yesparent <- usable_flags & raw$RF_INFT=="Y" & raw$RF_ART %in% c("N","Y")
  art_value[neg]<-0L;art_value[yesparent]<-as.integer(raw$RF_ART[yesparent]=="Y")
  art_status[neg|yesparent]<-"observed_parent_aware"
  conflict <- usable_flags & ((raw$RF_INFT=="N"&raw$RF_ART=="Y")|(raw$RF_INFT=="Y"&raw$RF_ART=="X"))
  art_status[conflict]<-"contradictory_parent_child"
  art_status[parent_flag %in% "0" | raw$F_RF_ART %in% "0"]<-"not_reported"
  decoded$art_parent_aware<-list(value=art_value,status=art_status,raw_invalid=!raw$RF_ART %in% c("","Y","N","U","X"))
  values <- as.data.frame(setNames(lapply(decoded,`[[`,"value"),names(decoded)),stringsAsFactors=FALSE)
  status <- as.data.frame(setNames(lapply(decoded,`[[`,"status"),names(decoded)),stringsAsFactors=FALSE)
  c0<-values$cig0_count_lowerbound;c1<-values$cig1_count_lowerbound
  values$early_pattern <- ifelse(is.na(c0)|is.na(c1),"incomplete",ifelse(c0==0,
    ifelse(c1==0,"no_early_smoking","t1_initiation"),ifelse(c1==0,"prepregnancy_only","continued")))
  values$cig0_topcoded98 <- !is.na(c0)&c0==98; values$cig1_topcoded98<-!is.na(c1)&c1==98
  values$primary_A <- ifelse(!is.na(c0)&c0>0&!is.na(c1),as.integer(c1>0),NA_integer_)
  values$birth_year<-rep(birth_year,n); values$birth_record_weight<-rep(1,n)
  values$record_source_id<-if(n)paste(source_id,record_rows,sep=":row:") else character()
  values$link_key<-key$key;values$link_key_status<-key$status
  values$race_imputation_raw<-raw$MRACEIMP;values$age_imputation_raw<-raw$MAGEIMP
  values$plurality_imputation_raw<-raw$IMP_PLUR
  values$combined_gestation_imputation_raw<-raw$COMBGST_IMP
  values$obstetric_estimate_used_for_combined_gestation_raw<-raw$OBGEST_FLG
  # These provenance flags are not proof of an originally observed edited OE.
  mortality <- if(role=="numerator") linked_death_age(raw,source_year) else NULL
  list(raw=raw,values=values,status=status,raw_invalid=lapply(decoded,`[[`,"raw_invalid"),
    mortality=mortality,role=role,birth_year=birth_year,source_year=source_year,plurality_definition=plurality_definition,
    source_rows_read=length(lines),other_birth_year_rows=sum(!selected),source_record_rows=record_rows)
}

linked_clinical_masks <- function(x,minimum_oe=20L) {
  if(!minimum_oe %in% c(20L,28L)) stop("Only prespecified 20/28-week thresholds are supported.")
  v<-x$values
  base<-v$restatus %in% 1:3 & v$plurality %in% 1 & v$maternal_age %in% 15:45 & !is.na(v$oe_week)&v$oe_week>=minimum_oe
  chronic<-base & v$chtn %in% 0
  early<-chronic & v$early_pattern!="incomplete"
  list(source_records=rep(TRUE,nrow(v)),us_residents=v$restatus %in% 1:3,analytic_base=base,
    reported_chtn_no=chronic,early_known=early,primary_smoker_mortality=early&!is.na(v$primary_A),
    gh_known=early&!is.na(v$ghpe),primary_smoker_gh_known=early&!is.na(v$primary_A)&!is.na(v$ghpe))
}

linked_bind_parsed <- function(parts) {
  if(!length(parts)) stop("No numerator chunks selected.")
  list(raw=do.call(rbind,lapply(parts,`[[`,"raw")),values=do.call(rbind,lapply(parts,`[[`,"values")),
    mortality=list(early=unlist(lapply(parts,function(p)p$mortality$early)),
      neonatal=unlist(lapply(parts,function(p)p$mortality$neonatal)),
      days=unlist(lapply(parts,function(p)p$mortality$days$value)),
      status=unlist(lapply(parts,function(p)p$mortality$classification_status)),
      definition=unlist(lapply(parts,function(p)rep(p$mortality$definition,nrow(p$values)))),
      source_year=unlist(lapply(parts,function(p)rep(p$source_year,nrow(p$values))))))
}

linked_plurality_reconcile <- function(denominator_raw,numerator_raw,birth_year,numerator_death_year) {
  n<-length(denominator_raw)
  if(length(numerator_raw)!=n||length(numerator_death_year)!=n||
     anyNA(numerator_death_year)||any(!numerator_death_year %in% c(birth_year,birth_year+1L)))
    stop("Plurality pair/source-year lengths or values invalid.")
  den_definition<-linked_plurality_definition(birth_year,birth_year+1L,"denominator")
  num_max<-ifelse(numerator_death_year<=2018L,5L,4L)
  den_valid<-denominator_raw %in% as.character(seq_len(den_definition$max_code))
  num_valid<-numerator_raw %in% as.character(1:5)&suppressWarnings(as.integer(numerator_raw))<=num_max
  num_valid[is.na(num_valid)]<-FALSE
  raw_equal<-!is.na(denominator_raw)&!is.na(numerator_raw)&denominator_raw==numerator_raw
  allowed<-den_valid&num_valid&den_definition$max_code==5L&num_max==4L&
    denominator_raw %in% "5"&numerator_raw %in% "4"
  equivalent<-den_valid&num_valid&(raw_equal|allowed)
  counts<-c(matched_pairs=n,raw_disagreements=sum(!raw_equal),
    documented_5_to_4plus_pairs=sum(allowed),semantic_disagreements=sum(!equivalent),
    singleton_indicator_disagreements=sum((denominator_raw %in% "1")!=(numerator_raw %in% "1")),
    invalid_or_unknown_pair_values=sum(!den_valid|!num_valid))
  # No blanket pmin(x,4): 4 versus5 must fail when BOTH members distinguish them.
  list(counts=counts,equivalent=equivalent,documented_coarsening=allowed,
    raw_pair_counts=table(paste0("den",denominator_raw,"_num",numerator_raw,"_death",numerator_death_year)))
}

linked_join_chunk <- function(den,num,numerator_search_complete=TRUE) {
  dk<-den$values$link_key;nk<-num$values$link_key
  if(anyNA(nk)||anyDuplicated(nk)) stop("Numerator keys missing, invalid or duplicated; no ambiguous join.")
  if(anyDuplicated(dk[!is.na(dk)])) stop("Duplicated denominator keys within chunk.")
  at<-match(dk,nk);at[is.na(dk)]<-NA_integer_;matched<-!is.na(at)
  status<-rep(if(numerator_search_complete)"no_linked_death_observed" else "numerator_search_incomplete",length(dk))
  invalid<-!den$values$link_key_status %in% c("valid_nonmissing_key","both_key_fields_missing")
  status[invalid]<-"invalid_denominator_link_key"
  status[!is.na(dk)&!matched]<-if(numerator_search_complete)"valid_key_without_numerator" else "numerator_search_incomplete"
  status[matched]<-"linked_infant_death_observed"
  observed<-rep(NA_integer_,length(dk));observed[matched]<-1L
  observed[status=="no_linked_death_observed"]<-0L
  early<-neonatal<-rep(NA_integer_,length(dk));early[matched]<-num$mortality$early[at[matched]]
  neonatal[matched]<-num$mortality$neonatal[at[matched]]
  early[status=="no_linked_death_observed"]<-0L;neonatal[status=="no_linked_death_observed"]<-0L
  out<-den$values;out$mortality_linkage_status<-status;out$linked_infant_death_observed<-observed
  out$linked_early_neonatal_death_observed<-early;out$linked_neonatal_death_observed<-neonatal
  out$observed_linked_death_weight<-ifelse(matched,1,NA_real_)
  out$death_age_days<-rep(NA_real_,length(dk));out$death_age_days[matched]<-num$mortality$days[at[matched]]
  out$death_age_status<-rep("no_linked_death_age",length(dk));out$death_age_status[matched]<-num$mortality$status[at[matched]]
  out$true_survival_status<-ifelse(matched,"death_observed","not_established_from_linkage")
  out$death_age_definition<-rep(NA_character_,length(dk))
  out$death_age_definition[matched]<-num$mortality$definition[at[matched]]
  out$death_source_year<-rep(NA_integer_,length(dk))
  out$death_source_year[matched]<-num$mortality$source_year[at[matched]]
  out$numerator_record_source_id<-rep(NA_character_,length(dk))
  out$numerator_record_source_id[matched]<-num$values$record_source_id[at[matched]]
  for(f in c("AGED","AGER5","AGER22")) {
    x<-rep(NA_character_,length(dk));x[matched]<-num$raw[[f]][at[matched]]
    out[[paste0("source_death_",tolower(f))]]<-x
  }
  # Compare core duplicated birth fields; never overwrite denominator values.
  compare<-c("DOB_YY","RESTATUS","MAGER","OEGest_Comb","DPLURAL","RF_PHYPE","F_RF_PHYPE",
    "RF_GHYPE","F_RF_GHYPE","CIG_0","F_CIGS_0","CIG_1","F_CIGS_1")
  disagreements<-setNames(vapply(compare,function(f)sum(den$raw[[f]][matched]!=num$raw[[f]][at[matched]]),numeric(1)),compare)
  plurality<-linked_plurality_reconcile(den$raw$DPLURAL[matched],num$raw$DPLURAL[at[matched]],
    den$birth_year,num$mortality$source_year[at[matched]])
  semantic_disagreements<-disagreements
  semantic_disagreements[["DPLURAL"]]<-unname(plurality$counts[["semantic_disagreements"]])
  list(data=out,matched_numerator_indices=at[matched],clinical_disagreements=disagreements,
    semantic_clinical_disagreements=semantic_disagreements,plurality_reconciliation=plurality,
    period_RECWT_used=FALSE,missing_to_missing_matches=0L)
}

linked_stream_member <- function(archive,member,birth_year,source_year,role,callback,death_year=NULL,
                                  chunk_size=10000L,max_records=Inf) {
  if(!file.exists(archive)||endsWith(archive,".part")||nzchar(Sys.readlink(archive))) stop("Complete regular ZIP required; partials never read.")
  if(chunk_size<1L||chunk_size>25000L||max_records<1) stop("Invalid bounded streaming parameters.")
  inventory<-utils::unzip(archive,list=TRUE)
  if(sum(inventory$Name==member)!=1L) stop("Exact member absent/duplicated; no filename fallback.")
  con<-unz(archive,member,open="rb");on.exit(close(con),add=TRUE)
  total<-selected<-0L;exhausted<-FALSE
  repeat {
    ask<-min(chunk_size,max_records-total)
    if(ask<=0)break
    lines<-readLines(con,n=ask,warn=FALSE,encoding="ASCII")
    if(!length(lines)){exhausted<-TRUE;break}
    p<-linked_parse_lines(lines,birth_year,source_year,role,paste(basename(archive),member,sep="/"),total,death_year)
    total<-total+length(lines);selected<-selected+nrow(p$values)
    if(nrow(p$values))callback(p)
    if(length(lines)<ask){exhausted<-TRUE;break}
  }
  width<-if(role=="denominator")1346L else 1743L
  expected<-inventory$Length[inventory$Name==member]/(width+2L)
  if(exhausted && total!=expected)stop("Stream count differs from pinned member length/CRLF structure.")
  list(member=member,role=role,source_year=source_year,source_records_read=total,target_birth_year_records=selected,
    expected_full_records=expected,entire_member_read=exhausted,partial_prefix_is_not_representative=!exhausted,
    fresh_CRC_claim=FALSE,CRC_note="CRC/CRLF authority is prior full structural receipt at matching ZIP hash, not this R text reader.")
}

linked_cohort_config <- function(v2,birth_year) {
  config<-jsonlite::fromJSON(file.path(v2,"config/linked_cohort_schema.json"),simplifyVector=FALSE)
  rows<-Filter(function(z)z$birth_year==birth_year,config$cohorts)
  if(length(rows)!=1L)stop("Cohort not uniquely pinned in existing schema.")
  rows[[1]]
}

linked_source_gate <- function(v2,row,verify_archive_hash=FALSE) {
  archive<-file.path(v2,"data/raw/linked_birth_infant_death",row$archive)
  receipt_path<-file.path(v2,"outputs/qa",paste0("linked_cohort_",row$birth_year,"_feasibility.json"))
  if(!file.exists(archive))return(list(ready=FALSE,reason="complete_archive_missing",archive=archive))
  if(!file.exists(receipt_path))return(list(ready=FALSE,reason="structural_receipt_missing",archive=archive))
  r<-jsonlite::fromJSON(receipt_path,simplifyVector=FALSE)
  checks<-unlist(r$checks)
  if(!all(checks)||!startsWith(r$status,"structural_linkage_passed"))return(list(ready=FALSE,reason="structural_checks_not_passed",archive=archive))
  exact<-c(row$denominator,unlist(row$numerators,use.names=FALSE))
  pinned<-vapply(r$selected_members,`[[`,character(1),"name")
  if(!identical(exact,pinned))stop("Clinical member selection differs from structural receipt.")
  if(file.info(archive)$size!=r$archive$file_bytes)stop("Archive size differs from structural receipt.")
  if(digest::digest(file=file.path(v2,"config/linked_cohort_schema.json"),algo="sha256")!=r$schema$sha256)
    stop("Member schema changed since structural reconciliation.")
  guide<-file.path(v2,"data/dictionaries/linked_birth_infant_death",row$guide)
  if(digest::digest(file=guide,algo="sha256")!=r$guide$sha256)stop("Guide changed since structural reconciliation.")
  if(verify_archive_hash && digest::digest(file=archive,algo="sha256")!=r$archive$sha256)stop("Archive hash differs from structural receipt.")
  list(ready=TRUE,archive=archive,structural_receipt=receipt_path,archive_sha256=r$archive$sha256,
    archive_hash_rechecked=verify_archive_hash,archive_bytes=file.info(archive)$size,
    archive_mtime_numeric=as.numeric(file.info(archive)$mtime),documentation_caveat=row$documentation_caveat)
}

# Harmonized clinical input, distinct from the parser's native semantic frame.
# Source R/01_measurement_helpers.R first for identical early-smoking coding.
linked_input_chunk <- function(den,num,numerator_search_complete=TRUE) {
  if(!exists("classify_early_smoking",mode="function"))stop("Source measurement helpers first.")
  j<-linked_join_chunk(den,num,numerator_search_complete);v<-j$data;n<-nrow(v)
  normalized_flag<-function(x)ifelse(x %in% "1","reported",ifelse(x %in% "0","not_reported","unknown"))
  early<-classify_early_smoking(den$raw$CIG_0,den$raw$CIG_1,
    normalized_flag(den$raw$F_CIGS_0),normalized_flag(den$raw$F_CIGS_1))
  masks<-linked_clinical_masks(den,20);keep<-masks$reported_chtn_no
  if(!identical(as.integer(early$within_prepregnancy_smokers),as.integer(v$primary_A)))
    stop("Common and linked exposure definitions disagree.")
  broad<-masks$gh_known;primary<-masks$primary_smoker_gh_known
  d<-data.frame(source_row=as.integer(den$source_record_rows),year=rep(as.integer(den$birth_year),n),
    source_type=rep("linked_birth_cohort",n),age=v$maternal_age,oe_weeks=v$oe_week,gh=v$ghpe,
    gh_status=den$status$ghpe,chtn_status=den$status$chtn,early_pattern=early$pattern,
    early_status=early$classification_status,clinical_member=keep,primary_member=primary,
    broad_member=broad,complementary_member=broad&early$pattern %in% c("no_reported_pre_or_t1_smoking","pre_and_t1_smoking"),
    mortality_member=masks$early_known,primary_mortality_member=masks$primary_smoker_mortality,
    primary_t1_smoking=early$within_prepregnancy_smokers,
    oe28_member=linked_clinical_masks(den,28)$reported_chtn_no,
    stringsAsFactors=FALSE)
  for(f in names(early)[grepl("^(pre_|t1_)",names(early))])d[[f]]<-early[[f]]
  baselines<-c("maternal_age","race_hispanic_origin","education","nativity","live_birth_order_recode",
    "prior_liveborn_children_now_living","prior_liveborn_children_now_dead","prepregnancy_bmi",
    "prepregnancy_diabetes","previous_preterm_birth","previous_cesarean","infertility_treatment","art_parent_aware","eclampsia")
  for(f in baselines){d[[f]]<-v[[f]];d[[paste0(f,"_status")]]<-den$status[[f]]}
  # The meanings of linked source imputation flags are not copied from fetal/natality.
  # Raw-coded statuses intentionally await cross-source sensitivity harmonization.
  provenance<-c(age_imputation_status="MAGEIMP",mage_repflg_status="MAGEREP",
    plurality_imputation_status="IMP_PLUR",race_imputation_status="MRACEIMP",
    compgst_imp_status="COMPGST_IMP",combgest_imp_status="COMBGST_IMP",
    obgest_flg_status="OBGEST_FLG",lmpused_status="LMPUSED")
  for(f in names(provenance)) {
    raw<-den$raw[[provenance[[f]]]]
    d[[f]]<-ifelse(is.na(raw),"source_missing",ifelse(raw=="","raw_blank",paste0("raw_code",raw)))
  }
  # Retain only guide-unambiguous race-imputation semantics; unknown codes stay NA.
  d$race_any_imputed<-ifelse(den$raw$MRACEIMP %in% "",FALSE,
    ifelse(den$raw$MRACEIMP %in% c("1","2"),TRUE,NA))
  mortality_fields<-c("record_source_id","link_key","link_key_status","mortality_linkage_status",
    "linked_infant_death_observed","linked_early_neonatal_death_observed","linked_neonatal_death_observed",
    "birth_record_weight","observed_linked_death_weight","death_age_days","death_age_status",
    "true_survival_status","death_age_definition","death_source_year","numerator_record_source_id",
    "source_death_aged","source_death_ager5","source_death_ager22")
  for(f in mortality_fields)d[[f]]<-v[[f]]
  for(f in names(den$raw))d[[paste0("source_",tolower(f))]]<-den$raw[[f]]
  stages<-list(source_records=rep(TRUE,n),us_residents=v$restatus %in% 1:3)
  stages$singleton<-stages$us_residents&v$plurality %in% 1
  stages$maternal_age_15_45<-stages$singleton&v$maternal_age %in% 15:45
  stages$known_oe_at_least20<-stages$maternal_age_15_45&!is.na(v$oe_week)&v$oe_week>=20
  stages$known_no_prepregnancy_hypertension<-keep
  stages$known_early_pattern<-masks$early_known
  stages$known_recorded_gh_pe<-broad
  stages$primary_within_prepregnancy_smokers<-primary
  counts<-vapply(stages,sum,numeric(1));previous<-c(n,head(counts,-1))
  ledger<-data.frame(year=den$birth_year,stage=names(counts),n_entering=unname(previous),
    n_retained=unname(counts),n_excluded=unname(previous-counts),stringsAsFactors=FALSE)
  d<-d[keep,,drop=FALSE]
  # Factorize low-cardinality strings only. Unique source IDs/keys remain strings.
  for(f in names(d))if(is.character(d[[f]])&&!f %in% c("record_source_id","numerator_record_source_id","link_key"))
    d[[f]]<-factor(d[[f]])
  rownames(d)<-NULL
  list(data=d,ledger=ledger,join=j,source_n=n,resident_n=sum(stages$us_residents),
    raw_invalid_counts=vapply(den$raw_invalid,sum,numeric(1)))
}

linked_input_schema <- function(d) {
  data.frame(field=names(d),type=vapply(d,function(x)paste(class(x),collapse="/"),""),stringsAsFactors=FALSE)
}

linked_invalid_reporting_flags <- function(parsed) {
  fields<-linked_clinical_fields(parsed$source_year)
  selected<-fields$field[grepl("^F_",fields$field)&fields$availability=="documented_field"]
  setNames(vapply(parsed$raw[selected],function(x)sum(!is.na(x)&!x %in% c("","0","1")),numeric(1)),selected)
}

linked_save_input_rds <- function(data,metadata,path) {
  if(file.exists(path))stop("Refusing to overwrite derived input.")
  # Persist a plain frame: data.table's process-local external pointer is not
  # serializable identity and must not make a byte-stable value check spurious.
  data<-as.data.frame(data);rownames(data)<-NULL
  if(anyDuplicated(names(data))||anyDuplicated(data$source_row)||
    (nrow(data)>1L&&is.unsorted(data$source_row,strictly=TRUE)))stop("Output row/schema integrity failure.")
  schema<-linked_input_schema(data);schema_hash<-digest::digest(schema,algo="sha256")
  metadata$output_n<-nrow(data);metadata$output_schema_sha256<-schema_hash
  object<-list(data=data,metadata=metadata)
  saveRDS(object,path,compress="gzip",version=3)
  back<-readRDS(path)
  if(!identical(object,back))stop("Saved RDS failed exact read-back validation.")
  list(path=path,bytes=unname(file.info(path)$size),sha256=digest::digest(file=path,algo="sha256"),
    rows=nrow(data),columns=ncol(data),schema=schema,schema_sha256=schema_hash,
    exact_readback_identical=TRUE,full_source=isTRUE(metadata$full_source),output_population=metadata$output_population)
}
