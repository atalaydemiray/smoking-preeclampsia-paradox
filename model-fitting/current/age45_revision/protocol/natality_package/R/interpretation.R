.nat_warn_interpretation <- function(year, fields) {
  fields <- toupper(fields)
  notes <- character()
  if ("F_MAR_P" %in% fields) {
    notes <- c(notes, "F_MAR_P reports paternity-acknowledgment coverage; it is not a validated marital-status reporting flag. Do not use it as a substitute for DMAR availability.")
  }
  if (year >= 2017L && "DMAR" %in% fields) {
    notes <- c(notes, "From 2017, NCHS withholds record-level maternal marital status for births occurring in or to residents of California. A known-DMAR subset is not the full U.S. birth population; retain blanks and document exclusions.")
  }
  if (length(notes)) warning(paste(notes, collapse = " "), call. = FALSE)
  notes
}
