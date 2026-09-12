# Rebuilds every table and figure in the manuscript.
#
#   Rscript run_all.R
#
# Reads the aggregate estimates in results/ and writes output/tables and output/figures. It needs no
# source data files, no credentials and no network, and uses only packages that ship with R. About a
# second on a laptop.
#
# The estimates it reads were produced by the code in model-fitting/, which does need the source
# files. model-fitting/README.md explains that path.
#
# Reproduction is a claim, so this script checks it. After writing, it confirms that every expected
# file exists and that every table still matches the version published with the manuscript, using the
# checksums in results/exhibit_checksums.csv. If a change moves a number, this stops.

options(warn = 1)
start <- Sys.time()

if (!file.exists("run_all.R"))
  stop("Run this from the repository root, the directory that contains run_all.R.")

for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
unlink(list.files("output/tables", full.names = TRUE))
unlink(list.files("output/figures", full.names = TRUE))

# ---------------------------------------------------------------------- tables
# One function per table, in the order the paper and its supplement present them.
tables <- list(
  Table_1   = table_1,    Table_2   = table_2,    Table_3   = table_3,
  Table_S1  = table_s1,   Table_S2  = table_s2,   Table_S3  = table_s3,
  Table_S4a = table_s4a,  Table_S4b = table_s4b,  Table_S4c = table_s4c,
  Table_S5  = table_s5,   Table_S6a = table_s6a,  Table_S6b = table_s6b,
  Table_S7  = table_s7,   Table_S8  = table_s8,   Table_S9  = table_s9,
  Table_S10 = table_s10,  Table_S11 = table_s11,  Table_S12 = table_s12,
  Table_S13 = table_s13,  Table_S14 = table_s14,  Table_S15 = table_s15,
  Table_S16 = table_s16,  Table_S17 = table_s17,  Table_S18 = table_s18,
  Table_S19 = table_s19,  Table_S20 = table_s20,  Table_S21 = table_s21,
  Table_S22 = table_s22)

for (name in names(tables)) {
  rows <- tables[[name]]()
  check_people_first(rows, name)
  write_table_csv(rows, name)
}

# ---------------------------------------------------------------------- figures
figures <- list(
  Figure_1_study_overview              = figure_1,
  Figure_2_age_specific_risk_RR_RD     = figure_2,
  Figure_3_main_crossover              = figure_3,
  Figure_S1_interaction_sensitivity    = figure_s1,
  Figure_S2_left_truncation            = figure_s2,
  Figure_S3_three_level_early_smoking  = figure_s3,
  Figure_S4_dose_response              = figure_s4,
  Figure_S5_control_outcome_and_strata = figure_s5)

for (draw in figures) draw()

# ---------------------------------------------------------------------- checks
missing <- c(file.path("output/tables", paste0(names(tables), ".csv")),
             file.path("output/figures", paste0(names(figures), ".pdf")))
missing <- missing[!file.exists(missing) | file.size(missing) == 0]
if (length(missing))
  stop("run_all.R did not produce:\n  ", paste(missing, collapse = "\n  "))

# The checksum is over the parsed cells with whitespace collapsed, not the raw bytes, so quoting and
# line endings do not matter while any change to a value, a label or the shape of a table does.
normalised_md5 <- function(path) {
  rows <- utils::read.csv(path, header = FALSE, colClasses = "character",
                          check.names = FALSE, stringsAsFactors = FALSE)
  cells <- trimws(gsub("[[:space:]]+", " ", as.matrix(rows)))
  tmp <- tempfile(); on.exit(unlink(tmp), add = TRUE)
  writeLines(c(paste(dim(cells), collapse = "x"), as.vector(t(cells))), tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
}
expected <- utils::read.csv("results/exhibit_checksums.csv", stringsAsFactors = FALSE)
changed <- expected$table[!vapply(seq_len(nrow(expected)), function(i)
  identical(normalised_md5(file.path("output/tables", paste0(expected$table[i], ".csv"))),
            expected$checksum[i]), logical(1))]
if (length(changed))
  stop("These tables no longer match the version published with the manuscript:\n  ",
       paste(changed, collapse = ", "))

writeLines(c(
  paste("Rebuilt", length(tables), "tables and", length(figures), "figures."),
  paste("All", nrow(expected), "tables match the version published with the manuscript."),
  paste("Elapsed:", format(round(difftime(Sys.time(), start, units = "secs")))),
  R.version.string, paste("Platform:", R.version$platform)), "output/session_info.txt")
cat(readLines("output/session_info.txt"), sep = "\n")
cat("\nTables: output/tables\nFigures: output/figures\n")
