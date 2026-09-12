# Driver for the fitting pipeline. Sourcing this file runs nothing; call
# run_model_fitting() to run every stage in order, or name the stages you want.
#
# Set the working directory to model-fitting/ first. Every path in this
# directory is relative to it.
#
# The stages, in order, with what each one needs and roughly how long it takes
# on one core of a recent laptop:
#
#   tests      the natality package installed, and its source tree at
#              vendor/natality (or NATALITY_PACKAGE_SOURCE). A few minutes.
#
#   imports    the public-use source files under data/raw, the dictionaries
#              under data/dictionaries, the schemas in config/, and the frozen
#              decoded checkpoints under derived/natality_inputs,
#              derived/fetal_inputs and derived/linked_inputs that the old-age
#              reconciliation compares against. Natality takes about 17 minutes
#              for the eleven years; the linked files are read with a full
#              denominator scan and take considerably longer.
#
#   prepare    completed imports in derived/, plus the pooled linked-mortality
#              baseline in derived/model_inputs. Roughly an hour, most of it in
#              the two thirty-million-record contrasts.
#
#   fits       prepared inputs in derived/. About two hours for all 36 models.
#              The two large combined fits are about 38 minutes each, which is
#              why they run one at a time.
#
#   validate   completed main fits in outputs/models and the prepared inputs
#              they were fitted on. It rebuilds each covariance matrix from
#              scratch, so it costs about as much as the fit did.
#
#   describe   prepared inputs and the annual ledgers in derived/natality.
#              Minutes.
#
#   aggregate  every completed fit, plus the previous submission's aggregate
#              estimates in reference/. Under a minute.
#
# The supplementary fits are separate and are not stages here. Run
# scripts/20, 21, 22 and 24 in that order once the main fits are complete.
# The 31 supplementary fits take about three hours in total.
#
# Every stage is restartable. Each script verifies the receipt and the hash of
# any completed work it finds and reuses it rather than refitting.

STAGES <- c("tests", "imports", "prepare", "fits", "validate", "describe", "aggregate")

run_model_fitting <- function(stages = STAGES) {
  stages <- match.arg(stages, STAGES, several.ok = TRUE)
  stages <- STAGES[STAGES %in% stages]
  if (!file.exists("scripts/01_package_natality.R"))
    stop("Run this from model-fitting/, the directory that contains run.R and scripts/.")

  # Each script is run in its own environment and must offer main() explicitly,
  # so that sourcing one never leaves objects behind for the next.
  call <- function(file, ...) {
    e <- new.env(parent = globalenv())
    sys.source(file, e)
    if (!exists("main", e, inherits = FALSE)) stop("No explicit main entry point: ", file)
    do.call(e$main, list(...))
  }
  s <- function(name) file.path("scripts", name)

  for (stage in stages) {
    message("stage: ", stage)
    switch(stage,
      tests = call(s("07_package_and_age_tests.R")),
      imports = {
        call(s("01_package_natality.R"))
        call(s("04_adjunct_sources.R"))
      },
      prepare = {
        call(s("02_prepare_natality.R"))
        call(s("05_prepare_adjunct_models.R"))
      },
      fits = {
        call(s("03_fit_natality.R"), "primary")
        call(s("03_fit_natality.R"), "historical")
        call(s("06_fit_adjunct_models.R"), "all")
        # The large comparisons are deliberately sequential to stay within memory.
        call(s("12_large_cohort_refits.R"), "broad")
        call(s("12_large_cohort_refits.R"), "prepregnancy")
      },
      validate = for (ct in c("primary", "broad", "prepregnancy"))
        call(s("09_independent_numerical_validation.R"), ct),
      describe = call(s("10_descriptive_tables.R")),
      aggregate = call(s("08_assemble_results.R")))
  }
  invisible(stages)
}
