# Aggregate reproduction only: no records or statistical fitting are needed.
# From the package root: Rscript --vanilla Code/reproduce_figures.R [output-directory]
reproduce_figures <- function(root = ".", output = "reproduced_outputs/Figures") {
  root <- normalizePath(root, mustWork = TRUE)
  out <- if (grepl("^/", output)) output else file.path(root, output)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  inputs <- file.path(root, "Code", "aggregate_inputs")
  sup <- file.path(root, "Code", "supplementary", "aggregate_outputs")
  scripts <- file.path(root, "Code", "publication_code")
  work <- tempfile("natality_publication_"); dir.create(work)
  mk <- function(...) { p <- file.path(work, ...); dir.create(p, recursive = TRUE, showWarnings = FALSE); p }
  put <- function(src, dst) { stopifnot(file.exists(src)); stopifnot(file.copy(src, dst, overwrite = TRUE)) }
  stg <- mk("age45_revision", "submission_staging")
  ai <- mk("age45_revision", "submission_staging", "Code", "aggregate_inputs")
  for (f in c("main_overall.csv", "interaction_curves.csv")) put(file.path(inputs, f), ai)
  tt <- mk("age45_revision", "submission_staging", "Tables")
  for (f in c("Crossover_estimates.csv", "supplement_common_clinical_flow_by_year.csv")) put(file.path(inputs, f), tt)
  mm <- mk("age45_revision", "outputs", "models", "prepregnancy_main")
  for (f in c("HC0_age_standardized.csv", "receipt.json", "age_arm_support.csv"))
    put(file.path(sup, "main_models", paste0("prepregnancy_main_", f)), file.path(mm, f))
  rep <- mk("age45_revision", "outputs", "supplementary", "report")
  for (f in c("supp_summary.csv", "supp_age_estimates.csv", "supp_roots.csv")) put(file.path(sup, f), rep)
  jt <- file.path(sup, "joint_three_group")
  joint <- mk("age45_revision", "outputs", "supplementary", "joint_3group")
  js <- mk("age45_revision", "outputs", "supplementary", "joint_3group", "summary")
  for (f in c("main_summary.csv", "main_age_estimates.csv", "three_group_curves.csv")) put(file.path(jt, paste0("summary_", f)), file.path(js, f))
  for (f in c("age_group_support.csv", "HC0_three_group_curves.csv", "receipt.json")) put(file.path(jt, f), joint)
  figs <- mk("age45_revision", "outputs", "supplementary", "figures")
  old <- setwd(work); on.exit(setwd(old), add = TRUE)
  for (f in c("render_main_figures_joint.R", "render_figure1_graphical_abstract_joint.R", "render_supplementary_fit_figures.R")) source(file.path(scripts, f), local = TRUE)
  render_main_figures_joint(); render_figure1_graphical_abstract_joint(); render_supplementary_fit_figures()
  source(file.path(scripts, "render_revised_additional_figures.R"), local = TRUE)
  map <- c(Figure_1_graphical_abstract_joint = "Figure_1_study_overview", Figure_2_joint_three_group = "Figure_2_age_specific_risk_RR_RD",
           Figure_3_joint_crossover_forest = "Figure_3_main_crossover", interaction_sensitivity = "Figure_S1_interaction_sensitivity",
           dose_response = "Figure_S2_dose_response", control_outcome_and_race_strata = "Figure_S3_control_outcome_and_strata")
  for (k in names(map)) put(file.path(figs, paste0(k, ".pdf")), file.path(out, paste0(map[[k]], ".pdf")))
  stopifnot(length(list.files(out, "[.]pdf$")) == 6L)
  message("All six publication figures reproduced from packaged aggregate inputs: ", out)
  invisible(out)
}
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  reproduce_figures(".", if (length(args)) args[1] else "reproduced_outputs/Figures")
}
