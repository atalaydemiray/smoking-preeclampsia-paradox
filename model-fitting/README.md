# Rerun the statistical analysis

Use `run_current.R` from the repository root. It preserves original source paths so the model
receipts continue to identify the exact code and inputs. `run_current.R` is the record-level entry point.

## Prepared-data route in RStudio

```r
source("model-fitting/run_current.R")       # no fitting or data writes
current_analysis_plan()                     # 53 serial commands
current_preflight("/path/to/project/01_Analysis")

workspace <- prepare_current_workspace(
  destination = "/path/to/new/nonexistent/rerun",
  input_workspace = "/path/to/project/01_Analysis")

run_current_analysis(workspace)              # dry-run plan only
run_current_analysis(workspace, execute = TRUE)  # actual fits; can take many hours
```

The destination must be outside this Git repository. The helper copies the current code, links
only prepared inputs, and creates new output folders. Input links are read-only **by convention**,
not protected by filesystem permissions. Do not run import/preparation writers in that workspace.
Model scripts validate input receipts and hashes before fitting. A failed command stops later
steps. Each large fit runs in a fresh R process, sequentially. No imputation is scheduled.

The plan includes sensitivity fits, the joint model, secondary model, exact contrast
inversions, group descriptions, two reduced-model interaction fits, collector, and final input/
model/output validator. The redundant pairwise fits remain internal prerequisites for numerical
checks, not duplicate publication rows. Known-truth simulation evidence is retained from the
earlier validated simulation runs, not rerun by these 53 commands.

## Inputs and packages

The existing preparation workspace must contain `age45_revision/derived/` with:

- `model_inputs/`: annual primary 2014–2024 and broad/prepregnancy 2016–2024 RDS files and receipts.
- `supplementary_inputs/`: both extended populations for 2016–2024 and their receipts.
- `adjunct_models/`: prepared fetal-inclusive, fetal-source and linked-mortality inputs and receipts.
- `natality/`: annual clinical-flow ledgers for 2016–2024.

The preflight checks all 221 required input/receipt/ledger paths and required package availability;
it does not substitute for the
full hash and numerical checks performed by the models. Record-level fitting needs R (originally
4.6.0), `data.table`, `jsonlite`, and `digest`. No script installs packages automatically. Output
receipts retain the actual session/version information from each fit.

## Starting from public source archives

`source_files.csv` records the historical NCHS download inventory. Original import/preparation
engines are included under `current/`. Natality imports require the bundled `natality` reader
0.4.0.9003; prepared-data fitting and aggregate reconstruction do not. Install the dependencies
listed in [DESCRIPTION](current/age45_revision/protocol/natality_package/DESCRIPTION) first,
then install the source from the repository root:

```sh
R CMD INSTALL model-fitting/current/age45_revision/protocol/natality_package
```

Imports additionally require the natality package cache, `arrow`, annual source dictionaries,
fetal/linked source gates, and earlier decoded/provenance inputs used for exact age-15–44 parity
checks. The import driver stops if these are unavailable. A public raw
archive download alone does **not** supply all these prerequisites; standalone raw-source
reproduction has not been verified. Import scripts document
the original construction; they are not scheduled by the prepared-input fitting runner.

The original fitting versions were R 4.6.0, `data.table` 1.18.4, `jsonlite` 2.0.0 and `digest` 0.6.39.
`R/06_mi_pooling.R` remains byte-identical solely because the frozen complete-case numerical engines
use its covariance validator and pin that source file. No imputation or pooling analysis is run.

## Verification and scope

The national interaction tests passed full-record likelihood/score/covariance checks and
independent 120,000-record `glm` comparisons. Their aggregate receipts are in
`../publication/Code/interaction_tests/`. Existing result receipts refer to the original research
workspace, not to record-level files distributed here.

The repository's aggregate reconstruction, source-integrity checks and preflight are tested.
The 53-step plan writes statistical results in the isolated workspace. In contrast,
`../run_all.R` reconstructs the publication exhibits from the packaged estimates; it does not
automatically consume new fits. A successful aggregate reconstruction is not validation of a
record-level rerun.
