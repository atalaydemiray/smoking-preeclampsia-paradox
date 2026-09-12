# Model fitting

This directory holds the code that fitted the models. The aggregate estimates in `../results/` were
produced by this code, and everything in `../output/` is built from those estimates by `../run_all.R`.

Rebuilding the tables and figures does not require anything here. `../run_all.R` reads `../results/`
and needs no source data, no credentials and no network. This directory is for the step before that:
reading about 40 GB of vital statistics records and fitting the 36 main models, the 31 supplementary
models and the quantitative bias analyses that `../results/` summarises.

The statistics are unchanged from the run that produced the published estimates. The only thing that
changed when the code moved here is where it looks for files.

## The withdrawn analysis

`scripts/14_fit_supplementary.R` carries a branch for a sensitivity analysis that keeps women with
recorded chronic hypertension. It was authorised, run and then withdrawn: no birth with chronic
hypertension recorded also carries a recorded GH/PE diagnosis, 0 of 70,549, so the model cannot be
estimated and the fit does not converge. Nothing from it appears in the paper, and the preparation
script it would need is not included. The branch is left in place because this is the code that ran.

## Two things a reader will notice

A few comments cite dated protocol documents, for example the supplementary amendment of 11 September
2026. Those documents are held with the study records and are not distributed here; the comments name
them so that a decision can be traced, not so that a file can be opened.

The code in this directory is the code that produced the estimates. What changed when it moved here is
where it looks for files and what those files are called: paths were rewritten, and the engines and
scripts were renumbered into the order they run, since the original numbering carried gaps left by
modules that this analysis never used. No statistical code was touched.

`vendor/` holds the reader package as a frozen tarball. The test stage unpacks it to `vendor/natality`
on first use, so that directory appears after the first run rather than in a fresh clone.

## What is not here

The source files are not in this repository. They are public-use files published by the National
Center for Health Statistics, free to download and too large to distribute. `source_files.csv` in this
directory lists every file with its download URL, its expected size and the path the code expects it
at, under a `data/` directory you create at the repository root.

Paths are resolved relative to this directory, so the `relative_path` column of `source_files.csv`
means `model-fitting/data/raw/...` here.

Besides the source files, the pipeline reads several small inputs that came from the earlier stages of
the same project and are also not in this repository:

| Path, relative to this directory | Read by | What it is |
| --- | --- | --- |
| `data/dictionaries/` | the linked and natality adapters | the NCHS record layouts, checked by hash before parsing |
| `config/fetal_schema.json`, `config/linked_cohort_schema.json` | `scripts/04` | fixed-width field positions and archive hashes for the fetal and linked files |
| `config/outcome_validation_candidates.json` | `scripts/11` | the published validation studies the bias scenarios are anchored on |
| `config/bias_anchors.json` | `scripts/22` | the same anchors for the supplementary bias tables |
| `reference/` | `scripts/08` | the previous submission's estimates, so the comparison tables can be rebuilt |
| `derived/natality_inputs/`, `derived/fetal_inputs/`, `derived/linked_inputs/` | `scripts/01`, `scripts/04` | the frozen decoded records the import reconciles every field against |
| `derived/model_inputs/linked_mortality_2018_2023_20260906_a/` | `scripts/05` | the pooled linked-mortality input the age-45 records are appended to |
| `outputs/qa/linked_cohort_<year>_feasibility.json` | `scripts/04` | the source gate receipts for the linked archives |
| `tests/test_primary_bias_sensitivity.R` | `scripts/11` | hashed into the bias receipt alongside the code it tests |

A stage stops with the name of the file it could not find rather than working around a missing input.

## Installing the natality reader

The records are read through the `natality` package, whose source is frozen in `vendor/` with its
checksum so that the field definitions are the ones that produced the published estimates. From the
repository root:

```sh
(cd vendor && shasum -a 256 -c SHA256SUMS)
tar xzf vendor/natality_0.4.0.9003.tar.gz -C vendor
R CMD INSTALL vendor/natality
```

The `tests` stage reads the unpacked source tree at `vendor/natality` to run the package's own test
suite. Set `NATALITY_PACKAGE_SOURCE` if you keep it somewhere else.

## Running the stages

Set the working directory to this one, then source the driver:

```r
source("run.R")
run_model_fitting()                  # every stage, in order
run_model_fitting("fits")            # or one stage
run_model_fitting(c("fits", "aggregate"))
```

The stages run in this order. `run.R` carries the same list with what each one needs.

| Stage | Scripts | Roughly how long |
| --- | --- | --- |
| `tests` | `07` | a few minutes |
| `imports` | `01`, `04` | 17 minutes for the eleven natality years, considerably longer for the linked files |
| `prepare` | `02`, `05` | about an hour |
| `fits` | `03`, `06`, `14` | about two hours for all 36 models |
| `validate` | `09` | about as long as the fits it audits |
| `describe` | `10` | minutes |
| `aggregate` | `08` | under a minute |

Every stage is restartable. Each script verifies the receipt and the hash of any completed work it
finds and reuses it instead of refitting, so a stage that stops partway can simply be run again.

`derived/` grows to about 2.5 GB. The two thirty-million-record fits are the memory ceiling: the
solver streams in chunks of 25,000 rows and the large comparisons run one at a time for that reason.

### The supplementary analyses

These are not stages in `run.R`. Once the main fits are complete, run them in this order:

| Script | What it does |
| --- | --- |
| `scripts/13_prepare_supplementary_inputs.R` | the two extended populations, with the early-smoking flags |
| `scripts/14_fit_supplementary.R` | the 31 fits, one per process for the large ones |
| `scripts/15_supplementary_bias.R` | the quantitative bias tables |
| `scripts/16_assemble_supplementary.R` | assembles the fits into the three summary files |

The 31 supplementary fits take about three hours in total, the longest of them about 14 minutes.

`scripts/11_bias_scenarios.R` produces the main-text bias scenarios and is run on its own:
`Rscript --vanilla scripts/11_bias_scenarios.R --execute --run-id=<id>`. Without `--execute` it prints
the plan and writes nothing.

## Packages and R version

Written and run under R 4.6.0. The versions below are the ones recorded in the session that produced
the published estimates.

| Package | Version | Needed for |
| --- | --- | --- |
| `natality` | 0.4.0.9003 | reading the natality files, installed from `vendor/` |
| `data.table` | 1.18.4 | every stage |
| `jsonlite` | 2.0.0 | every receipt |
| `digest` | 0.6.39 | the hashes in every receipt |
| `arrow` | 25.0.1 | the natality reader's storage backend |
| `sandwich` | 3.1-1 | the HC0 covariance |
| `testthat` | any recent version | the `tests` stage only |

`stats`, `utils` and `splines` ship with R. Nothing else is used, and no stage reaches the network.
