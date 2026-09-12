# Maternal smoking, maternal age and recorded pregnancy hypertension

[![R code](https://github.com/atalaydemiray/smoking-preeclampsia-paradox/actions/workflows/r-code.yml/badge.svg)](https://github.com/atalaydemiray/smoking-preeclampsia-paradox/actions/workflows/r-code.yml)

Analysis code and aggregate results for "Reframing the smoking-preeclampsia paradox: an age-related
reversal in 30.1 million United States birth records".

Smoking during pregnancy has long been reported to be associated with less preeclampsia. Using United
States natality records for 2016 to 2024, this analysis shows that the association changes direction
with maternal age: recorded gestational hypertension or preeclampsia was less frequent among women who
smoked at younger maternal ages and more frequent at older ages, crossing at about age 30 in all three
smoking comparisons examined.

## Reproduce every table and figure

```sh
Rscript run_all.R
```

About one minute. It rebuilds all 28 tables and all 8 figures of the paper into `output/`, reading the
aggregate estimates shipped in `results/`. It needs no data download, no credentials and no network,
and it uses only packages that ship with R.

To rerun the analysis itself, from the source files, see [model-fitting/README.md](model-fitting/README.md).
That path needs about 40 GB of public-use files from the National Center for Health Statistics and runs
for days. Those files are free to download; `model-fitting/source_files.csv` lists every one with its
URL, its expected size and the path the code expects it at.

## What is here

| Path | Contents |
| --- | --- |
| `run_all.R` | The only entry point. Rebuilds every table and figure, then checks them. |
| `R/` | Functions only: paths, number formatting, labels, one function per table and per figure. |
| `results/` | The estimates the paper reports, as aggregate CSVs. Described in [results/SCHEMA.md](results/SCHEMA.md). |
| `model-fitting/` | The code that produced `results/` from the source files, with the frozen reader package it uses. |
| `METHODS.md` | The study specification: population, exposure, outcome, model, crossover estimation. |
| `.github/` | The check that reruns the command above on a clean machine after every change. |

`run_all.R` writes into `output/`, which is not committed because it is rebuilt in a second.

## What is not here

- Record-level data of any kind. The natality, fetal death and linked birth and infant death files are
  public and permanently archived by the National Center for Health Statistics, so this repository
  ships the aggregate estimates instead of a second copy of 40 GB of source files.
- The manuscript, its tables as journal documents, and anything to do with submitting it.

Nothing in `results/` has one row per birth. Every file there is a count, an estimate or a curve.

## Software

- R 4.6.0. The fast path uses only base R with `stats`, `grDevices`, `splines` and `utils`.
- The fitting path additionally needs `data.table`, `jsonlite`, `digest` and `arrow`, plus the frozen
  `natality` reader in `model-fitting/vendor/`. No script installs anything.
- Run on macOS 15 with R 4.6.0. Nothing in the fast path is platform specific.
- No pseudo-random number generator is used in the fast path. The one place randomness enters the
  analysis is the known-truth simulation in `results/main/crossover_simulation_summary.csv`, which is a
  stored result of the fitting path rather than something the fast path recomputes.

## Where each exhibit comes from

Every table and figure is written by `run_all.R` from the files in `results/`. The function that
builds each one carries the same name as the exhibit, so
`Table_S16` is built by `table_s16()` in `R/tables_sensitivity.R` and `Figure_2` by `figure_2()` in
`R/figures.R`.

| Exhibit | Built by | Reads |
| --- | --- | --- |
| Tables 1 to 3, S1 to S5 | `R/tables_main.R` | `results/main/`, `results/descriptive/` |
| Tables S6a to S10 | `R/tables_specification.R` | `results/main/`, `results/models/` |
| Tables S11 to S15 | `R/tables_bias.R` | `results/bias/` |
| Tables S16 to S19, S21, S22 | `R/tables_sensitivity.R` | `results/sensitivity/`, `results/descriptive/` |
| Table S20 | `R/tables_definitions.R` | definitions stated in the file |
| Figures 1 to 3, S1 to S5 | `R/figures.R` | `results/main/`, `results/models/`, `results/sensitivity/`, `results/bias/` |

## Headline results

Among 1,980,559 births to women who smoked before pregnancy, continued first-trimester smoking versus
stopping was associated with an adjusted risk ratio of 0.944 (95% confidence interval 0.934 to 0.954),
and the fitted association crossed one at 31.8 years (local 95% confidence interval 31.0 to 32.7). The
two broader comparisons, of 29,576,508 and 30,097,165 births, crossed at 29.4 and 28.6 years. These
numbers are in `output/tables/Table_2.csv` and `output/tables/Table_3.csv` after a run, and in
`results/main/main_overall.csv` and `results/main/root_reference.csv` before one.

The three populations overlap and must not be added.

## Licence and citation

The code is released under the MIT licence (`LICENSE`). The licence covers the code in this repository
only. The source files remain governed by the terms of the agency that publishes them, which place no
restriction on their use.

`CITATION.cff` carries the citation metadata. Cite the manuscript; the repository version that produced
the submitted results is tagged.
