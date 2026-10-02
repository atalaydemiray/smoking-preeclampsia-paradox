# Maternal smoking, maternal age and recorded pregnancy hypertension

[![Publication reconstruction](https://github.com/atalaydemiray/smoking-preeclampsia-paradox/actions/workflows/r-code.yml/badge.svg)](https://github.com/atalaydemiray/smoking-preeclampsia-paradox/actions/workflows/r-code.yml)

Code and aggregate evidence for *Reframing the smoking-preeclampsia paradox: an age-related reversal
in 30.1 million United States birth records*. This working-tree revision matches the **1 October
2026** manuscript package, including the approved expanded Table 3. The 2 October
repository audit changes reconstruction safeguards and documentation, not fitted estimates.

## Reconstruct the publication tables and figures

From this repository's root:

```sh
Rscript --vanilla run_all.R
```

Or open the `.Rproj` in RStudio:

```r
source("run_all.R")             # sourcing starts nothing
reproduce_publication()
```

This uses only base/recommended R packages. It reconstructs **27 CSV table files** (3 main tables
and 19 numbered supplementary tables with lettered parts) and **6 vector PDF figures** from
packaged aggregate results. It compares every table cell with the October reference tables,
independently checks the likelihood-ratio statistics, log-scale probabilities and Holm adjustment,
and verifies contrast inversions, ages 15–45, and denominators. Table titles and notes are exported too.

Outputs are in `output/2026-10-02/`. A nonempty output directory is never deleted or overwritten;
choose another directory for another run:

```sh
Rscript --vanilla run_all.R output/my_check
python3 tests/check_source_integrity.py
```

**This is publication reconstruction, not a new fit of 30 million records.** The CI badge covers
aggregate reconstruction and code/source checks, not raw-data reproduction or scientific validity.
Visual approval of the manuscript figures remains an author task.

Every successful run also writes its environment, Git status when available, input fingerprints,
output checksums and a completion receipt. Within the repository, generated files can be written
only under `output/`; source and reference directories are protected. For use from another working
directory, supply `repository=` explicitly to `reproduce_publication()`.

## Rerun the statistical analysis

The current R code includes the joint three-group model, secondary prepregnancy model, retained
sensitivity analyses, and the new interaction tests. See [model-fitting/README.md](model-fitting/README.md)
for the isolated RStudio rerun, prerequisites, and dry-run plan. Large fits run sequentially.

The prepared-data refit route requires the record-level inputs and their provenance receipts.
Raw NCHS archives alone are not enough for the historical reconciliation steps. The import and
preparation source is retained, but a clean raw-download-to-all-results run without the research
workspace is **not certified**. No individual birth records, fitted record-level objects, manuscript
files, or credentials are included in the current source/aggregate bundle.

## Current analysis

The primary model includes 30,076,524 complete-case births in three groups:

- **SS:** smoking before pregnancy and in the first trimester, the reporting reference.
- **SN:** smoking before pregnancy, with none reported in the first trimester.
- **NN:** none reported in either period.

The secondary model includes 30,097,165 births and compares **N** (none before pregnancy) with
**S** (smoking before pregnancy), irrespective of first-trimester status. These populations overlap.

| Current comparison | Adjusted RR (95% CI) | Crossover age (local 95% CI) |
|---|---|---|
| SN vs SS | 1.063 (1.052–1.074) | 33.3 (32.1–34.5) |
| NN vs SS | 0.988 (0.983–0.994) | 29.4 (29.1–29.7) |
| N vs S | 0.970 (0.966–0.975) | 28.6 (28.2–28.9) |

The new global age-by-smoking likelihood-ratio statistics are 2161.41 on 8 df and 1901.18 on 4 df;
both Holm-adjusted P<.001, with concordant HC0 Wald tests. These tests assess interaction on the
conditional log-odds scale; standardized risks, RRs/RDs and simultaneous sign bands describe the
age-dependent reversal. The October tests did not change the full-model estimates.

The 31.8-year crossover in earlier material belongs to a separate within-group sensitivity fit,
not the current joint primary model. No investigator imputation was used. See [METHODS.md](METHODS.md).

## Repository map

| Path | Purpose |
|---|---|
| `run_all.R`, `R/tables_october.R` | Current aggregate table/figure reconstruction |
| `publication/Code/` | Current joint-model, descriptive, interaction and figure inputs/code |
| `publication/layouts/` | Headings and row labels only, not estimate inputs |
| `publication/reference_tables/` | Approved tables, read only as regression-test expectations |
| `publication/source_manifest.csv` | SHA256 provenance for imported sources and evidence |
| `model-fitting/run_current.R` | Safe workspace preparation, preflight and 53-step serial fitting plan |
| `model-fitting/current/` | Frozen original-path R engines and current analysis extensions |
| `results/` | Only the retained sensitivity aggregates used by the current tables |
| `tests/` | Independent aggregate, source-integrity and runner checks |
| `REPRODUCIBILITY.md` | Environment, data access, result map, audit scope and Zenodo release checklist |
| `DATA_DICTIONARY.md` | Aggregate schemas, keys, effect scales and missing-value conventions |
| `CONTRIBUTING.md` | Reporting errors, changing code and reviewing scientific amendments |
| `RUN_LOG.md` | Dated commands, comparisons, actual verification and remaining limits |

The public tree excludes duplicate September pipelines, old publication builders, withdrawn
quantitative-bias outputs, unused imputation engines, manuscript drafts and local working files.
Retained sensitivity analyses and their validation remain available, regardless of statistical
significance. See [PUBLICATION_SCOPE.md](PUBLICATION_SCOPE.md) for the inclusion rules.

## Software and citation

The analysis used R 4.6.0; aggregate reconstruction needs no add-on R packages. Record-level fitting
requires `data.table`, `jsonlite`, and `digest`; imports additionally use the frozen natality reader
and its dependencies. No driver installs packages or downloads data automatically.

The code has an MIT licence. NCHS source data remain governed by their source terms. `CITATION.cff`
records software/manuscript metadata; no accepted publication, new release DOI or submission status
is asserted by this code update. [CHANGELOG.md](CHANGELOG.md) records the October changes.
See [REPRODUCIBILITY.md](REPRODUCIBILITY.md) before creating a versioned Zenodo deposit.
