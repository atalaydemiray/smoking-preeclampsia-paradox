# Maternal smoking, maternal age and recorded pregnancy hypertension

[![Publication reconstruction](https://github.com/atalaydemiray/smoking-preeclampsia-paradox/actions/workflows/r-code.yml/badge.svg)](https://github.com/atalaydemiray/smoking-preeclampsia-paradox/actions/workflows/r-code.yml)

Analysis code and aggregate results for *Reframing the smoking-preeclampsia paradox: an age-related
reversal in 30.1 million United States birth records*. The study examines how the association
between maternal smoking and recorded gestational hypertension/preeclampsia varies with maternal age.
The main analysis uses United States natality records for 2016–2024 at maternal ages 15–45 years.
The repository follows The Turing Way's [research-compendium principles](https://book.the-turing-way.org/reproducible-research/compendia/):
separate inputs, methods and outputs, with a documented computational environment.

## Reconstruct the publication tables and figures

Requirements: R 4.6.0 with its base/recommended packages. No additional R packages or source-data
downloads are needed for aggregate reconstruction or the R integrity checks. Git is needed to clone
the repository and to check candidate release contents. Python is not required.

Download the repository, or clone it and run the reconstruction:

```sh
git clone https://github.com/atalaydemiray/smoking-preeclampsia-paradox.git
cd smoking-preeclampsia-paradox
Rscript --vanilla run_all.R
```

Alternatively, open [smoking-preeclampsia-paradox.Rproj](smoking-preeclampsia-paradox.Rproj) in
RStudio. The project starts without restoring a saved workspace:

```r
source("run_all.R")             # sourcing starts nothing
reproduce_publication()
```

This uses only base/recommended R packages. It reconstructs **27 CSV table files** (3 main tables
and 19 numbered supplementary tables with lettered parts) and **6 vector PDF figures** from
packaged aggregate results. It compares every table cell with the publication reference tables,
independently checks the likelihood-ratio statistics, log-scale probabilities and Holm adjustment,
and verifies contrast inversions, ages 15–45, and denominators. Table titles and notes are exported too.

Outputs are in `output/reproduction/`. A nonempty output directory is never deleted or overwritten;
choose another directory for another run:

```sh
Rscript --vanilla run_all.R output/my_check
Rscript --vanilla tests/check_source_integrity.R
```

This command rebuilds publication exhibits from existing estimates; it does not refit the models.
The CI badge covers aggregate reconstruction and code/source checks, not record-level estimation.

Every successful run also writes its environment, Git status when available, input fingerprints,
output checksums and a completion receipt. Within the repository, generated files can be written
only under `output/`; source and reference directories are protected. For use from another working
directory, supply `repository=` explicitly to `reproduce_publication()`.

## Rerun the statistical analysis

The R code includes the joint three-group model, secondary prepregnancy model, sensitivity analyses,
and interaction tests. See [model-fitting/README.md](model-fitting/README.md)
for the isolated RStudio rerun, prerequisites, and dry-run plan. Large fits run sequentially.

The prepared-data refit route requires the record-level inputs and their provenance receipts.
Raw NCHS archives alone do not supply these prepared inputs. Import and preparation code is included,
but standalone reproduction from raw downloads has not been verified. Individual birth records
and fitted record-level objects are not distributed.

## Study models and results

The primary model includes 30,076,524 complete-case births in three groups:

- **SS:** smoking before pregnancy and in the first trimester, the reporting reference.
- **SN:** smoking before pregnancy, with none reported in the first trimester.
- **NN:** none reported in either period.

The secondary model includes 30,097,165 births and compares **N** (none before pregnancy) with
**S** (smoking before pregnancy), irrespective of first-trimester status. These populations overlap.

| Comparison | Adjusted RR (95% CI) | Crossover age (local 95% CI) |
|---|---|---|
| SN vs SS | 1.063 (1.052–1.074) | 33.3 (32.1–34.5) |
| NN vs SS | 0.988 (0.983–0.994) | 29.4 (29.1–29.7) |
| N vs S | 0.970 (0.966–0.975) | 28.6 (28.2–28.9) |

The global age-by-smoking likelihood-ratio statistics are 2161.41 on 8 df and 1901.18 on 4 df;
both Holm-adjusted P<.001, with concordant HC0 Wald tests. These tests assess interaction on the
conditional log-odds scale; standardized risks, RRs/RDs and simultaneous sign bands describe the
age-dependent reversal.

No investigator imputation was used. Model definitions, adjustment, standardization and inference
are described in [METHODS.md](METHODS.md).

## Data access

The source records are **NCHS public-use vital statistics data**.
No record-level download is needed to rebuild the supplied tables and figures.

For record-level work, obtain the annual **U.S. public-use files and their user guides** from the
[NCHS vital statistics portal](https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm).
The [NBER natality catalogue](https://www.nber.org/research/data/vital-statistics-natality-birth-data)
is an alternative access route. Use 2016–2024 for the main analysis and 2014–2015 for the historical
sensitivity analysis. Fetal-death files for 2018–2024 and linked files named `2018PE2017CO.zip`
through `2024PE2023CO.zip` support supplementary analyses. Period and birth-cohort years differ;
they must not be treated interchangeably.

The [annual data manifest](model-fitting/current/age45_revision/protocol/natality_package/inst/extdata/data-manifest.json)
records official natality archive/guide URLs, converted-artifact endpoints and checksums;
[model-fitting/source_files.csv](model-fitting/source_files.csv) records fetal and linked archives
and manuals. Public raw downloads alone are not the prepared inputs needed by the fitting runner.
Natality imports use the bundled `natality` reader, version 0.4.0.9003; it is not needed for
aggregate reconstruction or fitting models from prepared inputs. See
[model-fitting/README.md](model-fitting/README.md) for import prerequisites and installation,
[REPRODUCIBILITY.md](REPRODUCIBILITY.md) for environments and provenance, and
[DATA_DICTIONARY.md](DATA_DICTIONARY.md) for the supplied aggregate schemas.

## Repository map

| Path | Purpose |
|---|---|
| `run_all.R`, `R/tables_october.R` | Aggregate table/figure reconstruction |
| `publication/Code/` | Joint-model, descriptive, interaction and figure inputs/code |
| `publication/layouts/` | Headings and row labels only, not estimate inputs |
| `publication/reference_tables/` | Publication-display reference tables for regression tests |
| `publication/source_manifest.csv` | SHA256 provenance for imported sources and evidence |
| `model-fitting/run_current.R` | Safe workspace preparation, preflight and 53-step serial fitting plan |
| `model-fitting/current/` | Statistical engines, source adapters and analysis scripts |
| `results/` | Supplementary aggregate estimates |
| `tests/` | Independent aggregate, source-integrity and runner checks |
| `REPRODUCIBILITY.md` | Environment, data access, exhibit map and reproducibility scope |
| `DATA_DICTIONARY.md` | Aggregate schemas, keys, effect scales and missing-value conventions |
| `CONTRIBUTING.md` | Reporting errors, changing code and reviewing scientific amendments |

Supplementary estimates and validation diagnostics are included regardless of statistical
significance. Dated analysis amendments document methodological changes and distinguish analyses
developed after the initial findings from preregistered analyses.

## Validation and support

The reconstruction stops if a table differs from its reference or a numerical check fails.
Successful runs write `run_receipt.csv`, `session_info.txt`, `reconstruction_inputs.csv` and
`output_manifest.csv`. See [tests/README.md](tests/README.md) for source-integrity, safety and
synthetic tests. GitHub Actions reports these checks for each tested commit.

If the destination already contains results, choose a new output directory. For missing prepared
inputs during model fitting, follow [model-fitting/README.md](model-fitting/README.md); they are not
needed for aggregate reconstruction. Report problems through [GitHub Issues](https://github.com/atalaydemiray/smoking-preeclampsia-paradox/issues)
using the instructions in [CONTRIBUTING.md](CONTRIBUTING.md).

## Software and citation

The analysis used R 4.6.0; aggregate reconstruction needs no add-on R packages. Record-level fitting
requires `data.table`, `jsonlite`, and `digest`. Import dependencies are listed in
[model-fitting/README.md](model-fitting/README.md). No driver installs packages or downloads data automatically.

The code has an MIT licence. NCHS source data remain governed by their source terms.
See [CITATION.cff](CITATION.cff) for citation metadata and [CHANGELOG.md](CHANGELOG.md) for changes.
For reproducibility, cite the archived release corresponding to the results you use.
