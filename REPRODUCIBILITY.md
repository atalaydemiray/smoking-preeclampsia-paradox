# Reproducibility

This repository separates executable
methods (`R/`, `model-fitting/`), aggregate inputs (`publication/Code/`, `results/`),
reference displays (`publication/reference_tables/`) and generated outputs (`output/`). The frozen
statistical sources are retained at their original relative paths because fitted-result receipts
pin their hashes.

The layout follows The Turing Way's principles for
[research compendia](https://book.the-turing-way.org/reproducible-research/compendia/),
[project documentation](https://book.the-turing-way.org/reproducible-research/code-documentation/code-documentation-project/),
[testing](https://book.the-turing-way.org/reproducible-research/testing/testing-checklist/) and
[computational environments](https://book.the-turing-way.org/reproducible-research/renv/renv-resources/).

## Supported modes

| Mode | Command/entry point | What it establishes |
| --- | --- | --- |
| Public reconstruction | `Rscript --vanilla run_all.R` | Rebuilds 27 table CSVs and six vector PDFs from shipped aggregate estimates, and checks every table cell. |
| Prepared-data refit | `model-fitting/run_current.R` | An explicit 53-step serial plan for authorized local records and receipts. Dry run is the default. |
| Source import | Source adapters and bundled natality reader | Requires annual dictionaries and historical reconciliation inputs in addition to raw archives. Standalone raw-download reproduction has not been verified. |

The default command installs nothing, downloads nothing and performs no record-level fitting.
Run from the repository root, or in R use `reproduce_publication(repository="/path/to/clone",
output="/path/to/new/output")`. It restores the original working directory after success or error.
Input/reference files are read-only by workflow convention; path guards prevent the master command
from writing generated results into source directories. A successful run records input/output
fingerprints, session information, code commit when available, dirty status and a completion receipt.
MD5 output fingerprints identify local artifacts; `publication/source_manifest.csv` supplies
the SHA256 hashes of imported sources. METHODS.md and DATA_DICTIONARY.md are also fingerprinted
to identify the analysis specification accompanying each reconstruction.

## Computational environment

Public reconstruction requires R 4.6.0 and its base/recommended packages only. No additional R
package is required. Source/safety checks require Python 3 (standard library) and Git. The GitHub
workflow pins R 4.6.0 on Ubuntu. Workflow results and run receipts identify the commit and
environment used for a particular reconstruction.

Prepared-data estimation originally used:

| Dependency | Version/scope |
| --- | --- |
| R | 4.6.0 |
| `data.table` | 1.18.4 |
| `jsonlite` | 2.0.0 |
| `digest` | 0.6.39 |
| `natality` | Bundled source 0.4.0.9003, with its own DESCRIPTION/license; needed for imports, not aggregate reconstruction. |
| `arrow` | Import/cache dependency of the bundled natality reader; see its DESCRIPTION for requirements. |
| `pdftotext` | External prerequisite for annual linked-source guide extraction; document the actual version in an import run. |

The original fitting session is recorded in `publication/Code/interaction_tests/joint/sessionInfo.txt`
and its secondary counterpart: arm64 macOS, default BLAS, LAPACK 3.12.1, C.UTF-8 locale and
Europe/Amsterdam timezone. Each actual refit records its own environment; installing the listed
versions does not guarantee numerical identity across BLAS/OS combinations. No complete dependency
restore or container build has been verified. This is an environment specification,
not a tested lockfile restoration claim.

The aggregate reconstruction is deterministic and uses no random sampling. Retained synthetic
coverage evidence uses eight scenarios and 1,000 attempts per scenario with explicit seed families
in `model-fitting/current/scripts/64_crossover_coverage_validation.R`; its historical simulation
domain is 15–44, whereas the clinical fitted domain is 15–45.
That diagnostic distinction does not exclude age 45 from the clinical analysis. The
interaction validation uses a deterministic subset; the full fitting plan is serial. See source
scripts for individual synthetic-test seeds and numerical tolerances.

## Data access and rights

Source records are public-use NCHS vital statistics files. Obtain annual natality files and manuals
from the [NCHS portal](https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm) or the
[NBER natality catalogue](https://www.nber.org/research/data/vital-statistics-natality-birth-data).
Main analysis years are 2016–2024; 2014–2015 supply historical sensitivity inputs. Fetal-death and
linked birth/infant-death sources supply additional source-specific sensitivities, not recovery
of unobserved early losses. Their historical archive/manual URLs are in `model-fitting/source_files.csv`;
linked filenames include both period and cohort years, so the filename year is not automatically
the birth-cohort year.

Public source availability does not supply the 221 historical prepared-input/receipt/ledger
prerequisites. Those paths are enumerated by `current_preflight()` and documented in
`model-fitting/README.md`. Model scripts verify receipt hashes, cohort signatures, keys, formulas
and convergence. Record identifiers stay outside this repository. Foreign-resident exclusions,
reporting-area gates and source-specific eligibility belong to preparation, not table formatting.

Retain provider citation and terms when obtaining or redistributing source records. The repository's
existing MIT license covers its software and associated documentation; it does not relicense NCHS
records or third-party reference guides. The bundled natality source retains its own MIT notices.
See DATA_DICTIONARY.md for schemas, missingness and effect units.

## Exhibit and in-text result map

All table builders are dispatched by `build_october_tables()` in `R/tables_october.R`. The main
summary below resolves the three principal source directories as follows:

- **Joint**: `publication/Code/supplementary/aggregate_outputs/joint_three_group/`.
- **Tests**: `publication/Code/interaction_tests/`.
- **Sensitivity**: `results/` and `publication/Code/supplementary/aggregate_outputs/`.

| Exhibit/result | Estimate inputs | Generator |
| --- | --- | --- |
| Table 1 | `publication/Code/table_inputs/table1_by_outcome_long.csv` | `build_october_tables()` descriptive block |
| Table 2 and overall risks/RRs/RDs | Joint `summary_main_summary.csv`, arm-support CSVs | `build_october_tables()` overall block |
| Table 3 and main crossover ages | Joint `summary_main_summary.csv` | `oct_summary_rows()`, `oct_root_component()` |
| Table S1 | `results/descriptive/common_clinical_flow_by_year.csv` | `R/tables_retained.R`, `table_s1()` |
| Table S2 and fitted/eligible population sizes | Tests `population_overall.csv` | `build_october_tables()` population block |
| Table S3 | `publication/Code/table_inputs/covariance/*.csv` | `build_october_tables()` covariance block |
| Tables S4a–c and S4e, age-specific main results | Joint `summary_main_age_estimates.csv` | `build_october_tables()` age block |
| Table S4d, within-smoker age-specific sensitivity | `results/main/main_age_estimates.csv` | `R/tables_retained.R`, `age_specific_table()` |
| Table S5 | Joint summary plus `results/main/crossover_summary.csv` and `root_reference.csv` | `oct_summary_rows()`, `R/tables_retained.R`, `table_s5()` |
| Table S6a | `results/main/root_reference.csv`, `crossover_summary.csv` | `R/tables_specification.R`, `table_s6a()` |
| Table S6b | `results/models/*counts.csv`, `*overall_standardized.csv` | Same file, `table_s6b()` |
| Table S7 | `results/main/paired_GH_overall_differences.csv` | Same file, `table_s7()` |
| Table S8 | `results/main/source_specific_endpoints.csv` | Same file, `table_s8()` |
| Table S9 | `results/main/crossover_simulation_summary.csv` | Same file, `table_s9()` |
| Table S10 | `results/main/interaction_age_all_models.csv`, `interaction_overall.csv` | Same file, `table_s10()` |
| Tables S11–14 | `publication/Code/supplementary/aggregate_outputs/supp_summary.csv` | `build_october_tables()`, `sens_row()` |
| Table S15 | `publication/Code/table_inputs/exposure_definitions.csv` | Static definition table, not estimated results |
| Table S16 | Tests population-specific group counts and descriptive CSVs | `build_october_tables()` group-description block |
| Table S17 and missingness percentages | Tests `missingness_overall.csv`, `population_overall.csv` | `build_october_tables()` missingness block |
| Table S18 and interaction statistics | Tests `interaction_tests.csv`, `global_interaction_tests.csv` | `build_october_tables()` test block; independent check in `tests/check_publication.R` |
| Table S19 | Joint `summary_main_summary.csv` | `oct_summary_rows()`; complete null-age sets retained |
| Figure 1 | Joint group support and `publication/Code/aggregate_inputs/` | `render_figure1_graphical_abstract_joint.R` |
| Figures 2–3 | Joint summaries/curves; aggregate crossover inputs and sensitivity summary | `render_main_figures_joint.R` |
| Figure S1 | `publication/Code/aggregate_inputs/interaction_curves.csv` | `render_revised_additional_figures.R` |
| Figures S2–3 | Supplementary summaries and age curves | `render_supplementary_fit_figures.R` |

Figure scripts reside under `publication/Code/publication_code/`; the wrapper stages only aggregate
inputs. Generated filenames are listed in README.md/output manifests. Full-precision numerical
inputs, not reference tables, supply estimates. Layouts supply headings and row labels only;
reference tables are comparison expectations. Main Table 3 is intentionally a selected confidence-set
component with an explanatory note; Table S19 is the complete set. Table S5 retains less precise
and inconclusive sensitivities, not only significant results.

## Analysis provenance and validation

Dated amendments under `model-fitting/current/age45_revision/protocol/` document changes to
the study methods. Supplementary analyses and interaction tests were developed after the initial
findings; they are not represented as preregistered analyses. Supplementary estimates are retained
regardless of statistical significance. Multipurpose source files contain branches that are not
scheduled by the fitting plan and are not reported analyses.

See [tests/README.md](tests/README.md) for checks and commands. The national interaction results
include 24 numerical validation checks with declared tolerances. Reconstruction tests cover
confidence-set selection, reference directions, endpoints, source integrity and protected outputs.
Generated receipts record the environment, inputs, output hashes and completion status of each run.
GitHub Actions checks aggregate reconstruction and synthetic behavior, not full record-level fitting
or scientific validity.

## Citation and archived versions

Use [CITATION.cff](CITATION.cff) for citation metadata and [CHANGELOG.md](CHANGELOG.md) for changes.
For a manuscript or a reproduction study, cite the version-specific DOI of the archived release
corresponding to the code and results used. A concept DOI refers to the project across versions.
Source-data citations and access instructions remain separate from the code citation.
