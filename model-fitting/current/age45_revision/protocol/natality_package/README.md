# Bundled natality reader

This directory contains `natality` version 0.4.0.9003, the reader source snapshot used for
the study's imports and source-field checks. It is retained so the importer does not depend
on future package changes. No birth records are included. The executable reader source is
unchanged; this README describes its use within the research repository.

## Installation

From the research repository root:

```sh
R CMD INSTALL model-fitting/current/age45_revision/protocol/natality_package
```

Install the dependencies listed in DESCRIPTION before installation. The publication-table
and figure reconstruction does not require this package.

## Annual metadata and local reads

```r
library(natality)

natality_years()
natality_vars("gestation", years = 2024)
natality_codebook("OEGest_Comb", years = 2024)
natality_changes("DPLURAL", years = 2014:2024)

births <- read_natality(
  2024,
  vars = c("MAGER", "OEGest_Comb", "DBWT"),
  path = "/path/to/natality_2024_source_v1.parquet",
  population = "us_residents",
  download = FALSE
)
```

Columns retain the annual source field spelling and source codes, with an explicit `year`
column. A supplied filename does not establish authenticated provenance or cross-year
clinical comparability. Consult the annual source manuals before pooling variables.

A matching annual artifact can be imported into the verified cache:

```r
natality_cache_import(2024, "/path/to/natality_2024_source_v1.parquet")
natality_cache_info()
births <- read_natality(2024, c("MAGER", "OEGest_Comb", "DBWT"),
                        population = "us_residents", download = FALSE)
```

The cache uses size and SHA256 checks. Reads with downloads enabled may retrieve a complete
annual Parquet file from the endpoints recorded in `inst/extdata/data-manifest.json`;
subsequent reads can reuse the cache. No study workflow installs or downloads inputs silently.

See the [refitting instructions](../../../../README.md) and
[data-access and environment guide](../../../../../REPRODUCIBILITY.md) for the additional
prepared-input requirements of this study. Package-level tests are under `tests/testthat/`.
