# natality

`natality` is the R-package companion to Natality Atlas. It is in private
development and is not yet on CRAN.

The local alpha searches the 2014–2024 U.S. source-variable catalog, shows
annual definitions and recorded changes, and supports verified-cache imports
and selected-column single-year or multi-year reads. `natality_years(details = TRUE)`
reports which annual artifacts have passed local conversion and numerical checks.
Original source codes remain strings; source preservation does not establish
clinical comparability. Source-access alpha data are hosted in GitHub Releases;
independent scientific review remains pending.

```r
library(natality)

natality_years()
natality_vars("gestation", years = 2024)
natality_codebook("OEGest_Comb", years = 2024)
natality_changes("DPLURAL", years = 2014:2024)
```

A local annual file can be read without modifying the source:

```r
births <- read_natality(
  2024,
  vars = c("MAGER", "OEGest_Comb", "DBWT"),
  path = "/path/to/natality_2024_source_v1.parquet",
  population = "us_residents"
)
```

The returned columns use the source field spelling recorded in the annual
manual and include an explicit `year` column. The package records that a local
file was supplied by the user; it does not infer authenticated provenance from
a filename.

For private alpha testing, import a matching local artifact into the verified
cache once. Original files are not modified:

```r
natality_cache_import(2024, "/path/to/natality_2024_source_v1.parquet")
natality_cache_info()
births <- read_natality(2024, c("MAGER", "OEGest_Comb", "DBWT"),
                        population = "us_residents", download = FALSE)
```

After importing both years, `read_natality(2023:2024, ...)` returns unchanged
source fields with each year's provenance. It warns that scientific
comparability is not established and stops if requested fields have different
source names, types or labels. It never silently fills unavailable variables.

Automatic downloads use pinned HTTPS release assets and matching size/SHA-256
checks. The first read downloads the full annual file; later reads use the cache.
CRAN is a later distribution step. See
[local testing](../../docs/PACKAGE_LOCAL_TESTING.md) and the runnable
[example](../../examples/test_natality_locally.R).

The first hosted read downloads one complete annual Parquet file into the
local cache; later reads select columns from that file. A valid cache works
without CDC, GitHub or another network service. Published releases can specify
HTTPS mirrors, each required to match the same size and checksum. Public mirrors
are not configured yet. See [access and preservation](../../docs/DATA_ACCESS_AND_PRESERVATION.md).

## GitHub source-access alpha

Annual files for 2014–2024 are available from
[the public data release](https://github.com/atalaydemiray/natality-data/releases/tag/source-v1-alpha).
With this locally installed package version, no `path` is needed:

```r
births <- read_natality(2024, c("MAGER", "OEGest_Comb", "DBWT"),
                        population = "us_residents")
# Reuse the verified copy without network access:
births <- read_natality(2024, c("MAGER", "OEGest_Comb", "DBWT"),
                        population = "us_residents", download = FALSE)
```

GitHub is the only host. Zenodo and an independent backup disk are deferred.
The package source and Atlas site remain in private development; publishing
data files does not imply package availability on CRAN.
