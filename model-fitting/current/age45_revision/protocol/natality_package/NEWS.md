# natality 0.4.0.9003

* Added item-specific interpretation warnings: F_MAR_P concerns paternity
  acknowledgment, and DMAR has California release restrictions from 2017.
* Preserve all original values and retain the warnings in each annual read's
  interpretation_notes provenance. These warnings do not establish harmonization.

# natality 0.4.0.9002

* Enabled GitHub source-access alpha downloads for 2014–2024, with pinned size and SHA-256 verification.
* Verified all eleven first downloads through the real R API, annual population controls, and offline cache reuse.
* Kept independent scientific review pending. GitHub is the sole configured host; archival mirrors and an independent backup disk are deferred.
* Added an offline empty-cache test; unpublished-release tests use explicit fixtures and never make network requests.

# natality 0.4.0.9001

* Expanded annual source-preserving builds backward from 2024; the bundled
  manifest reports passed local artifacts separately from metadata availability.
* Added verified local-cache imports, guarded multi-year reads and provenance.
* Preserved native U.S. source names, including the 2015–2016 FILLER_F reporting
  flag; excluded territory-only addenda from U.S. variable discovery.
* Codebooks distinguish prior-conversion storage, printed width and actual
  position span. MRACE6 printed-width decisions are recorded explicitly.
* Fixed versioned cache inventory and clearing; tested corruption, interrupted
  transfers and mirror recovery. A verified cache remains usable offline.
* Added warnings and codebook notes for the unlisted 2015 race codes, with
  original values preserved and no automatic recoding.
* Added a local RStudio project/example and prepared platform CI. Public hosting,
  live hosted-download checks and independent scientific review remain pending.

# natality 0.4.0.9000

* Added the initial installable package foundation.
* Added offline annual variable, codebook, and change-history discovery for
  2014–2024.
* Added cache inspection and bounded clearing helpers.
* Added selected-column reads from user-supplied Stata, RDS, or Parquet files.
* Built and validated the complete 2024 source-preserving Parquet artifact:
  3,638,436 records and 237 source fields.
* Added size- and SHA-256-verified download and cache handling. Remote reads
  remain disabled until the artifact receives a stable public release URL.

* Added verified local-cache import for private alpha testing before hosting.
* Fixed cache inventory and clearing for versioned artifact filenames.
* Added guarded multi-year reads with preserved annual provenance and explicit
  comparability warnings; incompatible names, types and labels stop the read.
* Added corruption, interruption, source-name ambiguity and local-import tests.
