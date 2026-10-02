# Validation scope

Run from the repository root:

```sh
Rscript --vanilla tests/check_source_integrity.R
Rscript --vanilla tests/check_release_contents.R
Rscript --vanilla tests/check_integrity_contract.R
Rscript --vanilla tests/check_current_runner.R
Rscript --vanilla tests/check_reconstruction_contract.R
Rscript --vanilla run_all.R output/my_validation
```

All commands use R 4.6.0 and bundled packages; the candidate-release scan and its fixtures also
need Git. There is no Python dependency. The three integrity checks resolve their repository
from the script location; the other commands below are run from the repository root.
Use a new output directory for each reconstruction. The release-content check examines tracked
and nonignored candidate files, including new uncommitted files. No test downloads or reads study
birth records. The retained engine tests use synthetic data.

`check_reconstruction_contract.R` tests protected output paths, overwrite/symlink refusal,
inert sourcing, interval endpoints, selected-component reporting, unchanged contrast directions
and complete confidence-set retention in S19. The master run independently checks that its inputs
did not change during reconstruction and writes success receipts only after its checks pass.

For the full retained synthetic suite, from `model-fitting/current/` run each `tests/test_*.R`
in a fresh R process. Some require the fitting dependencies listed in REPRODUCIBILITY.md.
This is not the 8,000-replication coverage study or record-level refitting.

## Checks and interpretation

| Check | Scope |
| --- | --- |
| Source integrity | SHA256 and file sizes for the imported files listed in the source manifest. |
| Release contents | Tracked and nonignored candidate files, excluded file types, paths and recognized secret patterns. |
| Integrity fixtures | Rejection of tampered hashes/sizes, duplicate/traversing manifest paths, missing sources, symlinks, excluded/oversized files, binary/invalid UTF-8 text and recognized secret patterns; support for spaces/newlines in filenames. |
| Fitting-runner fixtures | Inert sourcing, the 53-step plan, required-input checks and isolated-workspace safety. |
| Reconstruction contract | Default output, protected paths, overwrite/symlink refusal, interval boundaries and Table 3/S19 reporting. |
| Publication reconstruction | All 27 table files match their references; six PDFs are generated; LRT, df, log-P, Holm adjustments, inverse contrasts, ages and denominators are checked. |
| Engine fixtures | Eleven synthetic suites covering crossover inference, standardization, covariance, model specifications and source-field adapters. |

GitHub Actions runs the portable reconstruction, source checks, runner fixtures and two base-R
numerical suites. [Workflow results](https://github.com/atalaydemiray/smoking-preeclampsia-paradox/actions/workflows/r-code.yml)
identify the tested commit. Generated reconstruction receipts record local run details.

These checks do not refit the national cohorts or verify a raw-archive import. PDF existence and
structure checks do not replace visual inspection. Automated file scans are not a guarantee of
privacy and do not inspect remote history, issues, release assets or LFS storage.
