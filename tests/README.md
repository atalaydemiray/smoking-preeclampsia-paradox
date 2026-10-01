# Validation scope

Run from the repository root:

```sh
python3 tests/check_source_integrity.py
python3 tests/check_release_contents.py
Rscript --vanilla tests/check_current_runner.R
Rscript --vanilla run_all.R output/my_validation
```

Use a new output directory for each reconstruction. The release-content check examines the Git
index, so stage the intended revision before using it locally. No test downloads or reads study
birth records. The retained engine tests use synthetic data.

Local checks on 1 October 2026 passed:

- All 220 retained source/aggregate/reference-file SHA256 checks.
- Parsing of 87 R files. All 71 frozen R sources with research-workspace counterparts were
  byte-identical to those current sources.
- All 11 retained numerical/source-adapter test scripts passed, including crossover inference,
  standardization, covariance, finite-difference, model specification and synthetic field-coding checks.
- Inert sourcing, the 53-step plan, all 221 required prepared-input paths, missing-input and
  overwrite guards, and preparation/dry-run behavior using synthetic filesystem fixtures.
- An existence/package preflight against the actual prepared analysis workspace.
- Reconstruction from a separate repository copy without the research workspace: all cells in
  27 table files matched the October reference tables; all six vector PDF figures regenerated.
- Independent aggregate checks of likelihood-ratio statistics, degrees of freedom, log-P values,
  Holm adjustment, inverse contrasts, age coverage and denominators.

These checks are not a fresh 30-million-record analysis. The complete 53-step refit, a clean-room
raw-archive import, and author visual approval of the figures were not performed in this code-sync
task. GitHub Actions runs the portable reconstruction, release checks and two base-R numerical
test suites. Consult the workflow run for the status of a particular commit.
