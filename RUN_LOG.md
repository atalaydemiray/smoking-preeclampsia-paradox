# Reproduction record, 2 October 2026

## Scope and version

- Public exhibit reconstruction, synthetic/static checks and pre-release audit. Not record-level estimation.
- Baseline commit: `d7096249955e583681167f23cdfa984e9362cd4e`.
- This evidence concerns the uncommitted working-tree revision described in CHANGELOG.md. The
  candidate source fingerprints and generated receipts identify the tested contents; no new
  published commit or version DOI is asserted.
- Manuscript: 1 October AJOG package, including the approved expanded Table 3 and omission footnote.
- Frozen scientific engines, aggregate estimates and `results/` were unchanged. Three main
  crossover estimates remain 33.3, 29.4 and 28.6 years; no model was refitted.
- Execution environment: R 4.6.0, arm64 macOS, Darwin 25.6.0; fitting-test dependencies
  `data.table` 1.18.4, `jsonlite` 2.0.0 and `digest` 0.6.39. See REPRODUCIBILITY.md for the original
  fitting environment and the difference between specification and tested restoration.

## Commands and observed checks

| Check | Command/route | Observed result |
| --- | --- | --- |
| Imported provenance | `python3 tests/check_source_integrity.py` | 221 SHA256/size checks passed. |
| Candidate-tree safety | `python3 tests/check_release_contents.py` | Tracked and nonignored new files passed type/path/secret-pattern checks. Automated flags are not a privacy guarantee. |
| Fail-closed Python gates | `python3 tests/check_integrity_contract.py` | Synthetic tampered-input and unapproved-new-file fixtures were rejected, also under `python -O`. |
| R syntax | Parse every candidate `*.R` | 88 R files parsed. |
| Python syntax | AST-parse every candidate `*.py` | All three Python files parsed. |
| Retained numerical/adapter fixtures | Fresh `Rscript --vanilla` for each `model-fitting/current/tests/test_*.R` | All 11 scripts passed, reporting 674 synthetic checks. |
| Isolated fitting runner | `Rscript --vanilla tests/check_current_runner.R` | Inert sourcing, 53-step plan, 221 synthetic input prerequisites, overwrite/missing-input guards and dry run passed. |
| Actual prepared-workspace preflight | `current_preflight()` against the authors' preparation workspace | All 221 required paths and fitting-package checks passed. File existence is not hash verification or refitting. |
| Reconstruction contract | `Rscript --vanilla tests/check_reconstruction_contract.R` | Protected paths, symlink/overwrite refusal, interval endpoints, contrast directions and complete S19 retention passed. |
| Working-tree reconstruction | `Rscript --vanilla run_all.R output/audit-2026-10-02` | All 27 table CSVs and six vector PDFs rebuilt, with independent LRT/df/log-P/Holm/risk/denominator checks. |
| Separate source export | Copy candidate source files without `.git`, local assistant files, prior output or research inputs; run from another directory with explicit `repository=` | The same 27 tables/six figures rebuilt; original working directory restored. |
| Manuscript agreement | Compare all reference-table CSV cells with the latest AJOG package | All 27 matched, including expanded Table 3. |
| Figure agreement | Extract text from all six regenerated PDFs and compare with the AJOG figures | Text and labels matched; each PDF has one page. No visual inspection performed. |
| Citation metadata | Parse CITATION.cff and compare software/manuscript author lists and required local fields | YAML/consistency checks passed; full CFF JSON Schema validation was not performed. |
| Diff hygiene | `git diff --check` | Passed. |

Reconstruction is deterministic; no sampling, package installation, download, cloud query or
record-level input read is performed by `run_all.R`. Expected table cells match exactly. Other
numerical tolerances are those in `tests/check_publication.R` and the original validation CSVs;
none was relaxed. PDF metadata can vary, so byte identity is not required. Successful-run receipts
record actual elapsed time; no full-estimation runtime or memory measurement is claimed.

## Release-tree/history review

The local audit inventoried the working tree and all 330 locally reachable Git blobs. The one
historical compressed natality-package archive contained 27 text source/metadata/test files, no
record-level binary inputs, no unsafe member paths, and no recognized secret/path flags. No LFS
pointer blobs were found. Existing history, ignored assistant worktrees and local output were not
deleted or rewritten. The pre-change code archive and detailed inventory remain in the private
research workspace, not the public repository.

Read-only GitHub metadata confirmed a public repository, one main branch at the baseline commit,
no releases/assets and an older `v1.0-ajog-submission` tag. The open issue list was empty. A separate
all-state issue-history request failed because of network access, so closed-issue/comment history
was not verified. No remote modifications were performed.

## Remaining before archiving

- Author approval of code/citation/license information and final version number; `1.1.0-dev` is
  intentionally not a completed published release.
- Commit/push the approved diff, then check CI for that exact commit and archive a new matching version.
- User-led visual inspection of all manuscript figures/documents; text extraction is not visual QA.
- Optional full CFF schema validation and review of closed remote issue/comment history.
- Full 53-step prepared-data refitting and clean-room raw-source preparation were not executed.
  State the aggregate-reconstruction boundary in the code-availability statement.
- Create/verify a version-specific Zenodo DOI only after the concrete release is approved.

See REPRODUCIBILITY.md for the exhibit map, data access, environment and archive checklist.
