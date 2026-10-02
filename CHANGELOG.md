# Changelog

## Unreleased

- Replaced the three integrity-check scripts with R equivalents using bundled SHA256 support;
  removed the Python runtime requirement and updated CI and validation commands.
- Added fail-closed synthetic fixtures for manifest paths, file sizes/types, symlinks, text encoding
  and recognized secret patterns, alongside the retained source-tampering checks.
- Clarified NCHS/NBER data access and the frozen import dependency.
- Removed general package tutorials and showcase wording; documentation focuses on study reproduction.
- Reworked the README and supporting documentation for external researchers.
- Consolidated validation and analysis-scope guidance; removed redundant handoff documents.
- Simplified the RStudio project settings and removed unused table-writing helpers.
- Updated the bundled reader's documentation and corrected broken links; reader code is unchanged.
- Changed the default generated-output directory to `output/reproduction/`.
- Added methods and data-dictionary fingerprints to reconstruction receipts.

No scientific estimates or statistical engines changed.

## 2026-10-02

- Expanded Table 3 with null-age confidence sets, simultaneous sign regions and explanatory notes.
  Its selected null-age component is computed from the full-precision crossover; complete sets remain in S19.
- Added protected output paths, explicit repository selection, working-directory restoration,
  input-stability checks and successful-run environment/input/output receipts to the master command.
- Added interval-boundary, confidence-component, reference-direction, overwrite and symlink tests;
  included them in GitHub Actions. The safety scan now includes nonignored candidate release files.
- Made integrity-gate failures explicit and
  tested rejection of deliberately altered source inputs and unapproved release files.
- Added a data dictionary, environment/data-access instructions, an exhibit map and contribution guidance.

No fitted estimates, cohorts, model formulas, reference-group definitions or numerical engines changed.

## 2026-10-01

- Updated the publication entry point from three pairwise main models to the current joint primary
  model and secondary prepregnancy model. SS and S are the reporting references.
- Added the two global likelihood-ratio interaction tests, HC0 Wald tests, separate Holm families,
  group characteristics, common-denominator missingness and complete crossover uncertainty.
- Added model-fitting scripts, dated method amendments and statistical engines at their original paths.
- Reconstructed 27 publication table files and six figures; excluded withdrawn quantitative-bias
  displays and redundant numerical-check rows from the active publication workflow.
- Added cell-level reference comparisons, independent aggregate checks, source SHA256 verification,
  and a safe 53-step serial RStudio rerun with input preflight and isolated outputs.
- Removed superseded display pipelines and outputs not used by the publication.
- Documented that the raw-source route still requires historical preparation/provenance inputs;
  aggregate success is not represented as a clean-room full-data rerun.
- Added a public-file safety gate and trimmed table helpers to the functions used by the current paper.

The main-model crossover ages are 33.3, 29.4 and 28.6 years. The 31.8-year estimate belongs to the
separate within-group sensitivity analysis.
