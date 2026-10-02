# Contributing and reporting corrections

Report code or documentation problems through the repository's GitHub Issues. Include the
commit/version, command, operating system, R version, expected behavior and a minimal synthetic
example. Do not upload individual birth records, private correspondence, credentials or local
workspace files. Report sensitive material privately to
[Atalay Demiray](mailto:atalay.demiray@yale.edu) rather than posting it in an issue.

Before proposing a change, read README.md, METHODS.md and REPRODUCIBILITY.md. Preserve raw inputs,
approved reference outputs and the frozen source files identified in publication/source_manifest.csv.
Use a new output directory. Keep code changes small and document why they are needed. Be respectful
of collaborators and distinguish questions, proposed changes and verified corrections.

Run the commands in tests/README.md. Each bug fix needs a regression test. A numerical difference
must be investigated against its upstream estimate, not resolved by changing a reference table
until the test passes. An approved display-only change may update the corresponding layout,
metadata and reference table, with a dated changelog entry and unchanged full-precision estimates.

Changing the population, exposure definition, outcome, covariates, estimand, spline basis or
inference requires a written scientific amendment and the study owner's agreement before refitting.
Record what was original, previous and corrected, why it changed, and which validations were rerun.
Sensitivity models on overlapping births are not independent replication.

OpenAI Codex supported code development and debugging. The authors are responsible for reviewing
the code, validating the results and maintaining the research record.
