# Public repository scope

This repository supports the October 2026 manuscript: the joint SS/SN/NN primary model, the binary
S/N secondary model, their age-dependent associations and crossover uncertainty, the formal
interaction tests, and the retained supplementary analyses.

Included are the exact statistical engines and model scripts, source-field adapters, the frozen
natality reader, selected dated analysis amendments, numerical tests, aggregate estimates, table
templates, reference tables and figure code. The supplementary analyses include historical and
fetal-source comparisons with less precise or inconclusive inference; cleanup does not select
results by significance. Known-truth crossover simulations remain separately reproducible.

The public tree excludes superseded duplicate pipelines, withdrawn quantitative-bias calculations,
unused imputation engines, abandoned development/report scripts, old display outputs, duplicate
package archives, completed-run checkpoints and local working configuration. No individual records,
record-level fitted objects, manuscript drafts, meeting transcripts, screenshots or credentials
are distributed. Removed material is preserved in the authors' local research archive. Existing
Git history is not rewritten by this cleanup.

The standalone file `R/06_mi_pooling.R` inside the frozen source tree is a dependency of the
complete-case covariance calculations. Keeping that original helper preserves code provenance;
it does not mean imputation was performed. Some frozen multi-purpose scripts retain unscheduled
branches for previously considered analyses. The explicit 53-step plan selects only retained fits
and required diagnostic checks; do not interpret every dormant branch as a reported analysis.

The supplementary models were added after the initial findings, and the October interaction tests
were added after coauthor review. Neither is represented as an original preregistered analysis.
The interaction protocol and multiplicity clarification are retained unchanged. Withdrawal of the
quantitative-bias scenarios means this study cannot claim that those calculations resolve
live-birth selection, misclassification or residual confounding.

Publication reconstruction uses only aggregates. Record-level refitting requires the prepared
data and provenance receipts described in `model-fitting/README.md`; a standalone raw-download
pipeline is not certified. The local backup and individual-level research inputs are not GitHub
release contents.
