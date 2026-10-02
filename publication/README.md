# Publication inputs

`Code/supplementary/aggregate_outputs/joint_three_group/` contains full-precision joint-model
summaries, age curves and group support. In `summary_main_summary.csv`, the `events` column for
joint contrasts counts events in the two groups contrasted, not all three fitted groups. Use
`Code/interaction_tests/population_overall.csv` for total fitted population/event counts.

`Code/interaction_tests/` contains the two global model tests, three exploratory contrast tests,
missingness, group characteristics, convergence traces and the original validation receipts.
Raw P values can underflow to zero; use the finite log-P and formatted P columns, not a claim of P=0.

`Code/table_inputs/` contains outcome-stratified descriptive aggregates, covariance-estimator
comparisons and the non-numerical exposure-definition table. `Code/aggregate_inputs/` and
`../results/` supply sensitivity results. `Code/publication_code/` and
`Code/reproduce_figures.R` generate the six publication figures.

`layouts/` contains headings and row labels only. It does not provide numerical result cells.
`reference_tables/` holds the 27 reference CSV tables and is read only after reconstruction to
compare outputs. Table S15 is a definition table, not a statistical calculation. The table
metadata CSV supplies every title and note.

`source_manifest.csv` identifies imported files by source, bytes and SHA256. Original receipt
paths are retained as provenance; the publication driver resolves its own inputs locally.
No records, model RDS objects or manuscript drafts are included. Aggregate reproduction does not
recheck record-level clinical coding or identify a biological causal mechanism.
