# Multiplicity reporting clarification, 1 October 2026, 15:55 CEST

This reporting clarification was fixed before completion of the national reduced-model fits and before their final interaction test results were available. It changes no fit, cohort, exposure definition, covariance estimator, or prior result. The parent interaction amendment and running fit code remain byte-for-byte unchanged.

The two global age-by-smoking hypotheses are the primary joint three-group model (8 degrees of freedom) and the secondary binary prepregnancy model (4 degrees of freedom). Report unadjusted and Holm-adjusted p values across these two hypotheses. Apply the correction separately to the likelihood-ratio implementation and to its HC0 Wald robustness check. These are two implementations of the same two scientific questions, not four independent replications or a choice of whichever produces a smaller p value. Holm's procedure does not require these overlapping populations to be independent.

The additional three four-degree-of-freedom Wald contrasts from the joint model (SN versus SS, NN versus SS, SN versus NN) are exploratory. Provide their unadjusted p values and a separate Holm correction across those three contrasts, with the family explicitly named. These tests are not all the same as the three comparisons highlighted in the paper: the latter also include N versus S from the separate prepregnancy model.

Evaluate Holm corrections in log-probability space to preserve extreme tail probabilities that underflow in ordinary double-precision p values. Publish `P<0.001` where appropriate, never `P=0`.

The existing confidence bands remain simultaneous across age within each fitted contrast. They are not jointly simultaneous across all published contrasts. The local crossover intervals and simultaneous sign-inference summaries retain their existing definitions; an interaction p value does not replace either quantity.

The supplementary global-test table and the machine-readable test data will identify the exact multiplicity family for every adjusted p value. This additional interaction analysis remains explicitly post-original-results work, not an original preregistered analysis.
