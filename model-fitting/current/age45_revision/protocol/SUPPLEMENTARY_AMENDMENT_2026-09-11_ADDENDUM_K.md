# Addendum K, 19 September 2026: one reference group per model. SS is the reference of the primary model and S of the before-pregnancy model

Parent: `SUPPLEMENTARY_AMENDMENT_2026-09-11.md`; addenda G (three-group presentation and exposure codes),
H (the joint model), I (diagnostic decomposition) and J (the first-trimester margin).

## The change

Owner's decision, 19 September 2026. Until now the primary model reported SS against two references, SN and
NN, and the before-pregnancy model reported S against N. Every model now has **one reference and one or more
comparators**, so that the two models are read the same way:

| Model | Reference | Comparators | Question asked |
|---|---|---|---|
| Primary (joint SS, SN, NN) | **SS** | SN, NN | Compared with women who smoked before pregnancy and in the first trimester, what is the recorded risk for women who reported none in the first trimester, and for women who reported none in either window? |
| Before pregnancy | **S** | N | Compared with women who smoked before pregnancy, what is the recorded risk for women who did not? |

Table 2 carries the risk ratio and risk difference on the **same row as the comparator group**, with the
reference row marked as such. Table 3, Figure 2, Figure 3, the age-specific supplementary tables, the
abstract, the Results and the Comment all report the reversed direction.

## This is a re-expression, not a new analysis. No model is refitted.

For groups g and r with standardized risks R_g(a) and R_r(a) from the same fit and the same standardization
population, reversing which is the reference gives, exactly:

- **Risk difference.** RD(g vs r) = −RD(r vs g). The standard error is unchanged, because Var(−X) = Var(X);
  the interval is the old interval negated with its bounds swapped.
- **Risk ratio.** RR(g vs r) = 1 / RR(r vs g). On the log scale the estimate is negated and its standard
  error is unchanged, so the interval is the reciprocal of the old interval with its bounds swapped. **A
  risk ratio is not sign-flipped; 0.941 becomes 1.063, not −0.941.**
- **Crossover age.** The conditional contrast is negated, so its roots are unchanged: every crossover age,
  its local 95% CI and the whole 95% null-age set are identical. The implicit-delta standard error is also
  unchanged, because both b(a) and h'(a) are negated and the ratio's magnitude is preserved.
- **Simultaneous sign regions.** These swap: ages at which the band supported a lower recorded risk for the
  old exposed group now support a higher recorded risk for the new comparator, and vice versa. Whether a
  reversal is established is unchanged.
- **Standardized risks.** Unchanged; only which one is labelled the reference changes.

Derivation script: `age45_revision/scripts/39_flip_reference.R`. It reads the receipted outputs and the
fitted models, negates the relevant coefficient block, re-runs `analyze_age_crossover` on the negated block
(rather than editing its output), transforms the standardized tables by the identities above, and writes
new contrast folders. It **asserts** on every row that RR_new x RR_old = 1, RD_new + RD_old = 0 and the
crossover is identical to within 1e-9, and refuses to write if any assertion fails. Nothing under
`outputs/models`, `submission_staging` or the existing joint folders is modified.

## Quantities that are deliberately NOT reversed

- **The quantitative bias analyses (Tables S11 to S15, Figure S2).** They ask whether a bias could have
  produced the apparent protection associated with smoking, so they are stated in the smoking-exposed
  direction throughout. E-values are invariant to the reversal by construction (the E-value of a risk ratio
  below one is computed from its reciprocal). The tables state their direction explicitly.
- **The released two-arm sensitivity fits** (`primary_main`, `broad_main`, the specification, adjustment,
  fetal-inclusive and race and ethnicity rows) keep their released orientation; their crossover ages, which
  are what Figure 3 and Table S5 display, are unaffected by orientation.
- **The first-trimester margin (addendum J)** keeps smoking in the first trimester as its exposed group,
  because its reference is a pooled group and no single smoking pattern defines it.

## Interpretation recorded with the decision

Reversing the reference makes the comparators the groups that did not smoke in the window of interest, so
at younger ages they carry the **higher** recorded risk. The recorded risk of the SN group is the highest of
the three at every age, and the manuscript already attributes this to women who stop after an early
complication rather than to any harm of stopping. The Comment must therefore continue to state that these
are recorded associations in birth-certificate data, that reverse causation by indication is the leading
explanation for the SN group, and that nothing here supports continued smoking. **No wording that could be
read as advising against stopping smoking may appear in any document.**
