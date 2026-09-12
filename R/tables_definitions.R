# Table S20, the exposure definitions table: which combination of the four cigarette fields
# puts a birth record in the exposed arm and in the reference arm of every comparison reported,
# the three main models and all 31 supplementary fits.

# This table carries no estimates. It restates in one place the arm definitions that the
# model-fitting code applies, so a reader can check any row of Tables 2, 3 and S19 to S23
# against the fields it was built from. The wording is the wording of the submitted table.

table_s20 <- function() {
  n_cols <- 8

  # Every row states the same eight things, so a dropped cell fails here instead of shifting a
  # column in the written table.
  def_row <- function(...) {
    cells <- c(...)
    if (length(cells) != n_cols) {
      stop("Table S20 row ", cells[1], " has ", length(cells), " cells, expected ", n_cols)
    }
    cells
  }

  header <- def_row(
    "Model", "Comparison", "Protocol group", "Population (eligibility)",
    "Exposed (P/T1/T2/T3)", "Reference (P/T1/T2/T3)",
    "Standardization reference (if not just the two arms shown)", "Notes"
  )

  body <- rbind(
    # The three main comparisons every supplementary fit descends from.
    def_row("M1", "Continued vs stopped smoking in T1, women who smoked before pregnancy", "main",
            "P+, T1 known", "+/+/any/any", "+/-/any/any", "the two arms shown", ""),
    def_row("M2", "Smoking before pregnancy and in T1 vs neither period", "main",
            "P known, T1 known", "+/+/any/any", "-/-/any/any", "the two arms shown", ""),
    def_row("M3", "Any prepregnancy smoking vs none, irrespective of T1", "main", "P known",
            "+/any/any/any", "-/any/any/any", "the two arms shown", ""),

    # Fits inside the M1 population: a positive prepregnancy report and a known T1 report.
    def_row("supp_t1dose_1_5", "T1 dose 1-5 cigarettes/day vs 0", "S-B dose", "P+, T1 known",
            "+/+(1-5/day)/any/any", "+/-(0)/any/any", "whole M1 population, all T1 doses pooled",
            "adjusted for prepregnancy dose"),
    def_row("supp_t1dose_6_10", "T1 dose 6-10 cigarettes/day vs 0", "S-B dose", "P+, T1 known",
            "+/+(6-10/day)/any/any", "+/-(0)/any/any", "whole M1 population, all T1 doses pooled",
            "adjusted for prepregnancy dose"),
    def_row("supp_t1dose_11_20", "T1 dose 11-20 cigarettes/day vs 0", "S-B dose", "P+, T1 known",
            "+/+(11-20/day)/any/any", "+/-(0)/any/any",
            "whole M1 population, all T1 doses pooled", "adjusted for prepregnancy dose"),
    def_row("supp_t1dose_21plus", "T1 dose 21 or more cigarettes/day vs 0", "S-B dose",
            "P+, T1 known", "+/+(21+/day)/any/any", "+/-(0)/any/any",
            "whole M1 population, all T1 doses pooled", "adjusted for prepregnancy dose"),
    def_row("supp_t1change_reduced", "T1 dose reduced from prepregnancy dose vs stopped",
            "S-B dose", "P+, T1 and prepregnancy dose known", "+/+(T1 dose < P dose)/any/any",
            "+/-(0)/any/any", "whole M1 population, all dose-change categories pooled",
            "stopped defined as T1 raw dose = 0"),
    def_row("supp_t1change_same_or_more",
            "T1 dose same or higher than prepregnancy dose vs stopped", "S-B dose",
            "P+, T1 and prepregnancy dose known", "+/+(T1 dose >= P dose)/any/any",
            "+/-(0)/any/any", "whole M1 population, all dose-change categories pooled",
            "stopped defined as T1 raw dose = 0"),
    def_row("supp_strict_continued_vs_stopped",
            "Continued vs stopped, no positive later-trimester report", "S-C strict stopping",
            "P+, T1 known", "+/+/any/any", "+/-/(-,u)/(-,u)", "the two arms shown",
            "M1's own contrast; reference narrowed to exclude the relapse group below"),
    def_row("supp_strict_relapse_vs_stopped",
            "T1 negative then a later positive report vs strict stopping",
            "S-E bias illustration", "P+, T1 known", "+/-/(+ or T3+)/(+ or T2+)",
            "+/-/(-,u)/(-,u)", "the two arms shown",
            "exposed group is defined by a later-trimester field; reported as a bias illustration, not an estimate"),
    def_row("supp_timing_stopped_by_T1", "Stopped by T1 vs smoked throughout",
            "S-E bias illustration", "P+, T1 known", "+/-/(-,u)/(-,u)", "+/+/+/+",
            "the two arms shown",
            "comparator requires a positive report in all three periods, so it cannot be observed before 28 weeks"),
    def_row("supp_timing_stopped_in_T2", "Stopped in T2 vs smoked throughout",
            "S-E bias illustration", "P+, T1 known", "+/+/(-,u)/(-,u)", "+/+/+/+",
            "the two arms shown", "same comparator as above"),
    def_row("supp_timing_stopped_in_T3", "Stopped in T3 vs smoked throughout",
            "S-E bias illustration", "P+, T1 known", "+/+/+/(-,u)", "+/+/+/+",
            "the two arms shown", "same comparator as above"),

    # M1's own arms reused, with the outcome or the stratum changed.
    def_row("supp_preterm_primary", "Continued vs stopped in T1, control outcome preterm birth",
            "S-F control outcome", "P+, T1 known (M1 population)", "+/+/any/any", "+/-/any/any",
            "the two arms shown",
            "identical arms to M1; outcome is preterm birth (<37 weeks), not GH/PE"),
    def_row("supp_race_primary_nhw", "Continued vs stopped in T1, non-Hispanic White",
            "S-G race and ethnicity", "M1 population, non-Hispanic White", "+/+/any/any",
            "+/-/any/any", "the two arms shown",
            "identical arms to M1, restricted to stratum; race removed as a covariate"),
    def_row("supp_race_primary_nhb", "Continued vs stopped in T1, non-Hispanic Black",
            "S-G race and ethnicity", "M1 population, non-Hispanic Black", "+/+/any/any",
            "+/-/any/any", "the two arms shown",
            "identical arms to M1, restricted to stratum; race removed as a covariate"),
    def_row("supp_race_primary_hisp", "Continued vs stopped in T1, Hispanic",
            "S-G race and ethnicity", "M1 population, Hispanic", "+/+/any/any", "+/-/any/any",
            "the two arms shown",
            "identical arms to M1, restricted to stratum; race removed as a covariate"),
    def_row("supp_race_primary_other", "Continued vs stopped in T1, other groups combined",
            "S-G race and ethnicity", "M1 population, other groups combined", "+/+/any/any",
            "+/-/any/any", "the two arms shown",
            "identical arms to M1, restricted to stratum; race removed as a covariate"),

    # Fits inside the population with a known prepregnancy report.
    def_row("supp_3lvl_stopped_vs_none", "Stopped by T1 vs no early smoking",
            "S-A three-level early smoking", "P known, T1 known", "+/-/any/any", "-/-/any/any",
            "all three groups (none, stopped, continued) pooled", ""),
    def_row("supp_3lvl_continued_vs_none", "Continued in T1 vs no early smoking",
            "S-A three-level early smoking", "P known, T1 known", "+/+/any/any", "-/-/any/any",
            "all three groups (none, stopped, continued) pooled",
            "same records as M2's exposed and reference arms; reference standardization differs"),
    def_row("supp_clean_prepregnancy",
            "Any prepregnancy smoking vs a clean (all-negative) reference", "S-D clean reference",
            "P known", "+/any/any/any", "-/(-,u)/(-,u)/(-,u)", "the two arms shown",
            "M3's own contrast; reference tightened to exclude any positive T2/T3 report"),
    def_row("supp_clean_broad", "Both periods vs a clean (all-negative) reference",
            "S-D clean reference", "P known, T1 known", "+/+/any/any", "-/-/(-,u)/(-,u)",
            "the two arms shown",
            "M2's own contrast; reference tightened to exclude any positive T2/T3 report"),
    def_row("supp_pdose_1_5", "Prepregnancy dose 1-5 cigarettes/day vs 0", "S-B dose", "P known",
            "+(1-5/day)/any/any/any", "-(0)/any/any/any",
            "whole P-known population, all prepregnancy doses pooled", ""),
    def_row("supp_pdose_6_10", "Prepregnancy dose 6-10 cigarettes/day vs 0", "S-B dose",
            "P known", "+(6-10/day)/any/any/any", "-(0)/any/any/any",
            "whole P-known population, all prepregnancy doses pooled", ""),
    def_row("supp_pdose_11_20", "Prepregnancy dose 11-20 cigarettes/day vs 0", "S-B dose",
            "P known", "+(11-20/day)/any/any/any", "-(0)/any/any/any",
            "whole P-known population, all prepregnancy doses pooled", ""),
    def_row("supp_pdose_21plus", "Prepregnancy dose 21 or more cigarettes/day vs 0", "S-B dose",
            "P known", "+(21+/day)/any/any/any", "-(0)/any/any/any",
            "whole P-known population, all prepregnancy doses pooled", ""),
    def_row("supp_t3_through_vs_none", "Positive in all four windows vs zero in all four",
            "S-E bias illustration", "all four fields known", "+/+/+/+", "-/-/-/-",
            "the two arms shown",
            "the only fit requiring all four fields confirmed on both arms; requires a third-trimester value"),

    # M2's and M3's own arms reused, with the outcome or the stratum changed.
    def_row("supp_preterm_broad", "Both periods vs neither, control outcome preterm birth",
            "S-F control outcome", "P known, T1 known (M2 population)", "+/+/any/any",
            "-/-/any/any", "the two arms shown",
            "identical arms to M2; outcome is preterm birth (<37 weeks), not GH/PE"),
    def_row("supp_race_broad_nhw", "Both periods vs neither, non-Hispanic White",
            "S-G race and ethnicity", "M2 population, non-Hispanic White", "+/+/any/any",
            "-/-/any/any", "the two arms shown",
            "identical arms to M2, restricted to stratum; race removed as a covariate"),
    def_row("supp_race_broad_nhb", "Both periods vs neither, non-Hispanic Black",
            "S-G race and ethnicity", "M2 population, non-Hispanic Black", "+/+/any/any",
            "-/-/any/any", "the two arms shown",
            "identical arms to M2, restricted to stratum; race removed as a covariate"),
    def_row("supp_race_broad_hisp", "Both periods vs neither, Hispanic", "S-G race and ethnicity",
            "M2 population, Hispanic", "+/+/any/any", "-/-/any/any", "the two arms shown",
            "identical arms to M2, restricted to stratum; race removed as a covariate"),
    def_row("supp_race_broad_other", "Both periods vs neither, other groups combined",
            "S-G race and ethnicity", "M2 population, other groups combined", "+/+/any/any",
            "-/-/any/any", "the two arms shown",
            "identical arms to M2, restricted to stratum; race removed as a covariate"),
    def_row("supp_preterm_prepregnancy",
            "Any prepregnancy smoking vs none, control outcome preterm birth",
            "S-F control outcome", "P known (M3 population)", "+/any/any/any", "-/any/any/any",
            "the two arms shown",
            "identical arms to M3; outcome is preterm birth (<37 weeks), not GH/PE; no race-stratum version was authorised")
  )

  # The symbol key belongs with the table, not with the manuscript text, because the arm
  # definitions are unreadable without it.
  footnote <- paste0(
    "Symbol key: + confirmed smoking, - confirmed no smoking, u unknown or not reported, any ",
    "unrestricted (may be +, - or u). P/T1/T2/T3 = before-pregnancy, first-, second- and ",
    "third-trimester cigarette fields. This table states only which records fall into which ",
    "arm; standardized risks, risk ratios and crossover ages for each model are in Tables 2, ",
    "3 and S19 to S23. Rows labelled a bias illustration are shown to demonstrate a structural ",
    "artefact and are not reported as estimates of association."
  )

  rows <- rbind(header, body, c(footnote, rep("", n_cols - 1)))
  dimnames(rows) <- NULL
  check_people_first(rows, "Table S20")
  rows
}
