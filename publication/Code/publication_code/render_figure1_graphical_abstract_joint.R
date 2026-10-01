# Figure 1 as a graphical abstract for the three-group presentation (protocol addenda G and H, 19 Sep
# 2026): who was studied (A), how the two early cigarette fields define the groups and contrasts (B),
# and what was found (C). Aggregate inputs only, base R, no refitting: every count is read from the
# joint-model summaries, the released before-pregnancy fit and the released flow table, and checked
# against the receipts before drawing.
#
# Inputs:
#   <joint>/summary/main_summary.csv, <joint>/summary/main_age_estimates.csv, <joint>/age_group_support.csv
#   age45_revision/outputs/models/prepregnancy_main/age_arm_support.csv
#   age45_revision/submission_staging/Code/aggregate_inputs/main_overall.csv   (S vs N eligible and excluded)
#   age45_revision/submission_staging/Tables/supplement_common_clinical_flow_by_year.csv
# From 01_Analysis: Rscript --vanilla age45_revision/publication_code/render_figure1_graphical_abstract_joint.R [joint_dir] [out_dir]
render_figure1_graphical_abstract_joint <- function(joint = "age45_revision/outputs/supplementary/joint_3group",
                                                    out_dir = "age45_revision/outputs/supplementary/figures",
                                                    preview_png = NULL) {
  rd <- function(p) read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
  S <- file.path(joint, "summary")
  summ <- rd(file.path(S, "main_summary.csv")); ages <- rd(file.path(S, "main_age_estimates.csv"))
  grp <- rd(file.path(joint, "age_group_support.csv"))
  pre_arm <- rd("age45_revision/outputs/models/prepregnancy_main/age_arm_support.csv")
  overall <- rd("age45_revision/submission_staging/Code/aggregate_inputs/main_overall.csv")
  flow <- rd("age45_revision/submission_staging/Tables/supplement_common_clinical_flow_by_year.csv"); flow <- flow[flow$oe_threshold == 20, ]
  stopifnot(setequal(unique(flow$year), 2016:2024))
  row_of <- function(id) { r <- summ[summ$model_id == id, ]; stopifnot(nrow(r) == 1); r }
  # Addendum K (19 September 2026): one reference group per model. SS is the reference of the primary
  # model and S of the before-pregnancy model, so the figure reads the reversed rows of the same
  # summary. In these rows A0 is the reference group and A1 the comparator, the other way round from
  # the superseded rows. Nothing is refitted; the guard below re-checks the exact re-expression.
  ssn <- row_of("sn_vs_ss"); ss <- row_of("nn_vs_ss"); pre <- row_of("n_vs_s")
  for (p in list(c("sn_vs_ss", "joint_ss_vs_sn"), c("nn_vs_ss", "joint_ss_vs_nn"), c("n_vs_s", "prepregnancy_main"))) {
    a <- row_of(p[1]); b <- row_of(p[2])
    stopifnot(abs(a$rr * b$rr - 1) < 1e-9, abs(a$rd_per1000 + b$rd_per1000) < 1e-9,
              abs(a$crossover_age - b$crossover_age) < 1e-9)
  }
  G <- sapply(c("NN", "SN", "SS"), function(g) c(n = sum(grp$n[grp$group == g]), events = sum(grp$events[grp$group == g])))
  stopifnot(sum(G["n", ]) == ss$fit_n, G["n", "SS"] == ss$A0, G["n", "NN"] == ss$A1, G["n", "SN"] == ssn$A1)
  P <- sapply(c("0", "1"), function(a) c(n = sum(pre_arm$n[pre_arm$A == as.integer(a)]), events = sum(pre_arm$events[pre_arm$A == as.integer(a)])))
  ov_pre <- overall[overall$contrast == "prepregnancy", ]; stopifnot(nrow(ov_pre) == 1, sum(P["n", ]) == ov_pre$n, sum(P["events", ]) == ov_pre$events)
  stage <- function(s, col) sum(flow[flow$stage == s, col])
  n_source <- stage("source_records", "n_entering")
  excl <- c(us_residents = stage("us_residents", "n_excluded"), singleton = stage("singleton", "n_excluded"),
            maternal_age_15_45 = stage("maternal_age_15_45", "n_excluded"),
            known_oe_at_least_threshold = stage("known_oe_at_least_threshold", "n_excluded"),
            known_no_prepregnancy_hypertension = stage("known_no_prepregnancy_hypertension", "n_excluded"))
  n_clinical <- stage("known_no_prepregnancy_hypertension", "n_retained")
  stopifnot(n_source - sum(excl) == n_clinical, ov_pre$eligible_n <= n_clinical)
  # Eligible births minus the births a model cannot classify: for S vs N that is the before-pregnancy
  # cigarette field unknown. Stating it keeps panel A's three numbers reconcilable (eligible births,
  # this step, the model's eligible count). The step differs by model, so it is named for S vs N.
  pre_unknown <- n_clinical - ov_pre$eligible_n; stopifnot(pre_unknown >= 0)
  other_pattern <- ov_pre$n - ss$fit_n; stopifnot(other_pattern >= 0)
  age_rr <- function(id, a) { r <- ages[ages$model_id == id & ages$age == a, ]; stopifnot(nrow(r) == 1); r$rr }
  num <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  per1000 <- function(e, n) sprintf("%.1f", 1000 * e / n)
  f2 <- function(x) sprintf("%.2f", x); f1 <- function(x) sprintf("%.1f", x); f3 <- function(x) sprintf("%.3f", x)
  ink <- "#222222"; mute <- "#6B6B6B"; rule <- "#BFBFBF"; paper <- "#F5F5F2"; brown <- "#8A4B12"
  ccol <- c(ss_sn = "#D55E00", ss_nn = "#0072B2", pre = "#009E73")     # contrasts: SN vs SS, NN vs SS, N vs S
  gcol <- c(NN = "#6B6B6B", SN = "#D55E00", SS = "#0072B2")
  plus_fill <- "#3A3A3A"; zero_fill <- "#FFFFFF"; any_fill <- "#E3E3E3"; onset_fill <- "#FBE9D7"
  W <- 12.4; H <- 7.6
  open <- function(path, png = FALSE) {
    if (png) grDevices::png(path, width = W, height = H, units = "in", res = 170, type = "quartz", family = "Helvetica")
    else if (Sys.info()[["sysname"]] == "Darwin") grDevices::quartz(type = "pdf", file = path, width = W, height = H, family = "Helvetica")
    else grDevices::cairo_pdf(path, width = W, height = H, family = "Helvetica")
  }
  contrasts <- list(ss_sn = list(r = ssn, id = "sn_vs_ss", lab = "SN vs SS", sub = "none in the first trimester vs both periods"),
                    ss_nn = list(r = ss, id = "nn_vs_ss", lab = "NN vs SS", sub = "neither period vs both periods"),
                    pre = list(r = pre, id = "n_vs_s", lab = "N vs S", sub = "no smoking before pregnancy vs any"))
  cx <- sapply(contrasts, function(k) c(age = k$r$crossover_age, lo = k$r$delta_lower, hi = k$r$delta_upper,
                                        rev = as.numeric(isTRUE(as.logical(k$r$simultaneous_reversal)))))
  stopifnot(!anyNA(cx["age", ]))
  draw <- function() {
    par(mar = c(0, 0, 0, 0), family = "sans", xaxs = "i", yaxs = "i")
    plot.new(); plot.window(xlim = c(0, 124), ylim = c(0, 76))
    tx <- function(x, y, s, cex = .72, col = ink, adj = c(0, .5), font = 1, ...) text(x, y, s, cex = cex, col = col, adj = adj, font = font, xpd = NA, ...)
    box_ <- function(x0, y0, x1, y1, fill = "white", border = rule, lwd = .8) rect(x0, y0, x1, y1, col = fill, border = border, lwd = lwd)
    # ---------------------------------------------------------------- title band
    box_(0, 68.5, 124, 76, fill = paper, border = NA)
    tx(2, 73.5, "Reframing the smoking-preeclampsia paradox: an age-related reversal in 30.1 million United States birth records", cex = 1.14, font = 2)
    # The reference group of each model is named in panel B and on the panel C axis; naming it here as
    # well overflows the title band at this cex, so the subtitle stays orientation-neutral.
    tx(2, 70.6, paste0("US natality public-use files 2016 to 2024; singleton live births; maternal ages 15 to 45; the primary model of three groups (SS, SN, NN) ",
                       "and the before-pregnancy model (S vs N); adjusted, age-specific standardized risks."), cex = .74, col = mute)
    segments(0, 68.5, 124, 68.5, col = rule)
    # ---------------------------------------------------------------- panel A: who was studied
    tx(2, 65.6, "A", cex = 1.1, font = 2); tx(4.6, 65.6, "Who was studied", cex = .95, font = 2)
    box_(2, 58.6, 39.5, 63.2, fill = "white", border = ink, lwd = 1)
    tx(20.75, 60.9, paste0(num(n_source), " birth records, 2016 to 2024"), cex = .95, font = 2, adj = c(.5, .5))
    spine_x <- 8; y_top <- 58.6; y_bot <- 35.4
    segments(spine_x, y_top, spine_x, y_bot, col = ink, lwd = 1.2)
    ex_lab <- c("Not resident in the United States", "Not a singleton birth", "Maternal age outside 15 to 45",
                "Gestational age unknown or not 20 to 47 weeks", "Chronic hypertension recorded or unknown")
    ys <- seq(56.4, 38.2, length.out = 5)
    for (k in 1:5) {
      segments(spine_x, ys[k], spine_x + 3.2, ys[k], col = ink, lwd = .9)
      polygon(spine_x + 3.2 + c(0, -.9, -.9), ys[k] + c(0, .55, -.55), col = ink, border = NA)
      tx(spine_x + 3.9, ys[k] + .95, paste0("Excluded ", num(excl[k])), cex = .72, font = 2)
      tx(spine_x + 3.9, ys[k] - .75, ex_lab[k], cex = .62, col = mute)
    }
    polygon(spine_x + c(0, -.75, .75), y_bot + c(0, 1.2, 1.2), col = ink, border = NA)
    box_(2, 30.4, 39.5, 35.0, fill = "white", border = ink, lwd = 1)
    tx(20.75, 32.7, paste0(num(n_clinical), " eligible births"), cex = .95, font = 2, adj = c(.5, .5))
    tx(2, 29.55, paste0(num(pre_unknown), " with before-pregnancy smoking unknown are not eligible for S vs N"), cex = .58, col = mute)
    tx(2, 28.2, "Complete-case populations of the two models:", cex = .66, col = mute)
    # three-group model
    rect(2, 20.9, 2.9, 26.9, col = ccol["ss_nn"], border = NA)
    tx(3.8, 26.0, "Primary model: SS, SN and NN", cex = .72, font = 2, col = ccol["ss_nn"])
    tx(3.8, 24.5, paste0(num(ss$fit_n), " complete cases"), cex = .7)
    tx(3.8, 23.1, paste0("SS ", num(G["n", "SS"]), "   SN ", num(G["n", "SN"]), "   NN ", num(G["n", "NN"])), cex = .66, col = mute)
    tx(3.8, 21.8, paste0(num(other_pattern), " births with another early pattern (smoking in T1 only, or T1 unknown)"), cex = .56, col = mute)
    tx(3.8, 20.7, "are in the S vs N population only.", cex = .56, col = mute)
    # before-pregnancy model
    rect(2, 13.2, 2.9, 19.2, col = ccol["pre"], border = NA)
    tx(3.8, 18.3, "Before-pregnancy model: S vs N", cex = .72, font = 2, col = ccol["pre"])
    left <- paste0("Eligible ", num(ov_pre$eligible_n)); tx(3.8, 16.8, left, cex = .7)
    ax <- 3.8 + strwidth(left, cex = .7) + .7
    segments(ax, 16.8, ax + 1.05, 16.8, col = ink, lwd = .9); polygon(ax + 1.05 + c(.6, 0, 0), 16.8 + c(0, .36, -.36), col = ink, border = NA)
    tx(ax + 2.3, 16.8, paste0("complete cases ", num(ov_pre$n)), cex = .7)
    tx(3.8, 15.4, paste0(num(ov_pre$excluded_n), " (", f1(100 * ov_pre$excluded_n / ov_pre$eligible_n), "%) excluded for missing covariates"), cex = .62, col = mute)
    tx(3.8, 14.0, paste0("S ", num(P["n", "1"]), "   N ", num(P["n", "0"])), cex = .66, col = mute)
    tx(2, 9.6, "The two populations overlap; counts are births, not unique mothers.", cex = .62, col = mute)
    tx(2, 8.2, "Complete case on the nine model covariates; no imputation.", cex = .62, col = mute)
    # ---------------------------------------------------------------- panel B: how smoking was defined
    segments(41.3, 2, 41.3, 68.5, col = rule); segments(82.7, 2, 82.7, 68.5, col = rule)
    tx(43.3, 65.6, "B", cex = 1.1, font = 2); tx(45.9, 65.6, "How the groups were defined", cex = .95, font = 2)
    cx0 <- 45.2; cw <- 8.6; gap <- .5
    cells_x <- cx0 + (0:3) * (cw + gap); nx <- cells_x[2] + cw + 1.4
    flab <- c("Before\npregnancy (P)", "First\ntrimester (T1)", "Second\ntrimester (T2)", "Third\ntrimester (T3)")
    t23_mid <- (cells_x[3] + cells_x[4] + cw) / 2
    box_(cells_x[3], 53.1, cells_x[4] + cw, 62.9, fill = onset_fill, border = NA)
    tx(t23_mid, 61.75, "20 weeks onward: GH/PE window", cex = .6, col = brown, adj = c(.5, .5), font = 2)
    for (k in 1:4) {
      box_(cells_x[k], 57.2, cells_x[k] + cw, 60.4, fill = if (k <= 2) "white" else onset_fill, border = if (k <= 2) ink else "#D8B48F")
      tx(cells_x[k] + cw / 2, 58.8, flab[k], cex = .58, adj = c(.5, .5), col = if (k <= 2) ink else brown)
    }
    steps <- c("Overlap the outcome", "(GH/PE is recorded from 20 weeks)", "Do not define a group")
    sy <- c(56.2, 55.05, 53.9)
    for (k in 1:3) tx(t23_mid, sy[k], steps[k], cex = .56, col = brown, adj = c(.5, .5), font = if (k == 2) 1 else 2)
    tx(cx0, 51.5, "Code: first letter before pregnancy, second first trimester; S = cigarettes reported, N = none.", cex = .58, col = ink, font = 3)
    cell <- function(x, y, kind) {
      h <- 2.3; fill <- switch(kind, plus = plus_fill, zero = zero_fill, any = any_fill)
      rect(x, y - h / 2, x + cw, y + h / 2, col = fill, border = if (kind == "zero") ink else NA, lwd = .9)
      lab <- switch(kind, plus = "smoked", zero = "0 cigarettes", any = "any / unknown")
      text(x + cw / 2, y, lab, cex = .58, col = if (kind == "plus") "white" else ink, font = if (kind == "any") 3 else 1)
    }
    # block 1: the three groups of the joint model
    yt <- 49.7; yb <- yt - 22.6
    box_(43.3, yb, 81.2, yt, fill = "white", border = rule); rect(43.3, yb, 44.3, yt, col = ccol["ss_nn"], border = NA)
    tx(45.2, yt - 1.25, "Primary model, one fit: SN and NN each compared with SS", cex = .7, font = 2, col = ccol["ss_nn"])
    glab <- c(SS = "SS: women who smoked before pregnancy and in T1 (reference)", SN = "SN: women who smoked before pregnancy, none in T1", NN = "NN: women who reported none in either window")
    gcells <- list(SS = c("plus", "plus"), SN = c("plus", "zero"), NN = c("zero", "zero"))
    yg <- yt - c(4.4, 8.6, 12.8)
    for (i in seq_along(yg)) {
      g <- names(yg)[i] <- c("SS", "SN", "NN")[i]
      tx(cx0, yg[i] + 1.85, glab[[g]], cex = .6, font = 2, col = gcol[[g]])
      for (k in 1:2) cell(cells_x[k], yg[i], gcells[[g]][k])
      tx(nx, yg[i] + .6, paste0(num(G["n", g]), " births"), cex = .66, font = 2)
      tx(nx, yg[i] - .6, paste0(num(G["events", g]), " GH/PE (", per1000(G["events", g], G["n", g]), " per 1,000)"), cex = .58, col = mute)
    }
    tx(45.2, yb + 4.2, paste0("SN vs SS, overall: RR ", f3(ssn$rr), " (", f3(ssn$rr_lower), " to ", f3(ssn$rr_upper), "), RD ", sprintf("%+.2f", ssn$rd_per1000), " per 1,000"), cex = .6, col = ccol["ss_sn"], font = 2)
    tx(45.2, yb + 2.7, paste0("NN vs SS, overall: RR ", f3(ss$rr), " (", f3(ss$rr_lower), " to ", f3(ss$rr_upper), "), RD ", sprintf("%+.2f", ss$rd_per1000), " per 1,000"), cex = .6, col = ccol["ss_nn"], font = 2)
    tx(45.2, yb + 1.2, "All three groups standardized to the pooled population, so the risks are on one scale (Table 2).", cex = .56, col = mute)
    # block 2: before pregnancy
    yt2 <- yb - 1.2; yb2 <- yt2 - 12.4
    box_(43.3, yb2, 81.2, yt2, fill = "white", border = rule); rect(43.3, yb2, 44.3, yt2, col = ccol["pre"], border = NA)
    tx(45.2, yt2 - 1.25, "Before-pregnancy model: N vs the reference S, irrespective of T1", cex = .7, font = 2, col = ccol["pre"])
    ye <- yt2 - 4.4; yr <- yt2 - 8.4
    tx(cx0, ye + 1.85, "S: women who smoked before pregnancy (reference)", cex = .6, font = 2); tx(cx0, yr + 1.85, "N: women who did not smoke before pregnancy", cex = .6, font = 2)
    cell(cells_x[1], ye, "plus"); cell(cells_x[2], ye, "any"); cell(cells_x[1], yr, "zero"); cell(cells_x[2], yr, "any")
    tx(nx, ye + .6, paste0(num(P["n", "1"]), " births"), cex = .66, font = 2); tx(nx, ye - .6, paste0(num(P["events", "1"]), " GH/PE (", per1000(P["events", "1"], P["n", "1"]), " per 1,000)"), cex = .58, col = mute)
    tx(nx, yr + .6, paste0(num(P["n", "0"]), " births"), cex = .66, font = 2); tx(nx, yr - .6, paste0(num(P["events", "0"]), " GH/PE (", per1000(P["events", "0"], P["n", "0"]), " per 1,000)"), cex = .58, col = mute)
    tx(45.2, yb2 + 1.2, paste0("N vs S, overall: RR ", f3(pre$rr), " (", f3(pre$rr_lower), " to ", f3(pre$rr_upper), "), RD ", sprintf("%+.2f", pre$rd_per1000), " per 1,000"), cex = .6, col = ccol["pre"], font = 2)
    tx(43.3, 8.2, "Dark = smoked; white = 0 cigarettes; grey = any value or unknown.", cex = .56, col = mute)
    tx(43.3, 7.0, "Counts here are crude; RR, RD and the curves in C are adjusted.", cex = .56, col = mute)
    tx(43.3, 5.8, "The overall RR and RD average opposite age-specific associations (see C).", cex = .56, col = mute, font = 3)
    # ---------------------------------------------------------------- panel C: what was found
    tx(84.7, 65.6, "C", cex = 1.1, font = 2); tx(87.3, 65.6, "What was found", cex = .95, font = 2)
    px0 <- 89.7; px1 <- 121.5; py0 <- 34.5; py1 <- 62
    xs <- function(a) px0 + (a - 15) / 30 * (px1 - px0)
    allrr <- unlist(lapply(contrasts, function(k) { z <- ages[ages$model_id == k$id, ]; c(z$rr_lower, z$rr_upper) }))
    lo <- log(max(0.6, min(allrr) * .98)); hi <- log(min(1.6, max(allrr) * 1.02))
    ysc <- function(r) py0 + (log(r) - lo) / (hi - lo) * (py1 - py0)
    rect(px0, py0, px1, py1, col = "white", border = NA)
    for (r in c(.7, .8, .9, 1, 1.1, 1.2, 1.3, 1.4)) if (log(r) > lo && log(r) < hi) {
      segments(px0, ysc(r), px1, ysc(r), col = if (r == 1) ink else "#E6E6E6", lwd = if (r == 1) 1 else .6)
      tx(px0 - .6, ysc(r), f2(r), cex = .6, col = mute, adj = c(1, .5))
    }
    for (a in seq(15, 45, 5)) { segments(xs(a), py0, xs(a), py0 - .6, col = mute, lwd = .6); tx(xs(a), py0 - 1.6, a, cex = .6, col = mute, adj = c(.5, .5)) }
    segments(px0, py0, px1, py0, col = mute, lwd = .6)
    tx((px0 + px1) / 2, py0 - 3.2, "Maternal age (years)", cex = .66, adj = c(.5, .5))
    tx(px0 - 4.4, (py0 + py1) / 2, "Adjusted RR, comparator vs its reference group (log scale)", cex = .62, adj = c(.5, .5), srt = 90)
    tx(px0 + .6, ysc(1) + 2.6, "Higher recorded risk", cex = .58, col = mute, font = 3); tx(px0 + .6, ysc(1) + 1.4, "in the comparator", cex = .58, col = mute, font = 3)
    tx(px0 + .6, ysc(1) - 1.4, "Lower recorded risk", cex = .58, col = mute, font = 3); tx(px0 + .6, ysc(1) - 2.6, "in the comparator", cex = .58, col = mute, font = 3)
    clip_ <- function(v) pmin(pmax(v, exp(lo)), exp(hi))
    for (k in names(contrasts)) {
      z <- ages[ages$model_id == contrasts[[k]]$id, ]; z <- z[order(z$age), ]; stopifnot(identical(as.integer(z$age), 15:45))
      polygon(c(xs(z$age), rev(xs(z$age))), c(ysc(clip_(z$rr_lower)), rev(ysc(clip_(z$rr_upper)))), col = adjustcolor(ccol[[k]], .12), border = NA)
    }
    for (k in names(contrasts)) {
      z <- ages[ages$model_id == contrasts[[k]]$id, ]; z <- z[order(z$age), ]
      lines(xs(z$age), ysc(clip_(z$rr)), col = ccol[[k]], lwd = 2.2)
      segments(xs(max(15, cx["lo", k])), ysc(1), xs(min(45, cx["hi", k])), ysc(1), col = ccol[[k]], lwd = 3)
      points(xs(cx["age", k]), ysc(1), pch = 21, bg = if (cx["rev", k] == 1) ccol[[k]] else "white", col = ccol[[k]], lwd = 1.6, cex = 1.1)
    }
    tx(84.7, 27.6, "Crossover age, years (local 95% CI)", cex = .68, font = 2)
    yl <- c(24.9, 21.3, 17.7)
    for (i in seq_along(contrasts)) {
      k <- names(contrasts)[i]
      rect(84.7, yl[i] - 1.35, 85.6, yl[i] + 1.35, col = ccol[[k]], border = NA)
      tx(86.5, yl[i] + .75, paste0(contrasts[[k]]$lab, ": ", contrasts[[k]]$sub), cex = .66, font = 2, col = ccol[[k]])
      tx(86.5, yl[i] - .75, paste0(f1(cx["age", k]), " (", f1(cx["lo", k]), " to ", f1(cx["hi", k]), ") y;  RR ",
                                    f2(age_rr(contrasts[[k]]$id, 20)), " at 20, ", f2(age_rr(contrasts[[k]]$id, 40)), " at 40",
                                    if (cx["rev", k] == 1) "" else ";  reversal not established"), cex = .6)
    }
    box_(84.7, 6.4, 122, 15.2, fill = paper, border = NA)
    tx(86.2, 13.6, "Take-home", cex = .74, font = 2)
    rng <- range(cx["age", ])
    # Addendum K: the comparators are the groups that did not smoke in the window, so below the
    # crossover they carry the higher recorded risk. The caveat is kept on the figure so the panel
    # cannot be read on its own as advising against stopping smoking.
    lines_ <- c(paste0("Below the crossover (", f1(rng[1]), " to ", f1(rng[2]), " years) the comparison"),
                "groups had more recorded GH/PE than the women who smoked;",
                "above it, less. Overall estimates average the two and conceal",
                "the reversal. These are recorded associations; cessation",
                "remains a priority at every reproductive age.")
    for (k in seq_along(lines_)) tx(86.2, 12.2 - 1.35 * (k - 1), lines_[k], cex = .66)
    tx(84.7, 5.3, "Bands: pointwise 95% CI (HC0); markers at 1: fitted crossover with local 95% CI", cex = .5, col = mute)
    tx(84.7, 4.3, "(filled: opposite directions established by the simultaneous band; open: not established).", cex = .5, col = mute)
    tx(84.7, 3.3, "GH/PE: gestational hypertension or preeclampsia. SS, SN, NN, S, N: exposure codes (panel B).", cex = .5, col = mute)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  pdf_path <- file.path(out_dir, "Figure_1_graphical_abstract_joint.pdf")
  open(pdf_path); tryCatch(draw(), finally = grDevices::dev.off()); message("wrote ", pdf_path)
  if (!is.null(preview_png) && !is.na(preview_png) && nzchar(preview_png)) {
    open(preview_png, png = TRUE); tryCatch(draw(), finally = grDevices::dev.off()); message("wrote ", preview_png)
  }
  invisible(pdf_path)
}
if (sys.nframe() == 0L) { a <- commandArgs(trailingOnly = TRUE); do.call(render_figure1_graphical_abstract_joint, as.list(a)) }
