# Figures 2 and 3 for the three-group presentation (protocol addenda G and H, 19 September 2026),
# re-expressed with one reference group per model (protocol addendum K, 19 September 2026):
# SS is the reference of the primary model and S of the before-pregnancy model.
# Base R only, no refitting. The flipped contrasts (sn_vs_ss, nn_vs_ss, n_vs_s) were derived exactly by
# 39_flip_reference.R and are read from the same receipted summaries written by
# 34_summarize_joint_three_group.R, together with the released crossover table and the assembled
# supplementary summary.
#
# Figure 2: two rows by three panels. Row 1, the joint three-group model: standardized GH/PE risk per
#   1,000 for SS (reference), SN and NN on one absolute scale; adjusted RR and RD of SN vs SS and
#   NN vs SS. Row 2, the before-pregnancy comparison N vs S, with S as the reference. Each crossover is
#   a point at the null with its local 95% CI (filled: simultaneous band supports opposite directions on
#   the two sides; open: not established). No simultaneous-band bars in the panels (owner decision).
#   Crossover ages are orientation-free, so they are unchanged by the reversal.
# Figure 3: crossover ages across every GH/PE specification, relabelled with the exposure codes; the
#   main block is the three main contrasts. The points do not move under the reversal; only the labels
#   of the three main rows change. Sensitivity rows keep their released orientation (addendum K).
#
# From 01_Analysis: Rscript --vanilla age45_revision/publication_code/render_main_figures_joint.R [joint_dir] [output_dir]
render_main_figures_joint <- function(joint = "age45_revision/outputs/supplementary/joint_3group",
                                      output_dir = "age45_revision/outputs/supplementary/figures",
                                      package_root = "age45_revision/submission_staging",
                                      report = "age45_revision/outputs/supplementary/report") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  S <- file.path(joint, "summary")
  summ <- read.csv(file.path(S, "main_summary.csv"), stringsAsFactors = FALSE)
  ages <- read.csv(file.path(S, "main_age_estimates.csv"), stringsAsFactors = FALSE)
  curves <- read.csv(file.path(S, "three_group_curves.csv"), stringsAsFactors = FALSE)
  gray <- "#757575"; orange <- "#C8641E"; blue <- "#2367A0"; ink <- "#222222"
  # Addendum K: the reference group of each model carries the grey dashed styling; the comparators are
  # solid and coloured. SS and S are now the references, so grey moves to them and blue to NN and N.
  col_of <- c(SS = gray, SN = orange, NN = blue, S = gray, N = blue)
  draw <- function(name, w, h, fn) {
    p <- file.path(output_dir, paste0(name, ".pdf"))
    if (Sys.info()[["sysname"]] == "Darwin") quartz(type = "pdf", file = p, width = w, height = h, family = "Helvetica")
    else cairo_pdf(p, width = w, height = h, family = "Helvetica")
    tryCatch(fn(), finally = dev.off()); p
  }
  age_of <- function(id) { s <- ages[ages$model_id == id, ]; s <- s[order(s$age), ]; stopifnot(identical(as.integer(s$age), 15:45)); s }
  row_of <- function(id) { r <- summ[summ$model_id == id, ]; stopifnot(nrow(r) == 1); r }
  # Addendum K orientation: comparator vs reference. SS is the reference of the primary model, S of the
  # before-pregnancy model. The axis limits are computed from these plotted contrasts, so they adapt.
  main_ids <- c("sn_vs_ss", "nn_vs_ss", "n_vs_s")
  stopifnot(all(main_ids %in% summ$model_id), all(main_ids %in% ages$model_id))
  A <- do.call(rbind, lapply(main_ids, age_of))
  rr_lim <- range(c(A$rr_lower, A$rr_upper)); rr_lim <- exp(log(rr_lim) + c(-1, 1) * .05 * diff(log(rr_lim)))
  rd_lim <- range(c(0, A$rd_lower_per1000, A$rd_upper_per1000)); rd_lim <- rd_lim + c(-1, 1) * diff(rd_lim) * .06
  # risk0_* is the reference arm (S) and risk1_* the comparator (N) in the flipped before-pregnancy rows.
  pre <- age_of("n_vs_s")
  risk_lim <- range(c(curves$lower_per1000, curves$upper_per1000, pre$risk0_lower_per1000, pre$risk0_upper_per1000,
                      pre$risk1_lower_per1000, pre$risk1_upper_per1000)); risk_lim <- risk_lim + c(-1, 1) * diff(risk_lim) * .05
  ribbon <- function(x, lo, hi, col) polygon(c(x, rev(x)), c(lo, rev(hi)), border = NA, col = adjustcolor(col, .16))
  xaxis <- function() axis(1, at = seq(15, 45, 5), cex.axis = .85)
  # crossover marker: point at the null, local CI as a whisker, label above or below
  marker <- function(r, y0, col, above = TRUE, log_scale = FALSE) {
    if (is.na(r$crossover_age)) return(invisible())
    segments(r$delta_lower, y0, r$delta_upper, y0, col = col, lwd = 2.4, lend = 1)
    points(r$crossover_age, y0, pch = if (isTRUE(r$simultaneous_reversal)) 16 else 21, bg = "white", col = col, cex = 1.35, lwd = 1.6)
    u <- par("usr"); span <- u[4] - u[3]; off <- (if (above) .07 else -.10) * span
    y <- if (log_scale) 10^(log10(y0) + off) else y0 + off
    text(r$crossover_age, y, sprintf("%.1f y", r$crossover_age), cex = .72, col = col, font = 2)
  }
  panel_rr <- function(items, header) {
    plot(NA, xlim = c(15, 45), ylim = rr_lim, log = "y", xlab = "", ylab = "", bty = "l", xaxt = "n", yaxt = "n")
    ticks <- c(.6, .7, .8, .9, 1, 1.1, 1.25, 1.4, 1.6); ticks <- ticks[ticks > rr_lim[1] & ticks < rr_lim[2]]
    xaxis(); axis(2, at = ticks, labels = sprintf("%.2f", ticks), las = 1, cex.axis = .85); abline(h = 1, lty = 2, col = ink, lwd = .8)
    for (it in items) { s <- it$ages; ribbon(s$age, s$rr_lower, s$rr_upper, it$col); lines(s$age, s$rr, col = it$col, lwd = 1.8) }
    for (it in items) marker(it$row, 1, it$col, above = it$above, log_scale = TRUE)
    if (header) mtext("Adjusted risk ratio (log scale)", 3, line = .5, cex = .85, font = 2)
  }
  panel_rd <- function(items, header) {
    plot(NA, xlim = c(15, 45), ylim = rd_lim, xlab = "", ylab = "", bty = "l", xaxt = "n", yaxt = "n")
    xaxis(); axis(2, las = 1, cex.axis = .85); abline(h = 0, lty = 2, col = ink, lwd = .8)
    for (it in items) { s <- it$ages; ribbon(s$age, s$rd_lower_per1000, s$rd_upper_per1000, it$col); lines(s$age, s$rd_per1000, col = it$col, lwd = 1.8) }
    for (it in items) marker(it$row, 0, it$col, above = it$above)
    if (header) mtext("Adjusted risk difference per 1,000", 3, line = .5, cex = .85, font = 2)
  }
  panel_risk <- function(groups, header, label, n) {
    # groups: list of list(name, x, y, lo, hi, col, lty)
    plot(NA, xlim = c(15, 45), ylim = risk_lim, xlab = "", ylab = "", bty = "l", xaxt = "n", yaxt = "n")
    xaxis(); axis(2, las = 1, cex.axis = .85)
    for (g in groups) ribbon(g$x, g$lo, g$hi, g$col)
    for (g in groups) lines(g$x, g$y, col = g$col, lwd = 1.8, lty = g$lty)
    legend("topleft", legend = vapply(groups, `[[`, "", "name"), col = vapply(groups, `[[`, "", "col"),
      lty = vapply(groups, `[[`, 1, "lty"), lwd = 1.8, bty = "n", cex = .8, inset = c(.01, 0))
    if (header) mtext("Standardized GH/PE risk per 1,000", 3, line = .5, cex = .85, font = 2)
    mtext(label, 2, line = 4.8, cex = .8, las = 1, adj = 1, xpd = NA)
    mtext(paste0("N = ", format(n, big.mark = ",", trim = TRUE)), 2, line = 4.8, at = par("usr")[3] + .08 * diff(par("usr")[3:4]),
      cex = .72, las = 1, adj = 1, col = gray, xpd = NA)
  }
  nn <- row_of("nn_vs_ss"); sn <- row_of("sn_vs_ss"); pr <- row_of("n_vs_s")
  cv <- function(g) { z <- curves[curves$group == g, ]; z[order(z$age), ] }
  f2 <- draw("Figure_2_joint_three_group", 11.2, 8.5, function() {
    layout(matrix(1:6, 2, 3, byrow = TRUE))
    par(oma = c(8.7, 12.8, 3.4, .6), mar = c(2.6, 3.9, 1.8, .6), family = "sans", cex = .82, xaxs = "i", mgp = c(2.5, .6, 0))
    # ---- row 1: the joint three-group model, SS the reference
    NN <- cv("NN"); SN <- cv("SN"); SS <- cv("SS")
    panel_risk(list(
      list(name = "SS, reference", x = SS$age, y = SS$risk_per1000, lo = SS$lower_per1000, hi = SS$upper_per1000, col = col_of[["SS"]], lty = 2),
      list(name = "SN", x = SN$age, y = SN$risk_per1000, lo = SN$lower_per1000, hi = SN$upper_per1000, col = col_of[["SN"]], lty = 1),
      list(name = "NN", x = NN$age, y = NN$risk_per1000, lo = NN$lower_per1000, hi = NN$upper_per1000, col = col_of[["NN"]], lty = 1)),
      TRUE, "Primary model\nSN vs SS and NN vs SS\none fit; SS is the reference", sn$fit_n)
    items1 <- list(list(ages = age_of("sn_vs_ss"), row = sn, col = col_of[["SN"]], above = TRUE),
                   list(ages = age_of("nn_vs_ss"), row = nn, col = col_of[["NN"]], above = FALSE))
    panel_rr(items1, TRUE); legend("bottomright", legend = c("SN vs SS", "NN vs SS"), col = c(col_of[["SN"]], col_of[["NN"]]), lwd = 1.8, bty = "n", cex = .8)
    panel_rd(items1, TRUE)
    # ---- row 2: before pregnancy, N vs S, S the reference
    panel_risk(list(
      list(name = "S, reference", x = pre$age, y = pre$risk0_per1000, lo = pre$risk0_lower_per1000, hi = pre$risk0_upper_per1000, col = col_of[["S"]], lty = 2),
      list(name = "N", x = pre$age, y = pre$risk1_per1000, lo = pre$risk1_lower_per1000, hi = pre$risk1_upper_per1000, col = col_of[["N"]], lty = 1)),
      FALSE, "Before-pregnancy comparison\nN vs S, irrespective of T1\nS is the reference", pr$fit_n)
    items2 <- list(list(ages = pre, row = pr, col = col_of[["N"]], above = FALSE))
    panel_rr(items2, FALSE); legend("bottomright", legend = "N vs S", col = col_of[["N"]], lwd = 1.8, bty = "n", cex = .8)
    panel_rd(items2, FALSE)
    mtext("Age-specific associations of early-pregnancy smoking with recorded GH/PE, US births 2016-2024", 3, outer = TRUE, line = 1.6, cex = .95, font = 2)
    mtext("Maternal age (years)", 1, outer = TRUE, line = .6, cex = .9)
    cap <- c("SS: women who smoked before pregnancy and in the first trimester. SN: women who smoked before pregnancy and reported none in the first trimester.",
             "NN: women who reported none in either window. S / N: any smoking before pregnancy / none, irrespective of the first trimester.",
             "SS is the reference of the primary model (row 1) and S of the before-pregnancy model (row 2); a risk ratio below one is a lower recorded risk than the reference.",
             "Row 1 is the primary model, one fit of three groups, standardized to the pooled SS, SN and NN population at each age; row 2 is standardized to its own population.",
             "Shading: pointwise 95% CI (HC0). Crossover: point at the null with its local 95% CI; filled if the simultaneous 95% band supports opposite",
             "directions on the two sides, open if not. Crossover ages do not depend on orientation. Each age uses its own empirical covariate reference; lines are guides.")
    for (i in seq_along(cap)) mtext(cap[i], 1, outer = TRUE, line = 2.1 + 1.05 * (i - 1), cex = .70)
  })
  # ---------------------------------------------------------------- Figure 3: crossover forest, relabelled
  cross <- read.csv(file.path(package_root, "Tables", "Crossover_estimates.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  supp <- read.csv(file.path(report, "supp_summary.csv"), stringsAsFactors = FALSE)
  est <- function(id, age, lo, hi, filled) data.frame(id = id, age = age, lower = lo, upper = hi, filled = filled, stringsAsFactors = FALSE)
  from_summ <- function(id) { r <- row_of(id); est(id, r$crossover_age, r$delta_lower, r$delta_upper, isTRUE(r$simultaneous_reversal)) }
  from_supp <- function(id) { r <- supp[supp$model_id == id, ]; stopifnot(nrow(r) == 1); est(id, r$crossover_age, r$delta_lower, r$delta_upper, isTRUE(r$simultaneous_reversal)) }
  from_cross <- function(id) { r <- cross[cross$model_id == id, ]; stopifnot(nrow(r) == 1); est(id, r$age, r$lower, r$upper, isTRUE(as.logical(r$opposite_signs_around_main_root))) }
  # The first-trimester margin (protocol addendum J) appears as a supplementary row once it is fitted.
  has_t1 <- "t1_only" %in% summ$model_id
  # Addendum K: crossover ages are orientation-free, so the three main rows keep their points and only
  # change label. The sensitivity rows below keep their released orientation.
  groups <- list(
    "Main comparisons" = c("sn_vs_ss", "nn_vs_ss", "n_vs_s"),
    "First-trimester margin, irrespective of before-pregnancy smoking" = if (has_t1) c("t1_only") else character(),
    "SS vs NN, standardized to the SS and NN records only" = c("broad_main"),
    "SS vs SN, standardized to women who smoked before pregnancy" = c("primary_main", "primary_core_same_cc", "primary_augmented_same_cc", "primary_core_available", "primary_recent", "primary_oe28"),
    "Age specification (SS vs SN)" = c("age_spec_fewer_age_knots", "age_spec_shifted_age_knots", "age_spec_more_age_knots", "age_spec_age_by_bmi_nuisance"),
    "Fetal-inclusive comparisons (SS vs SN)" = c("all_years_shared_core_live", "all_years_shared_core_inclusive", "all_years_shared_augmented_live",
      "all_years_shared_augmented_inclusive", "reporting_years_shared_core_live", "reporting_years_shared_core_inclusive",
      "reporting_years_shared_augmented_live", "reporting_years_shared_augmented_inclusive"))
  pretty <- c(sn_vs_ss = "SN vs SS, primary model", nn_vs_ss = "NN vs SS, primary model",
    n_vs_s = "N vs S, none vs any smoking before pregnancy",
    t1_only = "T1 smoking vs none in T1 (reference pools SN and NN)",
    supp_3lvl_continued_vs_none = "SS vs NN", broad_main = "SS vs NN",
    primary_main = "SS vs SN, full adjustment", primary_core_same_cc = "Core adjustment, same records",
    primary_augmented_same_cc = "Core + BMI and education, same records", primary_core_available = "Core adjustment, core-available records",
    primary_recent = "2018-2024 only", primary_oe28 = "Gestation 28 weeks or more",
    age_spec_fewer_age_knots = "Two age knots (22, 32)", age_spec_shifted_age_knots = "Shifted age knots (23, 29, 36)",
    age_spec_more_age_knots = "Four age knots (20, 25, 30, 36)", age_spec_age_by_bmi_nuisance = "Age-by-BMI nuisance interaction",
    all_years_shared_core_live = "All years, core: live births only", all_years_shared_core_inclusive = "All years, core: fetal deaths included",
    all_years_shared_augmented_live = "All years, augmented: live births only", all_years_shared_augmented_inclusive = "All years, augmented: fetal deaths included",
    reporting_years_shared_core_live = "Reporting years, core: live births only", reporting_years_shared_core_inclusive = "Reporting years, core: fetal deaths included",
    reporting_years_shared_augmented_live = "Reporting years, augmented: live births only", reporting_years_shared_augmented_inclusive = "Reporting years, augmented: fetal deaths included")
  groups <- groups[lengths(groups) > 0]
  ids <- unlist(groups); stopifnot(!anyDuplicated(ids), all(ids %in% names(pretty)))
  from_main_summary <- c("sn_vs_ss", "nn_vs_ss", "n_vs_s", "t1_only")
  E <- do.call(rbind, lapply(ids, function(id) if (startsWith(id, "joint_") || id %in% from_main_summary) from_summ(id)
    else if (startsWith(id, "supp_")) from_supp(id) else from_cross(id)))
  rownames(E) <- E$id
  # Guard (addendum K): a reversed main row must reproduce the released crossover age exactly.
  if ("prepregnancy_main" %in% cross$model_id)
    stopifnot(abs(E["n_vs_s", "age"] - cross$age[cross$model_id == "prepregnancy_main"]) < 1e-6)
  stopifnot(abs(E["sn_vs_ss", "age"] - row_of("joint_ss_vs_sn")$crossover_age) < 1e-9,
            abs(E["nn_vs_ss", "age"] - row_of("joint_ss_vs_nn")$crossover_age) < 1e-9)
  ypos <- c(); headers <- c(); y <- 0
  for (g in names(groups)) { y <- y - 1; headers[g] <- y; for (id in groups[[g]]) { y <- y - 1; ypos[id] <- y }; y <- y - .35 }
  xlim <- c(floor(min(E$lower, na.rm = TRUE)) - .5, 37)
  f3 <- draw("Figure_3_joint_crossover_forest", 11.8, 10.1, function() {
    par(mar = c(7.6, 23.5, 3.8, 10.5), family = "sans", cex = .85, xaxs = "i")
    plot(NA, xlim = xlim, ylim = c(min(ypos) - .8, -.2), xlab = "", ylab = "", axes = FALSE)
    mr <- ypos[groups[[1]]]; rect(xlim[1], min(mr) - .5, xlim[2], max(mr) + .5, col = "#F3F6FA", border = NA)
    abline(v = seq(ceiling(xlim[1]), 37, 1), col = "#EBEBEB", lwd = .6)
    abline(v = E["sn_vs_ss", "age"], col = gray, lty = 3, lwd = .9)
    for (id in ids) {
      r <- E[id, ]; yy <- ypos[[id]]; if (is.na(r$age)) { text(mean(xlim), yy, "no fitted crossing", col = gray, cex = .8); next }
      lo <- max(xlim[1], r$lower); hi <- min(xlim[2], r$upper); segments(lo, yy, hi, yy, col = blue, lwd = 2)
      if (r$lower < xlim[1]) arrows(lo + .3, yy, xlim[1] + .05, yy, length = .06, col = blue, lwd = 2)
      if (r$upper > xlim[2]) arrows(hi - .3, yy, xlim[2] - .05, yy, length = .06, col = blue, lwd = 2)
      points(r$age, yy, pch = if (r$filled) 16 else 1, col = ink, cex = if (id %in% groups[[1]]) 1.05 else .85, lwd = 1.2)
      text(xlim[2] + .25, yy, sprintf("%.1f (%.1f, %.1f)", r$age, r$lower, r$upper), adj = 0, xpd = NA, cex = .8, font = if (id %in% groups[[1]]) 2 else 1)
    }
    axis(1, at = seq(ceiling(xlim[1]), 37, 1), cex.axis = .85)
    nonmain <- setdiff(ids, groups[[1]])
    axis(2, at = ypos[nonmain], labels = pretty[nonmain], las = 1, tick = FALSE, cex.axis = .8)
    for (g in names(groups)) mtext(g, 2, at = headers[[g]], line = .5, las = 1, adj = 1, cex = .8, font = 3, col = "#444444", xpd = NA)
    for (id in groups[[1]]) mtext(pretty[[id]], 2, at = ypos[[id]], line = .5, las = 1, adj = 1, cex = .8, font = 2, xpd = NA)
    box(bty = "l", col = gray)
    text(xlim[2] + .25, -.35, "Crossover age (local 95% CI)", adj = 0, xpd = NA, font = 2, cex = .8)
    title(sprintf("Fitted age at which the smoking association changes direction, across %d specifications", length(ids)), cex.main = 1.05, line = 2.4)
    mtext("Complete-case models fitted at ages 15-45; HC0 covariance. Dotted line: SN vs SS in the primary model.", 3, line = 1.0, cex = .78)
    mtext("Maternal age at fitted crossover (years)", 1, line = 2.6, cex = .9)
    mtext("Filled: the simultaneous 95% band supports opposite directions on the two sides of the crossover. Open: not established.", 1, line = 4.1, cex = .75)
    mtext("Rows are overlapping analyses, not independent replications. SS, SN, NN, S, N: exposure codes defined in Methods.", 1, line = 5.2, cex = .75)
    mtext("Crossover ages do not depend on which group is the reference; the sensitivity rows below the shaded block keep their released orientation.", 1, line = 6.3, cex = .75)
  })
  message("Wrote ", f2, " and ", f3); invisible(c(f2, f3))
}
if (sys.nframe() == 0L) { a <- commandArgs(trailingOnly = TRUE); do.call(render_main_figures_joint, as.list(a)) }
