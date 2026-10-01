# Three-level, dose-response and comparison-outcome/strata figures from the assembled supplementary
# fits (24_assemble_supplementary.R): package Figures S3 to S5 under addendum D. Base R, no
# refitting. Output names carry no figure number; the package builder assigns them.
# From 01_Analysis: Rscript --vanilla age45_revision/publication_code/render_supplementary_fit_figures.R
render_supplementary_fit_figures <- function(report = "age45_revision/outputs/supplementary/report",
                                             output_dir = "age45_revision/outputs/supplementary/figures") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  ages <- read.csv(file.path(report, "supp_age_estimates.csv"), stringsAsFactors = FALSE)
  summ <- read.csv(file.path(report, "supp_summary.csv"), stringsAsFactors = FALSE)
  have <- function(id) id %in% summ$model_id
  blue <- "#2367A0"; orange <- "#C8641E"; gray <- "#757575"; ink <- "#222222"
  pal <- c("#9ECAE1", "#4292C6", "#2166AC", "#08306B", "#C8641E", "#7A3B00")
  draw <- function(name, w, h, fn) {
    p <- file.path(output_dir, paste0(name, ".pdf"))
    if (Sys.info()[["sysname"]] == "Darwin") quartz(type = "pdf", file = p, width = w, height = h, family = "Helvetica")
    else cairo_pdf(p, width = w, height = h, family = "Helvetica")
    tryCatch(fn(), finally = dev.off()); message("Wrote ", p); p
  }
  curve_panel <- function(ids, labels, cols, measure = "rr", title = "", ylab = NULL, legend_pos = "topleft", ref_line = if (measure == "rr") 1 else 0) {
    sub <- ages[ages$model_id %in% ids, ]
    if (!nrow(sub)) { plot.new(); title(paste(title, "(not run)"), cex.main = .9); return(invisible()) }
    k <- if (measure == "rd") 1000 else 1
    lo <- k * sub[[paste0(measure, "_lower")]]; hi <- k * sub[[paste0(measure, "_upper")]]
    ylim <- range(c(ref_line, lo, hi)); ylim <- ylim + c(-1, 1) * diff(ylim) * .05
    plot(NA, xlim = c(15, 45), ylim = ylim, log = if (measure == "rr") "y" else "", xlab = "", ylab = if (is.null(ylab)) (if (measure == "rr") "Adjusted RR (log scale)" else "Adjusted RD per 1,000") else ylab, bty = "l", xaxt = "n", las = 1)
    axis(1, at = seq(15, 45, 5)); abline(h = ref_line, lty = 2, col = ink, lwd = .8)
    for (i in seq_along(ids)) {
      s <- sub[sub$model_id == ids[i], ]; s <- s[order(s$age), ]; if (!nrow(s)) next
      polygon(c(s$age, rev(s$age)), c(k * s[[paste0(measure, "_lower")]], rev(k * s[[paste0(measure, "_upper")]])), border = NA, col = adjustcolor(cols[i], .15))
      lines(s$age, k * s[[measure]], col = cols[i], lwd = 1.8)
    }
    title(title, cex.main = .95, line = 1)
    if (!is.null(legend_pos)) legend(legend_pos, legend = labels[ids %in% sub$model_id], col = cols[ids %in% sub$model_id], lwd = 1.8, bty = "n", cex = .75)
  }
  # The former three-level figure (two-arm fits against a common NN reference) is withdrawn under
  # addenda G and H: the three groups are now Figure 2 row 1, from the joint model.
  # ---- dose-response ----------------------------------------------------------------------
  draw("dose_response", 11, 8.4, function() {
    par(mfrow = c(2, 2), mar = c(3.6, 4.4, 3, 1), oma = c(4.6, 0, 2.2, 0), family = "sans", cex = .85, xaxs = "i")
    t1 <- paste0("supp_t1dose_", c("1_5", "6_10", "11_20", "21plus")); t1l <- paste("T1", c("1-5", "6-10", "11-20", "21 or more"), "cigarettes/day vs 0")
    # RR panels: legends bottom-right, where no curve or CI band runs (the top-left carries the
    # wide young-age bands).
    curve_panel(t1, t1l, pal[1:4], "rr", "First-trimester dose, women who smoked before pregnancy", legend_pos = "bottomright")
    curve_panel(t1, t1l, pal[1:4], "rd", "First-trimester dose, risk difference")
    pd <- paste0("supp_pdose_", c("1_5", "6_10", "11_20", "21plus")); pdl <- paste("Prepregnancy", c("1-5", "6-10", "11-20", "21 or more"), "cigarettes/day vs 0")
    curve_panel(pd, pdl, pal[1:4], "rr", "Prepregnancy dose vs no prepregnancy smoking", legend_pos = "bottomright")
    ch <- c("supp_t1change_reduced", "supp_t1change_same_or_more"); chl <- c("Reduced dose in T1 vs none in T1 (SN)", "Same or higher dose in T1 vs none in T1 (SN)")
    curve_panel(ch, chl, c(pal[2], pal[4]), "rr", "Dose change in T1, women who smoked before pregnancy")
    mtext("Dose-response by maternal age", 3, outer = TRUE, line = .6, cex = 1, font = 2)
    mtext("Maternal age (years)", 1, outer = TRUE, line = .2, cex = .9)
    mtext("First-trimester dose rows adjust for prepregnancy dose and share one reference (all women who smoked before pregnancy); prepregnancy dose shares the prepregnancy population reference.", 1, outer = TRUE, line = 1.5, cex = .72)
    mtext("Shading: pointwise 95% CI (HC0). Connecting lines are visual guides.", 1, outer = TRUE, line = 2.6, cex = .72)
  })
  # ---- Figure S6: control outcome and race/ethnicity strata ----------------------------
  draw("control_outcome_and_race_strata", 11, 8.4, function() {
    layout(matrix(c(1, 2, 3, 4, 4, 4), 2, 3, byrow = TRUE), heights = c(1, 1.1))
    par(mar = c(3.6, 4.4, 3, 1), oma = c(4.6, 0, 2.2, 0), family = "sans", cex = .85, xaxs = "i")
    pre <- c(primary = "supp_preterm_primary", broad = "supp_preterm_broad", prepregnancy = "supp_preterm_prepregnancy")
    lab <- c(primary = "SS vs SN", broad = "SS vs NN", prepregnancy = "S vs N")
    # The panel title already names the outcome; an in-panel legend only sat on the reference line.
    for (ct in names(pre)) curve_panel(pre[[ct]], "Preterm birth (<37 weeks)", blue, "rr", paste0("Preterm birth (<37 weeks): ", lab[[ct]]), legend_pos = NULL)
    # forest of crossovers by race stratum
    ids <- c(paste0("supp_race_primary_", c("nhw", "nhb", "hisp", "other")), paste0("supp_race_broad_", c("nhw", "nhb", "hisp", "other")))
    rl <- rep(c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other groups combined"), 2)
    s <- summ[match(ids, summ$model_id), ]
    par(mar = c(3.6, 16, 3, 9))
    plot(NA, xlim = c(15, 45), ylim = c(.5, 9.5), xlab = "", ylab = "", axes = FALSE)
    axis(1, at = seq(15, 45, 5)); abline(v = seq(15, 45, 5), col = "#EEEEEE")
    y <- c(9:6, 4:1)
    for (i in seq_along(ids)) {
      r <- s[i, ]; if (is.na(r$model_id)) { text(30, y[i], "not run", col = gray, cex = .8); next }
      if (!is.na(r$crossover_age)) {
        segments(max(15, r$delta_lower), y[i], min(45, r$delta_upper), y[i], col = blue, lwd = 2)
        points(r$crossover_age, y[i], pch = if (isTRUE(r$simultaneous_reversal)) 16 else 1, col = ink)
        text(45.4, y[i], sprintf("%.1f (%.1f, %.1f)", r$crossover_age, r$delta_lower, r$delta_upper), adj = 0, xpd = NA, cex = .78)
      } else text(30, y[i], if (nzchar(r$fitted_null_ages)) paste("more than one fitted crossing:", r$fitted_null_ages) else "no fitted crossing", col = gray, cex = .75)
    }
    axis(2, at = y, labels = rl, las = 1, tick = FALSE, cex.axis = .8)
    mtext("SS vs SN", 2, at = 9.9, line = .5, las = 1, adj = 1, font = 3, cex = .8, col = "#444444", xpd = NA)
    mtext("SS vs NN", 2, at = 4.9, line = .5, las = 1, adj = 1, font = 3, cex = .8, col = "#444444", xpd = NA)
    box(bty = "l", col = gray); title("Crossover age by race and ethnicity stratum (race covariate omitted)", cex.main = .95, line = 1)
    text(45.4, 9.9, "Crossover (local 95% CI)", adj = 0, xpd = NA, font = 2, cex = .78)
    mtext("Outcome specificity and modification by race and ethnicity", 3, outer = TRUE, line = .6, cex = 1, font = 2)
    mtext("Maternal age (years)", 1, outer = TRUE, line = .2, cex = .9)
    mtext("Top: preterm birth replaces GH/PE as the outcome on the same complete-case records. Bottom: filled points, simultaneous band supports opposite directions; open, not established.", 1, outer = TRUE, line = 1.5, cex = .72)
  })
  invisible(TRUE)
}
if (sys.nframe() == 0L) render_supplementary_fit_figures()
