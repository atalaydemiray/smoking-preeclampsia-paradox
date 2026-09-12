# The eight figures of the manuscript: output/figures/Figure_1..3 and Figure_S1..S5, drawn from
# the aggregate estimates in results/. Base R only, no refitting, one function per figure.

# One device for every figure, so a rebuilt PDF differs from the submitted one only in the
# creation time it embeds. quartz exists on macOS only; cairo_pdf is its equivalent elsewhere.
figure_pdf <- function(name, width, height, draw) {
  dir.create(out_path("figures"), recursive = TRUE, showWarnings = FALSE)
  path <- out_path("figures", paste0(name, ".pdf"))
  if (Sys.info()[["sysname"]] == "Darwin")
    grDevices::quartz(type = "pdf", file = path, width = width, height = height, family = "Helvetica")
  else grDevices::cairo_pdf(path, width = width, height = height, family = "Helvetica")
  tryCatch(draw(), finally = grDevices::dev.off())
  message("wrote ", sub(project_root(), "", path, fixed = TRUE))
  invisible(path)
}

# The fitted crossover age of each of the 21 models, on one row per model. The fitting code wrote
# the root and its local interval to root_reference.csv and the simultaneous sign regions to a
# separate file; Figures 1 and 3 need both, plus the flag for whether the simultaneous band
# supports opposite directions on the two sides of the root.
crossover_estimates <- function() {
  labels <- read_result("main", "crossover_labels.csv")
  roots <- read_result("main", "root_reference.csv")
  signs <- read_result("main", "crossover_simultaneous_sign_regions.csv")
  rows <- lapply(seq_len(nrow(labels)), function(i) {
    id <- labels$model_id[i]
    # The displayed root is the one regular interior crossing; the fitting code restricted the
    # display to ages 20 to 40 and this keeps that restriction.
    r <- roots[roots$model_id == id & roots$covariance == "HC0" & roots$age >= 20 & roots$age <= 40, ]
    stopifnot(nrow(r) == 1, r$kind == "crossing", r$regular_delta, is.finite(r$se_delta))
    sr <- signs[signs$model_id == id & signs$covariance == "HC0", ]
    data.frame(model_id = id, label = labels$display_label[i], age = r$age,
               lower = r$delta_lower, upper = r$delta_upper,
               opposite_signs_around_main_root =
                 any(sr$classification == "strict_negative" & sr$upper <= r$age) &&
                 any(sr$classification == "strict_positive" & sr$lower >= r$age),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  stopifnot(nrow(out) == 21, all(out$age > 20 & out$age < 40))
  out
}

# Figure 1 as a graphical abstract: who was studied (A), how the four cigarette fields define the
# three comparisons (B), and what was found (C). No new numbers: every count is read from results/
# and checked against the overall counts before drawing.
figure_1 <- function() {
  overall <- read_result("main", "main_overall.csv")
  stopifnot(identical(overall$contrast, c("primary", "broad", "prepregnancy")))
  ages <- read_result("main", "main_age_estimates.csv"); ages <- ages[ages$covariance == "HC0", ]
  cross <- crossover_estimates()
  flow <- read_result("descriptive", "common_clinical_flow_by_year.csv"); flow <- flow[flow$oe_threshold == 20, ]
  stopifnot(setequal(unique(flow$year), 2016:2024))
  ids <- c("primary_main", "broad_main", "prepregnancy_main")
  arms <- lapply(ids, function(id) {
    a <- read_result("models", paste0(id, "_age_arm_support.csv"))
    data.frame(A = c(0, 1), n = as.numeric(tapply(a$n, a$A, sum)), events = as.numeric(tapply(a$events, a$A, sum)))
  })
  for (i in 1:3) {
    stopifnot(sum(arms[[i]]$n) == overall$n[i], sum(arms[[i]]$events) == overall$events[i],
              arms[[i]]$n[1] == overall$A0[i], arms[[i]]$n[2] == overall$A1[i])
  }
  stage <- function(s, col) sum(flow[flow$stage == s, col])
  n_source <- stage("source_records", "n_entering")
  excl <- c(us_residents = stage("us_residents", "n_excluded"), singleton = stage("singleton", "n_excluded"),
            maternal_age_15_45 = stage("maternal_age_15_45", "n_excluded"),
            known_oe_at_least_threshold = stage("known_oe_at_least_threshold", "n_excluded"),
            known_no_prepregnancy_hypertension = stage("known_no_prepregnancy_hypertension", "n_excluded"))
  n_clinical <- stage("known_no_prepregnancy_hypertension", "n_retained")
  stopifnot(n_source - sum(excl) == n_clinical, all(overall$eligible_n <= n_clinical))
  age_rr <- function(id, a) { r <- ages[ages$model_id == id & ages$age == a, ]; stopifnot(nrow(r) == 1); r$rr }
  cx <- cross[match(ids, cross$model_id), ]; stopifnot(!anyNA(cx$age))

  num <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  per1000 <- function(e, n) sprintf("%.1f", 1000 * e / n)
  f2 <- function(x) sprintf("%.2f", x); f1 <- function(x) sprintf("%.1f", x); f3 <- function(x) sprintf("%.3f", x)
  ink <- "#222222"; mute <- "#6B6B6B"; rule <- "#BFBFBF"; paper <- "#F5F5F2"; brown <- "#8A4B12"
  mcol <- c("#0072B2", "#D55E00", "#009E73")            # Okabe-Ito: comparisons 1, 2, 3
  plus_fill <- "#3A3A3A"; zero_fill <- "#FFFFFF"; any_fill <- "#E3E3E3"; onset_fill <- "#FBE9D7"

  figure_pdf("Figure_1_study_overview", 12.4, 7.6, function() {
    par(mar = c(0, 0, 0, 0), family = "sans", xaxs = "i", yaxs = "i")
    plot.new(); plot.window(xlim = c(0, 124), ylim = c(0, 76))
    tx <- function(x, y, s, cex = .72, col = ink, adj = c(0, .5), font = 1, ...) text(x, y, s, cex = cex, col = col, adj = adj, font = font, xpd = NA, ...)
    box_ <- function(x0, y0, x1, y1, fill = "white", border = rule, lwd = .8) rect(x0, y0, x1, y1, col = fill, border = border, lwd = lwd)
    # ---------------------------------------------------------------- title band
    box_(0, 68.5, 124, 76, fill = paper, border = NA)
    tx(2, 73.5, "Reframing the smoking-preeclampsia paradox: an age-related reversal in 30.1 million United States birth records",
       cex = 1.14, font = 2)
    tx(2, 70.6, paste0("US natality public-use files 2016 to 2024; singleton live births; maternal ages 15 to 45; ",
                       "three smoking comparisons; adjusted, age-specific standardized risks."), cex = .78, col = mute)
    segments(0, 68.5, 124, 68.5, col = rule)
    # ---------------------------------------------------------------- panel A: who was studied
    tx(2, 65.6, "A", cex = 1.1, font = 2); tx(4.6, 65.6, "Who was studied", cex = .95, font = 2)
    box_(2, 58.6, 39.5, 63.2, fill = "white", border = ink, lwd = 1)
    tx(20.75, 60.9, paste0(num(n_source), " birth records, 2016 to 2024"), cex = .95, font = 2, adj = c(.5, .5))
    spine_x <- 8; y_top <- 58.6; y_bot <- 33.2
    segments(spine_x, y_top, spine_x, y_bot, col = ink, lwd = 1.2)
    ex_lab <- c("Not resident in the United States", "Not a singleton birth", "Maternal age outside 15 to 45",
                "Gestational age unknown or not 20 to 47 weeks", "Chronic hypertension recorded or unknown")
    ys <- seq(56, 36.4, length.out = 5)
    for (k in 1:5) {
      segments(spine_x, ys[k], spine_x + 3.2, ys[k], col = ink, lwd = .9)
      polygon(spine_x + 3.2 + c(0, -.9, -.9), ys[k] + c(0, .55, -.55), col = ink, border = NA)
      tx(spine_x + 3.9, ys[k] + .95, paste0("Excluded ", num(excl[k])), cex = .74, font = 2)
      tx(spine_x + 3.9, ys[k] - .75, ex_lab[k], cex = .64, col = mute)
    }
    polygon(spine_x + c(0, -.75, .75), y_bot + c(0, 1.2, 1.2), col = ink, border = NA)
    box_(2, 27.8, 39.5, 32.8, fill = "white", border = ink, lwd = 1)
    tx(20.75, 30.3, paste0(num(n_clinical), " eligible births"), cex = .95, font = 2, adj = c(.5, .5))
    tx(2, 25.4, "Complete-case population of each comparison:", cex = .66, col = mute)
    yy <- c(21.9, 16.9, 11.9)
    short <- c("1  Continued vs stopped smoking in T1", "2  Smoked before pregnancy and in T1 vs neither",
               "3  Smoked before pregnancy vs did not")
    for (i in 1:3) {
      s <- overall[i, ]
      rect(2, yy[i] - 2.1, 2.9, yy[i] + 2.1, col = mcol[i], border = NA)
      tx(3.8, yy[i] + 1.25, short[i], cex = .72, font = 2, col = mcol[i])
      left <- paste0("Eligible ", num(s$eligible_n)); tx(3.8, yy[i] - .15, left, cex = .7)
      ax <- 3.8 + strwidth(left, cex = .7) + .7                     # drawn arrow: the fonts have no arrow glyph
      segments(ax, yy[i] - .15, ax + 1.05, yy[i] - .15, col = ink, lwd = .9)          # shaft ends at the head's base
      polygon(ax + 1.05 + c(.6, 0, 0), yy[i] - .15 + c(0, .36, -.36), col = ink, border = NA)   # head tip beyond the shaft
      tx(ax + 2.3, yy[i] - .15, paste0("complete cases ", num(s$n)), cex = .7)
      tx(3.8, yy[i] - 1.45, paste0(num(s$excluded_n), " (", f1(100 * s$excluded_n / s$eligible_n), "%) excluded for missing covariates"),
         cex = .64, col = mute)
    }
    tx(2, 7.2, "The three populations overlap.", cex = .64, col = mute)
    # ---------------------------------------------------------------- panel B: how smoking was defined
    segments(41.3, 2, 41.3, 68.5, col = rule); segments(82.7, 2, 82.7, 68.5, col = rule)
    tx(43.3, 65.6, "B", cex = 1.1, font = 2); tx(45.9, 65.6, "How smoking was defined", cex = .95, font = 2)
    cx0 <- 45.2; cw <- 8.6; gap <- .5
    cells_x <- cx0 + (0:3) * (cw + gap); nx <- cells_x[2] + cw + 1.4      # model rows use only P and T1
    flab <- c("Before\npregnancy (P)", "First\ntrimester (T1)", "Second\ntrimester (T2)", "Third\ntrimester (T3)")
    t23_mid <- (cells_x[3] + cells_x[4] + cw) / 2
    box_(cells_x[3], 53.1, cells_x[4] + cw, 62.9, fill = onset_fill, border = NA)      # one shaded column: label, cells, explanation
    tx(t23_mid, 61.75, "20 weeks onward: GH/PE window", cex = .6, col = brown, adj = c(.5, .5), font = 2)
    for (k in 1:4) {
      box_(cells_x[k], 57.2, cells_x[k] + cw, 60.4, fill = if (k <= 2) "white" else onset_fill, border = if (k <= 2) ink else "#D8B48F")
      tx(cells_x[k] + cw / 2, 58.8, flab[k], cex = .58, adj = c(.5, .5), col = if (k <= 2) ink else brown)
    }
    steps <- c("Overlap the outcome", "(GH/PE is recorded from 20 weeks)", "Do not define a comparison")
    sy <- c(56.2, 55.05, 53.9)
    for (k in 1:3) tx(t23_mid, sy[k], steps[k], cex = .56, col = brown, adj = c(.5, .5), font = if (k == 2) 1 else 2)
    tx(cx0, 51.5, "The overall RR and RD below average opposite age-specific associations (see C).", cex = .58, col = ink, font = 3)
    cell <- function(x, y, kind) {
      h <- 2.4; fill <- switch(kind, plus = plus_fill, zero = zero_fill, any = any_fill)
      rect(x, y - h / 2, x + cw, y + h / 2, col = fill, border = if (kind == "zero") ink else NA, lwd = .9)
      lab <- switch(kind, plus = "smoked", zero = "0 cigarettes", any = "any / unknown")
      text(x + cw / 2, y, lab, cex = .58, col = if (kind == "plus") "white" else ink, font = if (kind == "any") 3 else 1)
    }
    rows <- list(list(exp = c("plus", "plus"), ref = c("plus", "zero"),
                      elab = "Exposed: women who continued smoking in T1", rlab = "Reference: women who stopped smoking by T1"),
                 list(exp = c("plus", "plus"), ref = c("zero", "zero"),
                      elab = "Exposed: women who smoked before pregnancy and in T1", rlab = "Reference: women with 0 cigarettes in both periods"),
                 list(exp = c("plus", "any"), ref = c("zero", "any"),
                      elab = "Exposed: women who smoked before pregnancy", rlab = "Reference: women who did not smoke before pregnancy"))
    blk_h <- 12.6; blk_top <- 49.7 - (0:2) * (blk_h + 0.9)
    for (i in 1:3) {
      yt <- blk_top[i]; yb <- yt - blk_h; s <- overall[i, ]; a <- arms[[i]]
      box_(43.3, yb, 81.2, yt, fill = "white", border = rule)
      rect(43.3, yb, 44.3, yt, col = mcol[i], border = NA)
      tx(45.2, yt - 1.25, short[i], cex = .72, font = 2, col = mcol[i])
      ye <- yt - 4.6; yr <- yt - 8.9
      tx(cx0, ye + 2.0, rows[[i]]$elab, cex = .62, font = 2)
      tx(cx0, yr + 2.0, rows[[i]]$rlab, cex = .62, font = 2)
      for (k in 1:2) { cell(cells_x[k], ye, rows[[i]]$exp[k]); cell(cells_x[k], yr, rows[[i]]$ref[k]) }
      tx(nx, ye + .65, paste0(num(a$n[2]), " births"), cex = .68, font = 2)
      tx(nx, ye - .65, paste0(num(a$events[2]), " GH/PE (", per1000(a$events[2], a$n[2]), " per 1,000)"), cex = .6, col = mute)
      tx(nx, yr + .65, paste0(num(a$n[1]), " births"), cex = .68, font = 2)
      tx(nx, yr - .65, paste0(num(a$events[1]), " GH/PE (", per1000(a$events[1], a$n[1]), " per 1,000)"), cex = .6, col = mute)
      tx(45.2, yb + 1.15, paste0("Overall: RR ", f3(s$rr), " (", f3(s$rr_lower), " to ", f3(s$rr_upper), "), RD ",
                                 sprintf("%+.2f", s$rd_per1000), " per 1,000"), cex = .6, col = ink)
    }
    tx(43.3, 7.9, "Dark = smoked; white = 0 cigarettes; grey = any value or unknown.", cex = .58, col = mute)
    tx(43.3, 6.6, "Counts here are crude; the RR, RD and the curves in C are adjusted.", cex = .58, col = mute)
    # ---------------------------------------------------------------- panel C: what was found
    tx(84.7, 65.6, "C", cex = 1.1, font = 2); tx(87.3, 65.6, "What was found", cex = .95, font = 2)
    px0 <- 89.7; px1 <- 121.5; py0 <- 34.5; py1 <- 62
    xs <- function(a) px0 + (a - 15) / 30 * (px1 - px0)
    lo <- log(0.76); hi <- log(1.32)
    ysc <- function(r) py0 + (log(r) - lo) / (hi - lo) * (py1 - py0)
    rect(px0, py0, px1, py1, col = "white", border = NA)
    for (r in c(.8, .9, 1, 1.1, 1.2, 1.3)) {
      segments(px0, ysc(r), px1, ysc(r), col = if (r == 1) ink else "#E6E6E6", lwd = if (r == 1) 1 else .6)
      tx(px0 - .6, ysc(r), f2(r), cex = .6, col = mute, adj = c(1, .5))
    }
    for (a in seq(15, 45, 5)) { segments(xs(a), py0, xs(a), py0 - .6, col = mute, lwd = .6); tx(xs(a), py0 - 1.6, a, cex = .6, col = mute, adj = c(.5, .5)) }
    segments(px0, py0, px1, py0, col = mute, lwd = .6)
    tx((px0 + px1) / 2, py0 - 3.2, "Maternal age (years)", cex = .66, adj = c(.5, .5))
    tx(px0 - 4.4, (py0 + py1) / 2, "Adjusted RR, smoking vs reference (log scale)", cex = .62, adj = c(.5, .5), srt = 90)
    tx(px0 + .6, ysc(1) + 2.6, "Higher recorded risk", cex = .58, col = mute, font = 3)
    tx(px0 + .6, ysc(1) + 1.4, "with smoking", cex = .58, col = mute, font = 3)
    tx(px0 + .6, ysc(1) - 1.4, "Lower recorded risk", cex = .58, col = mute, font = 3)
    tx(px0 + .6, ysc(1) - 2.6, "with smoking", cex = .58, col = mute, font = 3)
    for (i in 1:3) {
      z <- ages[ages$model_id == ids[i], ]; z <- z[order(z$age), ]; stopifnot(identical(z$age, 15:45))
      polygon(c(xs(z$age), rev(xs(z$age))), c(ysc(pmax(z$rr_lower, .76)), rev(ysc(pmin(z$rr_upper, 1.32)))), col = adjustcolor(mcol[i], .12), border = NA)
    }
    for (i in 1:3) {
      z <- ages[ages$model_id == ids[i], ]; z <- z[order(z$age), ]
      lines(xs(z$age), ysc(z$rr), col = mcol[i], lwd = 2.2)
      segments(xs(cx$lower[i]), ysc(1), xs(cx$upper[i]), ysc(1), col = mcol[i], lwd = 3)
      points(xs(cx$age[i]), ysc(1), pch = 21, bg = "white", col = mcol[i], lwd = 1.6, cex = 1.1)
    }
    tx(84.7, 27.6, "Crossover age, years (local 95% CI)", cex = .68, font = 2)
    yl <- c(24.9, 21.3, 17.7)
    for (i in 1:3) {
      rect(84.7, yl[i] - 1.35, 85.6, yl[i] + 1.35, col = mcol[i], border = NA)
      tx(86.5, yl[i] + .75, short[i], cex = .68, font = 2, col = mcol[i])
      tx(86.5, yl[i] - .75, paste0(f1(cx$age[i]), " (", f1(cx$lower[i]), " to ", f1(cx$upper[i]), ") years;  RR ",
                                    f2(age_rr(ids[i], 20)), " at age 20, ", f2(age_rr(ids[i], 40)), " at age 40"), cex = .64)
    }
    box_(84.7, 6.4, 122, 15.2, fill = paper, border = NA)
    tx(86.2, 13.6, "Take-home", cex = .74, font = 2)
    lines_ <- c("Below about age 30, smoking was associated with less recorded",
                "GH/PE; above it, with more. All three definitions agree.",
                "Overall estimates average the two and conceal the reversal.")
    for (k in seq_along(lines_)) tx(86.2, 12.0 - 1.45 * (k - 1), lines_[k], cex = .66)
    tx(84.7, 5.0, "Bands: pointwise 95% CI (HC0); markers at 1: fitted crossover with local 95% CI.", cex = .5, col = mute)
    tx(84.7, 3.9, "GH/PE: gestational hypertension or preeclampsia.", cex = .5, col = mute)
  })
}

# Figure 2: standardized risks, risk ratios and risk differences by maternal age, one row per
# comparison, with the fitted crossover and the simultaneous sign regions marked.
figure_2 <- function() {
  ages <- read_result("main", "main_age_estimates.csv")
  ages <- ages[ages$covariance == "HC0" & ages$age >= 15 & ages$age <= 45, ]
  totals <- read_result("main", "main_overall.csv")
  cross <- crossover_estimates()
  signs <- read_result("main", "crossover_simultaneous_sign_regions.csv")
  signs <- signs[signs$covariance == "HC0", ]
  blue <- "#2367A0"; orange <- "#C8641E"; gray <- "#757575"; ink <- "#222222"; band <- "#E8EEF5"; ribbon <- "#DCE8F2"
  rows <- c(primary = "primary_main", broad = "broad_main", prepregnancy = "prepregnancy_main")
  rowlab <- c(primary = "1. Continued vs stopped\nin T1, among women who\nsmoked before pregnancy",
    broad = "2. Before pregnancy and\nin T1 vs neither period", prepregnancy = "3. Any prepregnancy\nsmoking vs none")
  armlab <- list(primary = c("Stopped in T1", "Continued in T1"), broad = c("Neither period", "Both periods"),
    prepregnancy = c("No prepregnancy smoking", "Prepregnancy smoking"))
  rr_lim <- range(c(ages$rr_lower, ages$rr_upper)); rr_lim <- exp(log(rr_lim) + c(-1, 1) * .04)
  rd_lim <- range(c(0, 1000 * ages$rd_lower, 1000 * ages$rd_upper)); rd_lim <- rd_lim + c(-1, 1) * diff(rd_lim) * .05
  risk_lim <- range(c(1000 * ages$risk0_lower, 1000 * ages$risk0_upper, 1000 * ages$risk1_lower, 1000 * ages$risk1_upper))
  risk_lim <- risk_lim + c(-1, 1) * diff(risk_lim) * .05
  figure_pdf("Figure_2_age_specific_risk_RR_RD", 11, 9.4, function() {
    layout(matrix(1:9, 3, 3, byrow = TRUE), widths = c(1, 1, 1))
    par(oma = c(5.2, 12.5, 3.2, .6), mar = c(2.6, 3.6, 1.6, .6), family = "sans", cex = .82, xaxs = "i", mgp = c(2.2, .6, 0))
    for (ct in names(rows)) {
      s <- ages[ages$contrast == ct, ]; s <- s[order(s$age), ]; stopifnot(identical(s$age, 15:45))
      cr <- cross[cross$model_id == rows[[ct]], ]; stopifnot(nrow(cr) == 1)
      sg <- signs[signs$model_id == rows[[ct]], ]
      marker <- function() {
        # par("usr")[3:4] are log10 units on a log axis; rect() takes data units, so the
        # band vanished from the risk-ratio panels until these were converted back.
        yr <- par("usr")[3:4]; if (isTRUE(par("ylog"))) yr <- 10^yr
        rect(cr$lower, yr[1], cr$upper, yr[2], col = band, border = NA)
        abline(v = cr$age, col = gray, lty = 3, lwd = .9)
      }
      # (a) standardized risks
      plot(NA, xlim = c(15, 45), ylim = risk_lim, xlab = "", ylab = "", bty = "l", xaxt = "n", yaxt = "n")
      axis(1, at = seq(15, 45, 5), cex.axis = .85); axis(2, las = 1, cex.axis = .85)
      marker()
      polygon(c(s$age, rev(s$age)), 1000 * c(s$risk0_lower, rev(s$risk0_upper)), border = NA, col = adjustcolor(gray, .18))
      polygon(c(s$age, rev(s$age)), 1000 * c(s$risk1_lower, rev(s$risk1_upper)), border = NA, col = adjustcolor(blue, .18))
      lines(s$age, 1000 * s$risk0, col = gray, lwd = 1.7, lty = 2); lines(s$age, 1000 * s$risk1, col = blue, lwd = 1.7)
      if (ct == "primary") mtext("Standardized GH/PE risk per 1,000", 3, line = .4, cex = .85, font = 2)
      legend("topleft", legend = armlab[[ct]], col = c(gray, blue), lty = c(2, 1), lwd = 1.7, bty = "n", cex = .8, inset = c(.01, 0))
      mtext(rowlab[[ct]], 2, line = 4.6, cex = .8, las = 1, adj = 1, xpd = NA)
      mtext(paste0("N = ", format(totals$n[totals$contrast == ct], big.mark = ",", trim = TRUE)), 2, line = 4.6, at = par("usr")[3] + .1 * diff(par("usr")[3:4]), cex = .72, las = 1, adj = 1, col = gray, xpd = NA)
      # (b) RR on log scale with crossover marker and simultaneous sign regions
      plot(NA, xlim = c(15, 45), ylim = rr_lim, log = "y", xlab = "", ylab = "", bty = "l", xaxt = "n", yaxt = "n")
      ticks <- c(.7, .8, .9, 1, 1.1, 1.25, 1.4); ticks <- ticks[ticks > rr_lim[1] & ticks < rr_lim[2]]
      axis(1, at = seq(15, 45, 5), cex.axis = .85); axis(2, at = ticks, labels = sprintf("%.2f", ticks), las = 1, cex.axis = .85)
      marker(); abline(h = 1, lty = 2, col = ink, lwd = .8)
      polygon(c(s$age, rev(s$age)), c(s$rr_lower, rev(s$rr_upper)), border = NA, col = ribbon)
      lines(s$age, s$rr, col = blue, lwd = 1.7); points(s$age, s$rr, pch = 16, col = blue, cex = .38)
      y0 <- exp(log(rr_lim[1]) + .06 * diff(log(rr_lim)))
      for (i in seq_len(nrow(sg))) if (sg$classification[i] %in% c("strict_negative", "strict_positive"))
        segments(sg$lower[i], y0, sg$upper[i], y0, lwd = 4, col = if (sg$classification[i] == "strict_negative") blue else orange, lend = 1)
      if (ct == "primary") mtext("Adjusted risk ratio (log scale)", 3, line = .4, cex = .85, font = 2)
      text(cr$age, rr_lim[2] * .985, sprintf("crossover %.1f y", cr$age), cex = .7, col = ink, adj = c(.5, 1))
      # (c) RD per 1,000
      plot(NA, xlim = c(15, 45), ylim = rd_lim, xlab = "", ylab = "", bty = "l", xaxt = "n", yaxt = "n")
      axis(1, at = seq(15, 45, 5), cex.axis = .85); axis(2, las = 1, cex.axis = .85)
      marker(); abline(h = 0, lty = 2, col = ink, lwd = .8)
      polygon(c(s$age, rev(s$age)), 1000 * c(s$rd_lower, rev(s$rd_upper)), border = NA, col = ribbon)
      lines(s$age, 1000 * s$rd, col = blue, lwd = 1.7); points(s$age, 1000 * s$rd, pch = 16, col = blue, cex = .38)
      if (ct == "primary") mtext("Adjusted risk difference per 1,000", 3, line = .4, cex = .85, font = 2)
    }
    mtext("Age-specific associations of early-pregnancy smoking with recorded GH/PE, United States births 2016-2024", 3, outer = TRUE, line = 1.5, cex = .95, font = 2)
    mtext("Maternal age (years)", 1, outer = TRUE, line = .4, cex = .9)
    mtext("Shading around curves: pointwise 95% CI (HC0). Vertical dotted line and grey band: fitted crossover and its local 95% CI.", 1, outer = TRUE, line = 2.0, cex = .74)
    mtext("Thick bars at the base of the middle panels: ages where the simultaneous 95% band supports a lower (blue) or higher (orange) risk with smoking.", 1, outer = TRUE, line = 3.1, cex = .74)
    mtext("Each age uses its own empirical covariate reference; connecting lines are visual guides. Full fitted age range 15-45.", 1, outer = TRUE, line = 4.2, cex = .74)
  })
}

# Figure 3: the fitted crossover age and its local interval across the 20 displayed specifications.
figure_3 <- function() {
  cross <- crossover_estimates()
  blue <- "#2367A0"; gray <- "#757575"; ink <- "#222222"
  groups <- list(
    "Main comparisons" = c("primary_main", "broad_main", "prepregnancy_main"),
    "Adjustment set and records (comparison 1)" = c("primary_core_same_cc", "primary_augmented_same_cc", "primary_core_available"),
    "Period and gestation (comparison 1)" = c("primary_recent", "primary_oe28"),
    "Age specification (comparison 1)" = c("age_spec_fewer_age_knots", "age_spec_shifted_age_knots", "age_spec_more_age_knots", "age_spec_age_by_bmi_nuisance"),
    "Fetal-inclusive comparisons (comparison 1)" = c("all_years_shared_core_live", "all_years_shared_core_inclusive",
      "all_years_shared_augmented_live", "all_years_shared_augmented_inclusive", "reporting_years_shared_core_live",
      "reporting_years_shared_core_inclusive", "reporting_years_shared_augmented_live", "reporting_years_shared_augmented_inclusive"))
  ids <- unlist(groups); stopifnot(all(ids %in% cross$model_id), length(ids) == 20)
  pretty <- c(primary_main = "1. Continued vs stopped in T1 (smoked before pregnancy)",
    broad_main = "2. Before pregnancy and in T1 vs neither period", prepregnancy_main = "3. Any prepregnancy smoking vs none",
    primary_core_same_cc = "Core adjustment, same records", primary_augmented_same_cc = "Core + BMI and education, same records",
    primary_core_available = "Core adjustment, core-available records", primary_recent = "2018-2024 only",
    primary_oe28 = "Gestation 28 weeks or more", age_spec_fewer_age_knots = "Two age knots (22, 32)",
    age_spec_shifted_age_knots = "Shifted age knots (23, 29, 36)", age_spec_more_age_knots = "Four age knots (20, 25, 30, 36)",
    age_spec_age_by_bmi_nuisance = "Age-by-BMI nuisance interaction", all_years_shared_core_live = "All years, core: live births only",
    all_years_shared_core_inclusive = "All years, core: fetal deaths included", all_years_shared_augmented_live = "All years, augmented: live births only",
    all_years_shared_augmented_inclusive = "All years, augmented: fetal deaths included", reporting_years_shared_core_live = "Reporting years, core: live births only",
    reporting_years_shared_core_inclusive = "Reporting years, core: fetal deaths included", reporting_years_shared_augmented_live = "Reporting years, augmented: live births only",
    reporting_years_shared_augmented_inclusive = "Reporting years, augmented: fetal deaths included")
  # y positions: one line per model, plus one header line and a gap per group
  ypos <- c(); ylab <- c(); headers <- c(); y <- 0
  for (g in names(groups)) { y <- y - 1; headers[g] <- y; for (id in groups[[g]]) { y <- y - 1; ypos[id] <- y; ylab[id] <- pretty[[id]] }; y <- y - .35 }
  xlim <- c(27, 37)
  figure_pdf("Figure_3_main_crossover", 11.5, 9.2, function() {
    par(mar = c(6.2, 23, 3.6, 10.5), family = "sans", cex = .85, xaxs = "i")
    plot(NA, xlim = xlim, ylim = c(min(ypos) - .8, -.2), xlab = "", ylab = "", axes = FALSE)
    main_rows <- ypos[groups[[1]]]
    rect(xlim[1], min(main_rows) - .5, xlim[2], max(main_rows) + .5, col = "#F3F6FA", border = NA)
    abline(v = seq(27, 37, 1), col = "#EBEBEB", lwd = .6)
    ref <- cross$age[cross$model_id == "primary_main"]; abline(v = ref, col = gray, lty = 3, lwd = .9)
    for (id in ids) {
      r <- cross[cross$model_id == id, ]; yy <- ypos[[id]]
      lo <- max(xlim[1], r$lower); hi <- min(xlim[2], r$upper)
      segments(lo, yy, hi, yy, col = blue, lwd = 2)
      if (r$lower < xlim[1]) arrows(lo + .3, yy, xlim[1] + .05, yy, length = .06, col = blue, lwd = 2)
      if (r$upper > xlim[2]) arrows(hi - .3, yy, xlim[2] - .05, yy, length = .06, col = blue, lwd = 2)
      points(r$age, yy, pch = if (r$opposite_signs_around_main_root) 16 else 1, col = ink, cex = if (id %in% groups[[1]]) 1.05 else .85, lwd = 1.2)
      text(xlim[2] + .25, yy, sprintf("%.1f (%.1f, %.1f)", r$age, r$lower, r$upper), adj = 0, xpd = NA, cex = .8,
        font = if (id %in% groups[[1]]) 2 else 1)
    }
    axis(1, at = seq(27, 37, 1), cex.axis = .85)
    nonmain <- setdiff(ids, groups[[1]])
    axis(2, at = ypos[nonmain], labels = ylab[nonmain], las = 1, tick = FALSE, cex.axis = .8, font.axis = 1)
    for (g in names(groups)) mtext(g, 2, at = headers[[g]], line = .5, las = 1, adj = 1, cex = .8, font = 3, col = "#444444", xpd = NA)
    for (id in groups[[1]]) mtext(ylab[[id]], 2, at = ypos[[id]], line = .5, las = 1, adj = 1, cex = .8, font = 2, xpd = NA)
    box(bty = "l", col = gray)
    text(xlim[2] + .25, -.35, "Crossover age (local 95% CI)", adj = 0, xpd = NA, font = 2, cex = .8)
    title("Fitted age at which the smoking association changes direction, across 20 specifications", cex.main = 1.05, line = 2.2)
    mtext("Complete-case models fitted at ages 15-45; HC0 covariance. Dotted line: main comparison 1.", 3, line = .9, cex = .78)
    mtext("Maternal age at fitted crossover (years)", 1, line = 2.6, cex = .9)
    mtext("Filled: the simultaneous 95% band supports opposite directions on the two sides of the crossover. Open: not established.", 1, line = 4.1, cex = .75)
    mtext("Rows are overlapping analyses, not independent replications. The 2014-2015 historical model is in Figure S7 and Table S5.", 1, line = 5.2, cex = .75)
  })
}

# Figure S1: the smoking-by-BMI and calendar-year sensitivity curves against the original models.
figure_s1 <- function() {
  gray <- "#777777"; blue <- "#2367A0"; ink <- "#222222"
  labs <- c(primary = "1. Continued vs stopped in T1,\nwomen who smoked before pregnancy",
    broad = "2. Before pregnancy and in T1\nvs neither period", prepregnancy = "3. Any prepregnancy smoking\nvs none")
  curves <- read_result("main", "interaction_curves.csv"); curves <- curves[curves$age >= 15 & curves$age <= 45, ]
  stopifnot(nrow(curves) == 186)
  lim <- range(c(0, 1000 * curves$rd_lower, 1000 * curves$rd_upper)); lim <- lim + c(-1, 1) * diff(lim) * .06
  figure_pdf("Figure_S1_interaction_sensitivity", 9, 9, function() {
    par(mfrow = c(3, 1), mar = c(3, 4.5, 3, .7), oma = c(4.8, 0, 3.5, 0), family = "sans", cex = .86, xaxs = "i")
    for (ct in names(labs)) {
      z <- curves[curves$contrast == ct, ]
      plot(NA, xlim = c(15, 45), ylim = lim, xlab = "", ylab = "Adjusted RD per 1,000", bty = "l", xaxt = "n")
      axis(1, at = seq(15, 45, 5)); abline(h = 0, lty = 3, col = ink)
      for (m in c("original", "combined")) {
        q <- z[z$model == m, ]; q <- q[order(q$age), ]; stopifnot(identical(q$age, 15:45))
        col <- if (m == "original") gray else blue
        polygon(c(q$age, rev(q$age)), 1000 * c(q$rd_lower, rev(q$rd_upper)), border = NA, col = adjustcolor(col, .13))
        lines(q$age, 1000 * q$rd, col = col, lwd = 1.5, lty = if (m == "original") 2 else 1)
        points(q$age, 1000 * q$rd, col = col, pch = if (m == "original") 1 else 16, cex = .4)
      }; title(gsub("\n", " ", labs[[ct]]), cex.main = .95, line = 1)
    }
    mtext("Smoking-by-BMI and calendar-year sensitivity, 2016-2024", 3, outer = TRUE, line = 1.8, cex = 1.05)
    mtext("Original: gray dashed/open points; expanded: blue solid/filled points. Same fitted complete cases.", 3, outer = TRUE, line = .4, cex = .75)
    mtext("Maternal age (years)", 1, outer = TRUE, line = .2, cex = .9)
    mtext("Ages 15-45; shading is pointwise HC0 95% uncertainty. Lines connect integer-age estimates.", 1, outer = TRUE, line = 1.8, cex = .78)
    mtext("Integer-age sign brackets are not continuous crossover estimates or crossover confidence intervals.", 1, outer = TRUE, line = 3.1, cex = .75)
  })
}

# Figure S2: could pregnancy loss alone produce the age pattern? Fixed-assumption scenarios.
# Layout: two rows of panels and a legend strip beneath them, so no legend sits on data. The
# caption is wrapped to lines that fit an 11-inch page at the caption size.
figure_s2 <- function() {
  sel <- read_result("bias", "S_selection_left_truncation_scenarios.csv")
  main_age <- function(ct) read_result("models", paste0(ct, "_main_HC0_age_standardized.csv"))
  blue <- "#2367A0"; orange <- "#C8641E"; gray <- "#757575"; ink <- "#222222"; ribbon <- "#DCE8F2"
  # Two loss curves only: the flat 12% and the published age-graded curve.
  curves <- c(flat_12 = "if 12% of pregnancies were lost at every age",
              age_graded = "if loss rose with age as published (10% at 25 to 29, 53% at 45 and over)")
  cols <- c(flat_12 = "#3B7DB5", age_graded = orange)
  shades <- c("2" = "#9ECAE1", "3" = blue, "5" = "#08306B")
  labs <- c(primary = "Comparison 1: continued vs stopped in T1", broad = "Comparison 2: both periods vs neither")
  figure_pdf("Figure_S2_left_truncation", 11, 9.8, function() {
    layout(matrix(c(1, 2, 3, 4, 5, 5), 3, 2, byrow = TRUE), heights = c(1, 1, .42))
    par(mar = c(3.6, 4.6, 3, 1), oma = c(5.6, 0, 2.8, 0), family = "sans", cex = .85, xaxs = "i")
    # Upper row: observed RR and the RR that selection alone would produce. No in-panel legend.
    for (ct in c("primary", "broad")) {
      a <- main_age(ct); a <- a[order(a$age), ]
      z <- sel[sel$contrast == ct & sel$smoking_loss_rr == 1.23 & sel$type_prevalence == 0.1 & sel$type_gh_pe_rr == 3, ]
      ylim <- c(min(0.7, a$rr_lower), max(1.3, a$rr_upper))
      plot(NA, xlim = c(15, 45), ylim = ylim, log = "y", xlab = "", ylab = "Risk ratio (log scale)", bty = "l", xaxt = "n", yaxt = "n")
      axis(1, at = seq(15, 45, 5)); axis(2, at = c(.7, .8, .9, 1, 1.1, 1.25), labels = c("0.70", "0.80", "0.90", "1.00", "1.10", "1.25"), las = 1)
      abline(h = 1, lty = 2, col = ink, lwd = .8)
      polygon(c(a$age, rev(a$age)), c(a$rr_lower, rev(a$rr_upper)), border = NA, col = ribbon)
      lines(a$age, a$rr, col = blue, lwd = 2)
      for (cv in names(curves)) {
        s <- z[z$loss_curve == cv, ]; s <- s[order(s$age), ]
        ok <- !is.na(s$predicted_rr_under_calibrated_mechanism)
        if (!any(ok)) next
        lines(s$age[ok], s$predicted_rr_under_calibrated_mechanism[ok], col = cols[[cv]], lwd = 1.8, lty = 3)
        if (any(!ok)) { last <- max(which(ok)); points(s$age[last] + .5, s$predicted_rr_under_calibrated_mechanism[last], pch = 4, col = cols[[cv]], cex = .9) }
      }
      cal <- unique(z$calibration_age)[1]; abline(v = cal, col = gray, lty = 3)
      infeasible <- all(is.na(z$predicted_rr_under_calibrated_mechanism))
      text(cal, ylim[2] * .99, if (infeasible) "not reachable at any age" else sprintf("matched to the data at age %d", cal),
           cex = .7, adj = c(if (cal < 25) -0.05 else .5, 1), col = gray)
      title(labs[[ct]], cex.main = .95, line = 1.2)
    }
    # Lower row: how much more often smoking would have to cause loss of a hypertension-prone pregnancy.
    for (ct in c("primary", "broad")) {
      z <- sel[sel$contrast == ct & sel$smoking_loss_rr == 1.23 & sel$type_prevalence == 0.1 & sel$loss_curve == "age_graded", ]
      plot(NA, xlim = c(15, 45), ylim = c(1, 45), log = "y", xlab = "", ylab = "Fold-difference in loss needed (log scale)", bty = "l", xaxt = "n", yaxt = "n")
      axis(1, at = seq(15, 45, 5)); axis(2, at = c(1, 2, 5, 10, 20, 40), las = 1)
      abline(h = 1, lty = 2, col = ink, lwd = .8)
      for (rr in c(2, 3, 5)) {
        s <- z[z$type_gh_pe_rr == rr, ]; s <- s[order(s$age), ]
        feas <- s$feasible %in% c(TRUE, "TRUE", 1)
        lines(s$age[feas], s$required_type_differential_delta[feas], col = shades[[as.character(rr)]], lwd = 1.8)
        points(s$age[feas], s$required_type_differential_delta[feas], col = shades[[as.character(rr)]], pch = 16, cex = .5)
        inf <- !feas & s$observed_rr < 1
        # Ages no differential can reach sit on a marker row at the top, clear of the curves.
        if (any(inf)) points(s$age[inf], rep(42, sum(inf)), pch = 4, col = shades[[as.character(rr)]], cex = .8)
      }
      title(paste0(sub(":.*$", "", labs[[ct]]), ": how much more loss smoking would have to cause"), cex.main = .9, line = 1.2)
      mtext("Maternal age (years)", 1, line = 2.3, cex = .8)
    }
    # Legend strip beneath the panels.
    par(mar = c(0, 1, 0, 1)); plot.new()
    legend("left", inset = .01, bty = "n", cex = .8, title = "Upper row", title.adj = 0, title.font = 2,
      legend = c("Observed risk ratio (pointwise 95% CI)", paste("Selection alone,", curves),
                 "Could match only by losing every hypertension-prone pregnancy"),
      col = c(blue, cols, gray), lwd = c(2, 1.8, 1.8, NA), lty = c(1, 3, 3, NA), pch = c(NA, NA, NA, 4), seg.len = 2.4)
    legend("right", inset = .01, bty = "n", cex = .8, title = "Lower row", title.adj = 0, title.font = 2,
      legend = c("Assumed risk ratio of the hidden hypertension-prone group: 2", "Assumed risk ratio: 3",
                 "Assumed risk ratio: 5", "Not reachable by any amount of loss"),
      col = c(shades, gray), lwd = c(1.8, 1.8, 1.8, NA), pch = c(NA, NA, NA, 4), seg.len = 2.4)
    mtext("Could pregnancy loss alone produce the age pattern?", 3, outer = TRUE, line = 1.0, cex = 1.05, font = 2)
    mtext("Upper row: the selection mechanism is matched to the observed risk ratio at the age of strongest inverse association, then held fixed;", 1, outer = TRUE, line = 1.3, cex = .72)
    mtext("dotted curves show what it then implies at other ages. Lower row: how many times more often smoking would have to cause the loss of a", 1, outer = TRUE, line = 2.4, cex = .72)
    mtext("hypertension-prone pregnancy than of any other pregnancy, at each age, for there to be no true association.", 1, outer = TRUE, line = 3.5, cex = .72)
    mtext("Fixed-assumption scenarios; nothing is estimated from the data (loss by age: Magnus et al 2019; smoking and loss: Pineles et al 2014).", 1, outer = TRUE, line = 4.6, cex = .72)
  })
}

# One age curve panel of the supplementary fit figures S3 to S5.
supp_curve_panel <- function(ages, ids, labels, cols, measure = "rr", title = "", ylab = NULL,
                             legend_pos = "topleft", ref_line = if (measure == "rr") 1 else 0) {
  ink <- "#222222"
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

# Figure S3: three-level early smoking against one shared reference.
figure_s3 <- function() {
  ages <- read_result("sensitivity", "supp_age_estimates.csv")
  blue <- "#2367A0"; gray <- "#757575"
  figure_pdf("Figure_S3_three_level_early_smoking", 10.5, 4.6, function() {
    par(mfrow = c(1, 2), mar = c(3.6, 4.4, 3, 1), oma = c(3.6, 0, 2.2, 0), family = "sans", cex = .85, xaxs = "i")
    ids <- c("supp_3lvl_stopped_vs_none", "supp_3lvl_continued_vs_none"); labs <- c("Stopped by T1 vs no early smoking", "Continued in T1 vs no early smoking")
    supp_curve_panel(ages, ids, labs, c(gray, blue), "rr", "Risk ratio, common reference")
    supp_curve_panel(ages, ids, labs, c(gray, blue), "rd", "Risk difference per 1,000, common reference")
    mtext("Three-level early smoking: women who stopped by the first trimester and women who continued, each against no early smoking", 3, outer = TRUE, line = .6, cex = .95, font = 2)
    mtext("Maternal age (years)", 1, outer = TRUE, line = .2, cex = .9)
    mtext("Both groups standardized to the covariate distribution of all three groups at each age. Shading: pointwise 95% CI (HC0).", 1, outer = TRUE, line = 1.5, cex = .74)
  })
}

# Figure S4: dose-response by maternal age.
figure_s4 <- function() {
  ages <- read_result("sensitivity", "supp_age_estimates.csv")
  pal <- c("#9ECAE1", "#4292C6", "#2166AC", "#08306B", "#C8641E", "#7A3B00")
  figure_pdf("Figure_S4_dose_response", 11, 8.4, function() {
    par(mfrow = c(2, 2), mar = c(3.6, 4.4, 3, 1), oma = c(4.6, 0, 2.2, 0), family = "sans", cex = .85, xaxs = "i")
    t1 <- paste0("supp_t1dose_", c("1_5", "6_10", "11_20", "21plus")); t1l <- paste("T1", c("1-5", "6-10", "11-20", "21 or more"), "cigarettes/day vs 0")
    # RR panels: legends bottom-right, where no curve or CI band runs (the top-left carries the
    # wide young-age bands).
    supp_curve_panel(ages, t1, t1l, pal[1:4], "rr", "First-trimester dose, women who smoked before pregnancy", legend_pos = "bottomright")
    supp_curve_panel(ages, t1, t1l, pal[1:4], "rd", "First-trimester dose, risk difference")
    pd <- paste0("supp_pdose_", c("1_5", "6_10", "11_20", "21plus")); pdl <- paste("Prepregnancy", c("1-5", "6-10", "11-20", "21 or more"), "cigarettes/day vs 0")
    supp_curve_panel(ages, pd, pdl, pal[1:4], "rr", "Prepregnancy dose vs no prepregnancy smoking", legend_pos = "bottomright")
    ch <- c("supp_t1change_reduced", "supp_t1change_same_or_more"); chl <- c("Reduced dose in T1 vs stopped", "Same or higher dose in T1 vs stopped")
    supp_curve_panel(ages, ch, chl, c(pal[2], pal[4]), "rr", "Dose change in T1 vs stopped")
    mtext("Dose-response by maternal age", 3, outer = TRUE, line = .6, cex = 1, font = 2)
    mtext("Maternal age (years)", 1, outer = TRUE, line = .2, cex = .9)
    mtext("First-trimester dose rows adjust for prepregnancy dose and share one reference (all women who smoked before pregnancy); prepregnancy dose shares the prepregnancy population reference.", 1, outer = TRUE, line = 1.5, cex = .72)
    mtext("Shading: pointwise 95% CI (HC0). Connecting lines are visual guides.", 1, outer = TRUE, line = 2.6, cex = .72)
  })
}

# Figure S5: preterm birth as a control outcome, and the crossover age by race and ethnicity stratum.
figure_s5 <- function() {
  ages <- read_result("sensitivity", "supp_age_estimates.csv")
  summ <- read_result("sensitivity", "supp_summary.csv")
  blue <- "#2367A0"; gray <- "#757575"; ink <- "#222222"
  figure_pdf("Figure_S5_control_outcome_and_strata", 11, 8.4, function() {
    layout(matrix(c(1, 2, 3, 4, 4, 4), 2, 3, byrow = TRUE), heights = c(1, 1.1))
    par(mar = c(3.6, 4.4, 3, 1), oma = c(4.6, 0, 2.2, 0), family = "sans", cex = .85, xaxs = "i")
    pre <- c(primary = "supp_preterm_primary", broad = "supp_preterm_broad", prepregnancy = "supp_preterm_prepregnancy")
    lab <- c(primary = "comparison 1", broad = "comparison 2", prepregnancy = "comparison 3")
    # The panel title already names the outcome; an in-panel legend only sat on the reference line.
    for (ct in names(pre)) supp_curve_panel(ages, pre[[ct]], "Preterm birth (<37 weeks)", blue, "rr", paste0("Preterm birth (<37 weeks): ", lab[[ct]]), legend_pos = NULL)
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
    mtext("Comparison 1", 2, at = 9.9, line = .5, las = 1, adj = 1, font = 3, cex = .8, col = "#444444", xpd = NA)
    mtext("Comparison 2", 2, at = 4.9, line = .5, las = 1, adj = 1, font = 3, cex = .8, col = "#444444", xpd = NA)
    box(bty = "l", col = gray); title("Crossover age by race and ethnicity stratum (race covariate omitted)", cex.main = .95, line = 1)
    text(45.4, 9.9, "Crossover (local 95% CI)", adj = 0, xpd = NA, font = 2, cex = .78)
    mtext("Outcome specificity and modification by race and ethnicity", 3, outer = TRUE, line = .6, cex = 1, font = 2)
    mtext("Maternal age (years)", 1, outer = TRUE, line = .2, cex = .9)
    mtext("Top: preterm birth replaces GH/PE as the outcome on the same complete-case records. Bottom: filled points, simultaneous band supports opposite directions; open, not established.", 1, outer = TRUE, line = 1.5, cex = .72)
  })
}
