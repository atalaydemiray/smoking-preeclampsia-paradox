# The analysis-populations figure and the interaction-sensitivity figure (package Figure S1 under
# addendum D), re-rendered with people-first labels and the manuscript's comparison wording. Same
# checked aggregate inputs as the released figures, no refitting, no number changes. The released
# renders said "prepregnancy smokers" and "neither window"; the manuscript says "women who smoked
# before pregnancy" and "neither period". Output names carry no figure number; the package builder
# assigns them. Writes to age45_revision/outputs/supplementary/figures.
# From 01_Analysis: Rscript --vanilla age45_revision/publication_code/render_revised_additional_figures.R
main <- function() {
  input <- "age45_revision/submission_staging/Code/aggregate_inputs"
  figs <- "age45_revision/outputs/supplementary/figures"; dir.create(figs, recursive = TRUE, showWarnings = FALSE)
  read <- function(x) read.csv(file.path(input, x), check.names = FALSE)
  overall <- read("main_overall.csv")
  ids <- c("primary_main", "broad_main", "prepregnancy_main")
  labels <- c("Continued vs stopped smoking in T1,\namong women who smoked before pregnancy",
    "Smoking before pregnancy and in T1\nvs neither period",
    "Any prepregnancy smoking vs none,\nirrespective of T1"); names(labels) <- ids
  labs <- c(primary = "1. Continued vs stopped in T1,\nwomen who smoked before pregnancy",
    broad = "2. Before pregnancy and in T1\nvs neither period", prepregnancy = "3. Any prepregnancy smoking\nvs none")
  blue <- "#2367A0"; gray <- "#777777"; ink <- "#222222"
  num <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  draw <- function(name, w, h, fun) {
    path <- file.path(figs, paste0(name, ".pdf"))
    if (Sys.info()[["sysname"]] == "Darwin") grDevices::quartz(type = "pdf", file = path, width = w, height = h, family = "Helvetica")
    else grDevices::cairo_pdf(path, width = w, height = h, family = "Helvetica")
    tryCatch(fun(), finally = grDevices::dev.off()); message("wrote ", path)
  }
  stopifnot(identical(overall$contrast, c("primary", "broad", "prepregnancy")))
  curves <- read("interaction_curves.csv"); curves <- curves[curves$age >= 15 & curves$age <= 45, ]
  stopifnot(nrow(curves) == 186)
  lim <- range(c(0, 1000 * curves$rd_lower, 1000 * curves$rd_upper)); lim <- lim + c(-1, 1) * diff(lim) * .06
  draw("interaction_sensitivity", 9, 9, function() {
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
    mtext("Separate two-group fits: base model gray dashed; expanded model blue solid. Same complete cases.", 3, outer = TRUE, line = .4, cex = .75)
    mtext("Maternal age (years)", 1, outer = TRUE, line = .2, cex = .9)
    mtext("Ages 15-45; shading is pointwise HC0 95% uncertainty. Lines connect integer-age estimates.", 1, outer = TRUE, line = 1.8, cex = .78)
    mtext("Integer-age sign brackets are not continuous crossover estimates or crossover confidence intervals.", 1, outer = TRUE, line = 3.1, cex = .75)
  })
  invisible(NULL)
}
main()
