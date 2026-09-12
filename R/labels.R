# People-first labels.
#
# Every label, header and footnote that reaches a table or a figure names the person first and
# the behaviour after: "women who smoked before pregnancy", never "smokers". The estimates are
# stored under the short variable names the models use, so this is the one place where those
# names become the wording the manuscript uses. `check_people_first()` is called before any
# table is written, so a label that slips through fails loudly here instead of reaching a reader.

relabel_exact <- c(
  "Within prepregnancy smokers" = "Continued vs stopped smoking in T1, women who smoked before pregnancy",
  "Both early windows vs neither" = "Smoking before pregnancy and in T1 vs neither period",
  "Prepregnancy smoking vs none" = "Any prepregnancy smoking vs none",
  "Prepregnancy-only" = "Any prepregnancy smoking vs none",
  "Prepregnancy smoking, irrespective of T1" = "Any prepregnancy smoking vs none, irrespective of T1"
)

relabel_substring <- list(
  c("Own complete-case prepregnancy smokers at same age",
    "Own complete-case women who smoked before pregnancy at the same age"),
  c("T1 smoking versus T1 abstinence among prepregnancy smokers",
    "continued versus stopped smoking in T1 among women who smoked before pregnancy"),
  c("among prepregnancy smokers", "among women who smoked before pregnancy"),
  c("within reported prepregnancy smokers", "within women who reported smoking before pregnancy"),
  c("within prepregnancy smokers", "within women who smoked before pregnancy"),
  c("Within-smoker comparison",
    "Continued versus stopped smoking in T1 among women who smoked before pregnancy"),
  c("within-smoker subset", "subset of women who smoked before pregnancy"),
  c("primary within-smoker population", "primary population of women who smoked before pregnancy"),
  c("N=1,980,559 within smokers", "N=1,980,559 for women who smoked before pregnancy"),
  c("Within smokers:", "Women who smoked before pregnancy:"),
  c("lifetime never smokers", "women who have never smoked"),
  c("prepregnancy smokers", "women who smoked before pregnancy"),
  c("neither window", "neither period")
)

relabel <- function(x) {
  x <- as.character(x)
  hit <- !is.na(x) & x %in% names(relabel_exact)
  x[hit] <- unname(relabel_exact[x[hit]])
  for (pair in relabel_substring) x <- gsub(pair[1], pair[2], x, fixed = TRUE)
  x
}

# Wording that names people by a behaviour. Variable names inside code are the one exception,
# so this is applied to table content, never to the scripts themselves.
avoid_terms <- c("smokers", "nonsmokers", "non-smokers", "quitters", "diabetics", "the obese")

check_people_first <- function(rows, where) {
  flat <- tolower(as.character(unlist(rows, use.names = FALSE)))
  for (term in avoid_terms) {
    bad <- grep(paste0("(^|[^a-z])", term, "([^a-z]|$)"), flat, value = TRUE)
    if (length(bad)) {
      stop("people-first check failed in ", where, ": ", term, " in ", substr(bad[1], 1, 90))
    }
  }
  invisible(TRUE)
}
