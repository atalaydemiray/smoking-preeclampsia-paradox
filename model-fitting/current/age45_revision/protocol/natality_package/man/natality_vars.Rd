\name{natality_vars}
\alias{natality_vars}
\title{Search annual U.S. natality source variables}
\usage{natality_vars(pattern = NULL, years = NULL)}
\arguments{
\item{pattern}{Optional regular expression matched against source names, descriptions, and annual rule text.}
\item{years}{Optional years used to restrict availability.}
}
\value{A data frame with one row per matching canonical source name.}
\description{Searches the bundled 2014--2024 annual source-variable catalog.}

\details{Searches the canonical U.S.-file layouts, excluding territory-only addenda. Spelling changes remain separate source names. No scientific equivalence is inferred from similar names or labels.}
