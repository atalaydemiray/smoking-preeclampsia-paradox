\name{natality_changes}
\alias{natality_changes}
\alias{print.natality_changes}
\title{Inspect recorded annual changes for a variable}
\usage{
natality_changes(variable, years = NULL)
\method{print}{natality_changes}(x, ...)
}
\arguments{
\item{variable}{A source or local catalog variable name.}
\item{years}{Optional years used to restrict comparisons and notes.}
\item{x}{A natality change-history object.}
\item{...}{Additional arguments reserved for print methods.}
}
\value{A change-history object with rule eras, adjacent-year comparisons, and notes.}
\description{Exposes documented annual differences without claiming that unchanged text establishes comparability.}
