\name{natality_codebook}
\alias{natality_codebook}
\alias{print.natality_codebook}
\title{Read annual source documentation for a variable}
\usage{
natality_codebook(variable, years = NULL)
\method{print}{natality_codebook}(x, ...)
}
\arguments{
\item{variable}{A source or local catalog variable name.}
\item{years}{Optional years to return.}
\item{x}{A natality codebook object.}
\item{...}{Additional arguments reserved for print methods.}
}
\value{A codebook object containing annual source entries.}
\description{Returns source fields, locations, rules, review state, and manual links by year.}

\details{The column observed_conversion_storage describes the inspected prior conversion, not the source-preserving package artifact. Printed width and position span remain separate so documentation discrepancies are visible. Package Parquet source fields are stored as character text.

The source_observations component reports recorded source-data discrepancies for the requested field and years. These observations do not assign undocumented code meanings or establish clinical comparability.}
