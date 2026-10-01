\name{read_natality}
\alias{read_natality}
\title{Read selected natality source variables}
\usage{
read_natality(
  year,
  vars,
  path = NULL,
  population = c("all_occurrence", "us_residents"),
  download = TRUE
)
}
\arguments{
\item{year}{One or more supported data years.}
\item{vars}{Source or local catalog variable names.}
\item{path}{Optional local annual Stata, RDS, or Parquet file. For multiple years, a character vector named by every requested year.}
\item{population}{All occurrence records or births to U.S. residents.}
\item{download}{Allow a package-hosted download when a validated, published annual release exists. Cached files remain available when false.}
}
\value{A data frame containing the requested source fields and an explicit year column.}
\description{Reads only requested source variables and applies an explicit residence-universe choice.}
\details{When \code{path} is omitted, a verified cache entry is used. Private locally validated releases can first be imported with \code{natality_cache_import()}. Downloading requires a published release with an HTTPS URL. Multiple-year reads warn that comparability is not established, retain annual provenance, and refuse differing source names, types or labels. Fields must be present in every requested year; missing fields are not silently filled.

Recorded source-value discrepancies trigger a warning when their unlisted codes appear in a requested field. Values remain unchanged and the result's provenance records the issue. Published downloads may use manifest-listed HTTPS mirrors, each verified against the same pinned artifact. The complete annual file is downloaded once; selected-column reads do not reduce that initial download.

Requests for \code{F_MAR_P} warn that this is a paternity-acknowledgment reporting flag, not a validated marital-status reporting flag. Requests for \code{DMAR} from 2017 onward warn that NCHS withholds record-level marital status for births occurring in or to residents of California. Interpretation notes are retained in annual provenance; source fields and blanks remain unchanged.}
