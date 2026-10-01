\name{natality_cache_dir}
\alias{natality_cache_dir}
\alias{natality_cache_info}
\alias{natality_cache_import}
\alias{natality_cache_clear}
\title{Manage the natality data cache}
\usage{
natality_cache_dir(create = FALSE)
natality_cache_info()
natality_cache_import(year, path)
natality_cache_clear(years = NULL)
}
\arguments{
\item{year}{One supported data year.}
\item{path}{Local release artifact matching the bundled size and SHA-256. The original is unchanged.}
\item{create}{Create the cache directory when it does not exist.}
\item{years}{Optional annual files to remove; omission selects all natality annual cache files.}
}
\value{The cache path, a cache inventory, or removed paths invisibly.}
\description{Locates, inventories, and selectively clears package-managed annual data files. Downloads and local imports enter this cache only after size and SHA-256 verification. Locally validated private artifacts can be imported and read offline before public hosting is available.}
