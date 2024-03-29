% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NMinfo.R
\name{NMinfo}
\alias{NMinfo}
\title{Get metadata from an NMdata object}
\usage{
NMinfo(data, info, as.fun)
}
\arguments{
\item{data}{An object of class NMdata (a result of `NMscanData()`)}

\item{info}{If not passed, all the metadata is returned. You can
use "details", "tables", or "columns" to get only these
subsets. If info is "tables" or "columns"}

\item{as.fun}{The default is to return data as a `data.frame`. Pass
a function (say `tibble::as_tibble`) in as.fun to convert to
something else. If `data.table`s are wanted, use
`as.fun="data.table"`. The default can be configured using
`NMdataConf()`.}
}
\value{
A table of class as defined by as.fun in case info is
    "columns" or "tables". A list if info missing or equal to
    "details".
}
\description{
Extract metadata such as info on tables, columns and further
details in your favorite class
}
