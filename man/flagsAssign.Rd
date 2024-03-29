% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flagsAssign.R
\name{flagsAssign}
\alias{flagsAssign}
\title{Assign exclusion flags to a dataset based on specified table}
\usage{
flagsAssign(
  data,
  tab.flags,
  subset.data,
  col.flagn,
  col.flagc,
  flags.increasing = FALSE,
  grp.incomp = "EVID",
  flagc.0 = "Analysis set",
  as.fun = NULL
)
}
\arguments{
\item{data}{The dataset to assign flags to.}

\item{tab.flags}{A data.frame containing at least these named
columns: FLAG, flag, condition. Condition is disregarded for
FLAG==0. FLAG must be numeric and non-negative, flag and
condition are characters.}

\item{subset.data}{An optional string that provides a subset of
data to assign flags to. A common example is
subset=\"EVID==0\" to only assign to observations. Numerical
and character flags will be missing in rows that are not
matched by this subset.}

\item{col.flagn}{The name of the column containing the numerical
flag values in tab.flags. This will be added to data. Default
value is FLAG and can be configured using NMdataConf.}

\item{col.flagc}{The name of the column containing the character
flag values in tab.flags. This will be added to data. Default
value is flag and can be configured using NMdataConf.}

\item{flags.increasing}{The flags are applied by either decreasing
(default) or increasing value of col.flagn. Decreasing order
means that conditions associated with higher values of
col.flagn will be evaluated first. By using decreasing order,
you can easily adjust the Nonmem IGNORE statement from
IGNORE(FLAG.NE.0) to say IGNORE(FLAG.GT.10) if BLQ's have
FLAG=10, and you decide to include these in the analysis.}

\item{grp.incomp}{Column(s) that distinct incompatible subsets of
data. Default is "EVID" meaning that if different values of
EVID are found in data, the function will return an
error. This is a safeguard not to mix data unintentionally
when counting flags.}

\item{flagc.0}{The character flag to assign to rows that are not
matched by exclusion conditions (numerical flag 0).}

\item{as.fun}{The default is to return data.tables if input data
is a data.table, and return a data.frame for all other input
classes. Pass a function in as.fun to convert to something
else. If return.all=FALSE, this is applied to data and
tab.flags independently.}
}
\value{
The dataset with flags added. Class as defined by
    as.fun. See parameter flags.return as well.
}
\description{
The aim with this function is to take a (say PK) dataset and a
pre-specified table of flags, assign the flags automatically.
}
\details{
dt.flags must contain a column with numerical exclusion
    flags, one with character exclusion flags, and one with a
    expressions to evaluate for whether to apply the exclusion
    flag. The flags are applied sequentially, by increasing value
    of the numerical exclusion flag.
}
\examples{
\dontrun{
pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
dt.flags <- data.frame(
       flagn=10,
       flagc="Below LLOQ",
       condition=c("BLQ==1")
)
pk <- flagsAssign(pk,dt.flags,subset.data="EVID==0",col.flagn="flagn",col.flagc="flagc")
pk <- flagsAssign(pk,subset.data="EVID==1",flagc.0="Dosing",
        col.flagn="flagn",col.flagc="flagc")
unique(pk[,c("EVID","flagn","flagc","BLQ")])
flagsCount(pk[EVID==0],dt.flags,col.flagn="flagn",col.flagc="flagc")
}
}
\seealso{
Other DataCreate: 
\code{\link{NMorderColumns}()},
\code{\link{NMstamp}()},
\code{\link{NMwriteData}()},
\code{\link{addTAPD}()},
\code{\link{findCovs}()},
\code{\link{findVars}()},
\code{\link{flagsCount}()},
\code{\link{mergeCheck}()},
\code{\link{tmpcol}()}
}
\concept{DataCreate}
