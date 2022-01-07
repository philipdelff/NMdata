
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NMdata

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/philipdelff/NMdata/workflows/R-CMD-check/badge.svg)](https://github.com/philipdelff/NMdata/actions)
[![Codecov test
coverage](https://codecov.io/gh/philipdelff/NMdata/branch/master/graph/badge.svg)](https://codecov.io/gh/philipdelff/NMdata?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/NMdata)](https://CRAN.R-project.org/package=NMdata)
<!-- badges: end -->

## A fast R package for efficient data preparation, consistency-checking and post-processing in PK/PD modeling

Pharmacometrics and PK/PD modeling offers unique information for
decision-making in several steps of drug development. However, the leg
work in pharmacometrics remains technical, and this is a typical
bottleneck for a pharmacometrician to contributing even more.

Preparing data sets - and if you use Nonmem, reading the results data -
can be tedious, and mistakes can lead to hours of frustration. NMdata
provides useful tools (including automated checks) for these trivial
tasks. The aim is to automate the book keeping and allow more time for
actual modeling.

A central design feature of NMdata is that all included tools require as
little as possible about how the user works. *Any functionality in the
NMdata can be used independently of the rest of the package*, and NMdata
is not intended to force you to change any habits or preferences.
Instead, NMdata tries to fit in with how you (or your colleague who
worked on the project before you) do things.

The data set creation tools in NMdata may be equally interesting to
users of nlmixr or other proprietary software than Nonmem. However, many
features of the package relates to the final organization and writing of
data for Nonmem, and reading data from Nonmem after a model run.

The best place to browse information about the package is
[here](https://philipdelff.github.io/NMdata/). All documentation is of
course included in the package itself too.

## Prepare, check, and export data for NONMEM

On the data-generation side, functionality is provided for documentation
of the datasets while generating them. Check out [this
vignette](https://philipdelff.github.io/NMdata/articles/DataPrepare.html)
on the topic. There are functions for automatic checks of (some) data
merges, handling and counting of exclusions flags, final preparations
for ensuring readability in Nonmem, and ensuring traceability of
datasets back to data generation scripts.

## Check data as read by NONMEM

Finally, the `NMcheckData` function will do an extensive and fully
automated set of checks of the data before you run NONMEM. And did
NONMEM not behave? `NMcheckData` can debug the data *as seen by NONMEM*.
That’s right - it has never been easier to find data bugs.

## Automated and general reader of Nonmem results data

Reading the resulting data from Nonmem can require a few manual steps.
Especially because all modelers seem to do things a little differently.
`NMscanData` can return all data output (`$TABLE`) from Nonmem combined,
and if wanted with additional columns and rows in input data. It’s as
simple as

``` r
res <- NMscanData("xgxr001.lst",recover.rows=TRUE)
#> Model:  xgxr001 
#> Input and output data merged by: ROW 
#> 
#> Used tables, contents shown as used/total:
#>               file      rows columns     IDs
#>    xgxr001_res.txt   905/905   16/16 150/150
#>  xgxr1.csv (input) 1502/1502   22/24 150/150
#>           (result)      1502    38+2     150
#> 
#> Distribution of rows on event types in returned data:
#>  EVID Input only Output
#>     0        597    755
#>     1          0    150
## plot a subset of the result
res.plot <- subset(res,ID%in%c(113,135)&EVID==0)
library(ggplot2)
ggplot(res.plot,aes(TIME))+
    geom_point(aes(y=DV,colour=flag))+
    geom_line(aes(y=PRED))+
    facet_wrap(~trtact)+
    labs(y="Concentration (unit)",subtitle=unique(res.plot$model),colour="Observations",
         caption="NOTICE:\nObservations are coloured by a character column fetched from input data.\nSamples below LLOQ are rows added from input data.\nPlots are correctly sorted because factor levels of dose are preserved from input data.")+
    theme_bw()+theme(legend.position="bottom")
#> Warning: Removed 4 row(s) containing missing values (geom_path).
```

<img src="man/figures/README-NMscanData-example1-1.png" width="100%" />

Want a tibble instead? Easy:

``` r
res.tibble <- NMscanData("xgxr001.lst",as.fun=tibble::as_tibble,quiet=TRUE)
```

Or a data.table? This time, we’ll configure NMdata to return data.tables
by default:

``` r
NMdataConf(as.fun="data.table")
res.dt <- NMscanData("xgxr001.lst",quiet=TRUE)
```

NMscanData is very general, and should work with all kinds of models,
and all kinds of other software and configurations. Take a look at [this
vignette](https://philipdelff.github.io/NMdata/articles/NMscanData.html)
for more info on the Nonmem data reader. Then you will learn how to
access the meta data that will allow you to trace every step that was
taken combining the data and the many checks that were done along the
way too.

## How to install

`NMdata` is on [CRAN](https://cran.r-project.org/),
[MRAN](https://mran.microsoft.com/), and [MPN](https://mpn.metworx.com).
To install from the package archive you are already using, do:

    install.packages("NMdata")
    library(NMdata)

If your archive has not been updated since July 2021, you may not find
`NMdata` that way. In that case you have two other options. You can
explicitly select CRAN for the installation. Or if you should want a
version that has not yet reached CRAN, installing from Github is easy
too.

    ## Option 2: Install explicitly from CRAN
    install.packages("NMdata",repos="https://cloud.r-project.org")
    library(NMdata)

    ## Option 3: Install from github
    library(remotes)
    install_github("philipdelff/NMdata")
    library(NMdata)

If you use the Github version, you may want to see the
[FAQ](https://philipdelff.github.io/NMdata/articles/NMdata-FAQ.html) for
how to install specific releases from Github (ensuring reproducibility).

## Questions?

Check the
[FAQ](https://philipdelff.github.io/NMdata/articles/NMdata-FAQ.html), or
ask on [github](https://github.com/philipdelff/NMdata/)

## Issues?

The best way to report a bug or to request features is on
[github](https://github.com/philipdelff/NMdata/).

## Code of Conduct

Please note that the patchwork project is released with a [Contributor
Code of
Conduct](https://philipdelff.github.io/NMdata/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
