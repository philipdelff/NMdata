# NMdata <a href='https://philipdelff.github.io/NMdata/'><img src='devel/NMdata_Rpackage/NMdata_logo_v01.png' align="right" height="131.5" /></a>
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/philipdelff/NMdata/workflows/R-CMD-check/badge.svg)](https://github.com/philipdelff/NMdata/actions)
[![Codecov test coverage](https://codecov.io/gh/philipdelff/NMdata/branch/master/graph/badge.svg)](https://codecov.io/gh/philipdelff/NMdata?branch=master)
<!-- badges: end -->

### A fast R package for efficient data preparation, consistency-checking and post-processing in PK/PD modeling
NMdata provides data creation and data handling tools for
pharmacometrics (PK/PD modeling). The tools are created around the use
of Nonmem for estimation or simulation. Data set creation tools may be
equally users of nlmixr or other proprietary software than
Nonmem. However, so far, the most elaborated features of the package
relates to the final organization and writing of data for Nonmem, and
reading data from Nonmem after a model run.

Nonmem is a flexible tool for PK and PK/PD modeling. However, creating
the datasets and reading the data resulting from running Nonmem can be
tedious, and mistakes can lead to hours of frustration. This package
provides useful tools (including automated checks) for these trivial
tasks so we can spend more time on the actual analysis.

The best place to browse information about the package is
[here](https://philipdelff.github.io/NMdata). All documentation is of
course included in the package itself too.

## How to install
`NMdata` is aimed at CRAN release in near future. Meanwhile, installing
from Github is easy:

```
library(remotes)
install_github("philipdelff/NMdata")
library(NMdata)
```
See the
[FAQ](https://philipdelff.github.io/NMdata/articles/NMdata-FAQ.html)
for how to install specific releases from Github (ensuring reproducibility).


## Automated and general reader of Nonmem output data
Reading the resulting data from Nonmem can require quite a bit of
manual steps. Especially because all modelers seem to do things a
little differently. The frustrating fact is that we always want the
same out of these efforts. This is all data output from Nonmem combined with additional columns and sometimes rows in
input data. NMdata automates this process and can save you a
lot of time. It can also break down the data depending on level of variability (eg. subject or occasion level).

Take a look at [this vignette](https://philipdelff.github.io/NMdata/articles/NMscanData.html)
for more info on the Nonmem data reader. Once `NMdata` is installed, you
can of course access the vignette from within R:

```
vignette("NMscanData")
``` 


## Create data, export to Nonmem
On the data-generation side, functionality is provided for
documentation of the datasets while generating them. Check out [this
vignette](https://philipdelff.github.io/NMdata/articles/DataCreate.html)
on the topic. There are functions for automatic checks of (some) data
merges, handling and counting of exclusions flags, final
preparations for ensuring readability in Nonmem, and ensuring
traceability of datasets back to data generation scripts.

## Questions?
Check the [FAQ](https://philipdelff.github.io/NMdata/articles/FAQ.html), or ask on [github
page](https://github.com/philipdelff/NMdata)

## Feedback?
The best way to request features, report bugs etc. is by the [github
page](https://github.com/philipdelff/NMdata).
