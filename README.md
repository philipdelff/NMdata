# NMdata
Nonmem is a flexible tool for PK and PK/PD modeling. However, creating
the datasets and reading the data resulting from running Nonmem can be
tedious, and mistakes can lead to hours of frustration. This package
provides useful tools (including automated checks) for these trivial
tasks so we can spend more time on the actual analysis.

The best place to browse information about the package is
[here](https://philipdelff.github.io/NMdata). All documentation is of
course included in the package itself too.

## How to Install
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
same - all data output from Nonmem combined with additional columns and sometimes rows in
input data. This package automates this process and can save you a
lot of time. It can also break down the data depending on level of variability (eg. subject or occasion level).

Take a look at [this vignette](https://philipdelff.github.io/NMdata/articles/NMscanData.html)
for more info on the Nonmem data reader. Once `NMdata` is installed, you
can of course access the vignette from within R:

```
vignette("NMscanData")
``` 

No more having to look at the .lst to see which tables to read. No
more having to read multiple tables to get all the variables. No more
having to merge with input data to get variables that were not
exported with nonmem or rows that were ignored. This and more is done
automatically - of course including a lot of checks for things that
will or may creat trouble.


## Create data, export to Nonmem
On the data-generation side, functionality is provided for
documentation of the datasets while generating them. Check out [this
vignette](https://philipdelff.github.io/NMdata/articles/DataCreate.html)
on the topic. There are functions for automatic checks of (some) data
merges, handling of exclusions flags, final preparations for ensuring
readability in Nonmem, and ensuring traceability of datasets back to
data generation scripts.

## Questions?
Check the [FAQ](https://philipdelff.github.io/NMdata/articles/FAQ.html), or ask on [github
page](https://github.com/philipdelff/NMdata)

## Feedback?
The best way to request features, report bugs etc. is by the [github
page](https://github.com/philipdelff/NMdata).
