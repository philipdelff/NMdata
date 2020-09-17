# NMdata
Nonmem provides a flexible toolbox for PK and PK/PD modeling. However,
creating the datasets and reading the data resulting from running
Nonmem can be tedious. This package provides useful tools for these
trivial tasks so we can spend more time on the actual analysis. 

## How to Install
NMdata is aimed at CRAN release in near future. Meanwhile, installing from Github
is easy:

```
library(remotes)
install_github("philipdelff/NMdata",build_vignettes=TRUE)
library(NMdata)
```

## Automated and general reader of Nonmem output data
Reading the resulting data from Nonmem can require quite a bit of
manual steps. Especially because all modelers seem to do
things a little differently. The frustrating fact is that we always
want the same - all data output from Nonmem combined with additional columns in
input data, and then possibly broken down to the different levels
of variability. This package automates this process and can save you a
lot of time. 

Take a look at [this vignette](https://philipdelff.github.io/NMdata/articles/NMscanData.html)
for more info on the Nonmem data reader. Once NMdata is installed, you
can of course access the vignette from within R:

```
vignette("NMscanData")
``` 

No more having to look at the
.lst to see which tables to read. No more having to read multiple
tables to get all the variables. No more having to merge with input
data to get variables that were not exported with nonmem. This and
much more is done automatically.


## Hightlights
These are the steps that this package especially helps with. 

### Create data, export to Nonmem
On the data-generation side, functionality is provided for
documentation of the datasets while generating them. 

#### flagsAssign
Assign exclusion flags to a dataset based on specified table.

#### flagsCount
Create an overview of number of retained and discarded datapoints.

#### NMorderColumns - Order columns in dataset for use in Nonmem.

#### NMwriteData
Write documented dataset for use in Nonmem (and R).

### Read Nonmem results
(See [vignette](https://philipdelff.github.io/NMdata/articles/NMscanData.html))
#### NMscanData 
Automatically find Nonmem input and output tables and
organize data.

#### NMtransInput 
Read input data as seen by Nonmem (e.g. by naming
columns as specified in $INPUT and taking DROP statements and others
into account).

#### NMreadCsv
Shortcut to read input datasets created for Nonmem.

#### NMreadTab

### Handy data wrangling tools
#### findCovs 
Extract columns that do not vary within variables in a data.frame.

#### findVars 
Extract columns that vary within values of other columns in a data.frame.

#### mergeCheck
Merge, order, and check resulting rows and columns.
