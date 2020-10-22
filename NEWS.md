# NMdata 0.0.6.2
A summary function is provided for NMdata objects. There is a print
function for the summary too. This is printet automatically by NMdata
unless quiet=TRUE.

A lot of meta information has been added in an attribute to NMdata
objects. This will help the user to understand and automatically
document what has been read.

# NMdata 0.0.6.1
The default class to generate is now data.frame rather than
data.table. If you want to work with data.tables, do 
options(NMdata.as.fun="none")

# NMdata 0.0.6
General support for conversion of output to user-specified
class. Setting the option "NMdata.as.fun" to a conversion function
such as as.data.frame or tibble::as_tibble it is possible for the user
to work with their preferred data class. An argument, as.fun, can be
used for the individual functions too.

The translation from the output control stream file path (.lst in PSN)
and the input control stream (.mod in PSN) can now be configured
through the option "NM.file.mod". Typically, all the models to be
considered in an anlysis have been run on the same system, so it makes
most sense to define this behavior once and for all for most users.

NMwriteData improved with checks of column names and automated
generation of $INPUT and $DATA Nonmem sections.

NMorderColumns simplified, and documentation improved.

Documentation has been upgraded with a pkgdown site.

New vignette on data set creation tools in NMdata.

New FAQ vignette.

## Bugfixes
NMtransInput now supports the case where additional unused column
names are given in $INPUT than actually found in $DATA. A warning will
be given.

# NMdata 0.0.5
This release introduces a consistent default NMscanData behavior that
will work in most cases and provide the user with information on how
to use a more robust approach to merging input and output data.

Naming of a few arguments to NMscanData has been changed from
camelCase to lower.case for consistency. 

NMscanTables keeps track of LASTONLY and FIRSTLASTONLY. LASTONLY are
now treated like FIRSTONLY while FIRSTLASTONLY tables are disregarded
(with a warning).

# NMdata 0.0.4
The most obvious change since 0.0.3 is that only one data.table is
being returned from NMscanData. This is what used to be the `row`
element in the returned objects previously. The main reason for this
change is that it makes it easier for users to postprocess only one
dataset before splitting into different levels of variability. The
small cost is that the user will have to run findCovs, or findVars to
get the desired level of variability. These functions are however very
simple to use and very fast to run.

This release features numerous improvements to especially the
NMscanData function. The work is mainly focused around use without a
row identifier. Even without a row identifier, NMscanData should now
work for the vast majority of models, including merging with input and
recovering rows. 

An attribute called `vars` has been added to the NMdata objects coming out of
NMscanData. It features a table of the columns in the returned object
and information about where they originate from. More work is still to
be done on this, but hopefully it is useful already.

NMwriteData: Added support for passing arguments to saveRDS.

Last but far from least is a new vignette on using NMscanData. Check
it out with vignette("NMscanData").

# NMdata 0.0.2
This release contains bugfixes and experimental support for merging nonmem input and output data without a row identifier. 

A clearer cut has been made between the pmxtricks package (version 0.0.10) and NMdata. The packages should not overlap in exported functionality, and they do not depend on each other.

