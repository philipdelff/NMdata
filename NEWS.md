# NEWS
## 0.0.5
NMscanTables keeps track of LASTONLY and FIRSTLASTONLY. LASTONLY are
now treated like FIRSTONLY while FIRSTLASTONLY tables are disregarded
(with a warning).

## 0.0.4
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

## 0.0.2
This release contains bugfixes and experimental support for merging nonmem input and output data without a row identifier. 

A clearer cut has been made between the pmxtricks package (version 0.0.10) and NMdata. The packages should not overlap in exported functionality, and they do not depend on each other.

