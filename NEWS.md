# NMdata 0.0.10
* NMcheckData is a new function that checks data for Nonmem
  compatibility in numerous ways. It returns a list of all findings
  making it easy to identify the location of each issue in the
  data. See the man page of NMcheckData for a complete list of the
  checks that are done. The function does not modify data in any way,
  and it is a very simple and easy step to avoid many problems in
  Nonmem. NMcheckData can check a data object in R, and it can also
  check how a control stream reads data and then do all the
  checks. The latter provides an extensive check of potential issues
  related to data and getting it into NONMEM. Great for both debugging
  and QC.

* NMwriteSection has been updated with a few very useful
  features. Namely these are related to updating multiple nonmem files
  at once. The user can now supply multiple paths, regular expressions
  for finding files (like pattern in list.files) and even input data
  files for nonmem models to match. This is very useful when updating
  many models after modifying input data. You can specify say all
  models in a directory names like pd*.mod where * is anything (in
  regular expressions this would be "pd.+\\.mod") and only the using
  the data file that was just written. Since NMwriteData generates the
  $INPUT for you, you just need to add one line to get the update of
  all your models automatically.

* flagsAssign has got a few updates related to separate handling of
  different types of events. Often, this will be used to assign flags
  to observations, doses etc. separately. You can easily speficy a
  subset of data to run flagsAssign on, and it will by default check
  for whether values of EVID are unique. This is similar to what
  flagsCount does.

* NMgenText is a new function that provides the generation of $INPUT
  and $DATA. This used to be part of NMwriteData. NMwriteData still
  calls NMgenText but the separation of the two functionalities allows
  for more inituitive separate uses of one dataset for different
  models. 
  
* NMcompareCols now takes the argument "cols.wanted" which is a
  character vector of column names of special interest. Helpful when
  building a data set with specific column names in mind.

* NMextractDataFile is a function that identifies the input datafile
  used by a Nonmem model. It reports the string as in the Nonmem
  control stream, file path and whether the file exists. It also looks
  for the corresponding rds files. The function is not new in NMdata
  but was not exported until 0.0.10.

* egdt now reports dimensions of the two data sets to combine and the
  resulting data. Can be disabled with quiet argument.

* NMcheckColumns Change of column name from DATA to INPUT in order to
  match $INPUT in the control streams.

* NMreadSection is now case insensitive in the section specification
  (i.e. "input" is the same as "INPUT").

## Bugfixes 
* In NMwriteData the datafile is now correctly included in the $DATA
  suggestion for Nonmem. No impact on data file output.

* Bugfix in NMscanData related to searching for candidates for unique
  row identifiers.

* In compareCols multiple classes of single columns would give a
  warning and sometimes confusing overview of columns. Fixed.
  
* findCovs fixed in ordering output when by argument is of length > 1

# NMdata 0.0.9
The only change from 0.0.8 is a patch provided by Matt Dowle ensuring
that tests pass after the release of data.table v1.14.2.

# NMdata 0.0.8
* Meta data system rewritten. NMinfo and NMstamp are used to read and
  write meta data. Meta data is stored as an attribute to the data
  object (attributes(data)$NMdata).

* Translation table includes a column ranking the match betwen input
  data file contents and $INPUT. OK: names match, diff: names do not
  match, off: diff and name is found elsewhere.

* NMscanInput, NMaplyFilters, NMtransInp all return meta data
  compatibly with NMinfo.

* merge.by.row="ifAvailable"

* Check for new values of row identifier

* Check for disjoint ID's when ID-level output tables found

* Improved message from NMscanData

* Support for custom (and NULL) values of col.model and col.nmout

* Support for Nonmem filters without operators (COL XX)

# NMdata 0.0.7.2
NMreadCsv, NMscanInput, and NMscanData take argument args.fread. The
contents of this list are passed as arguments to fread when reading
csv files. This should only be needed in rare cases but offers full
flexibility to match structure of csv files. Default contents of
args.fread can be controlled using NMdataConf.

NMwriteData updated with more concise message.

NMreadSection now returns all sections if argument section is missing
or equals NULL or ".".

NMinfo is a new function that provides meta data, processed by as.fun.

If merge.by.row=TRUE, NMscanData now checks if col.row seems to be
changed in the Nonmem code. If that is the case, an error is returned.

save argument added to flagsCount function to align with other
functions.

NMwriteData now writes meta data to a txt file when writing csv
file. NMreadCsv looks for this info and attaches it if found.

NMwriteData takes the argument args.fwrite - a list of arguments
passed to fwrite. This is aligned with args.fread used by
NMreadCsv. Defaults can be configured using NMdataConf.

Improved and shortened text to console from NMscanData
(print.summary_NMdata).

mergeCheck will now throw an explained error if argument df1 has zero
rows.

## Bugfixes
- In the special case where only one data set is given, compareCols
  used to sort the list of columns in an irrelevant way. Now no
  reordering is done but the list will match the column order in the
  data set.
- In the NMdata summary, the number of columns from input data are now
  calculated correctly.

# NMdata 0.0.7.1
compareCols generalized to the single data set case. 

mergeCheck has improved warnings when checks fail. This should in most
cases provide information for the user to get a good idea what needs
to be resolved for the merge to work as expected.

Support for pseudonyms when translating input data column names based
on nonmem control stream. Now by default, the column will be returned
(doubled) with both peudonyms as column names.

new function - fnExtension is a simple function to replace the
extension of a file name (say from file.mod to file.lst)

new function - dims is a simple function that returns a table of the
dimensions of multiple data sets. It is used by multiple other
functions in the package and may be useful on its own. However,
compareCols reports this information too (by calling dims).

NMscanData now has an argument, translate.input, which can be used to
skip the translation of column names according to $DATA in the listing
file. This can be necessary if input data has changed and hence $DATA
is outdated since last model run.

flagsCount now reports cumulative counts of discards too.

## Bugfixes
- Correct ordering of rows in compareCols
- flagsCount now reports numbers of discarded subjects as intended.

# NMdata 0.0.7
This is a major upgrade from 0.0.6.6 featuring many improvements and
bug fixes. Everyone is strongly encouraged to upgrade.

The choice between data combination methods in NMscanData is now done
in one argument, called merge.by.row. Before the method was decided
based on values of two arguments, cbind.by.filters and col.row. A
default value for col.row can now be set using NMdataConf and will not
affect the data combination method.

Other arguments which default values can now be modified using
NMdataConf are: merge.by.row, col.flagn, col.flagc, use.input,
recover.rows, col.model, modelname, file.mod, and check.time, quiet,
use.rds.

The tools to assign and cound exclusion flags, flagsAssign and
flagsCount, have been improved. They now support working on a subset
of data (say samples only), and the order (increasing/decreasing) of
the exclusion flags is optional. The printing of the count of
exclusion flags has been improved.

NMgetSection and NMwriteSection are new functions that can be used to
extract sections from and write sections to Nonmem control
streams. NMwriteData now returns a list of sections that can be passed
directly to NMwriteSection, in order to update control streams to read
the updated data file correctly.

compareCols is a very useful new data creation tool. See the
difference between presence and classes of columns in data sets. This
is useful before rbind'ing or merging - or maybe when those throw an
error, and you want to figure out why.

renameByContents is a function that can rename columns which contents
match a given criterion. In combination with the provided NMisNumeric,
this can be used to rename (say to lowercase) columns that Nonmem
cannot interpret (as numeric).

mergeCheck now informs about common column names that are not used to
merge by. These will create new column names, and it's often not
intended. An argument has been added (ncols.expected) to check the
number of columns added to df1 against expectation.

egdt is a new function for expanding grids of data.tables. This is
quite technical, and it fills a whole when constructing data with
data.tables. It mimicks the behavior of merge.data.frame on objects
with no common columns.

## Bugfixes related to 
- Class of return from NMorderData.
- NA values in NMisNumeric. This bug would spill over to NMOrderColumns.
- Processing rds files including non-data.table objects.
- Single-character data filters in Nonmem (say IGNORE=C)

# NMdata 0.0.6.6
Central configuration mechanism implemented. The configuration
function to use is NMdataConf. You can configure several options,
corresponding to default values of arguments to different functions in
the package. This is very useful if you want to change the directory
and file naming structure, or if you want to change default column
names.

The exclusion flag functions flagsAssign and flagsCount have been
generalized to use customizable column names for the numerical and
caracter flags. The default can be configured using NMdataConf.

# NMdata 0.0.6.4
If all common column names two data objects to merge are not used for
merging (by), new column names are created by merge. The behavior of
mergeCheck can now be controlled in case this happens. This is
especially useful when using mergeCheck in programming.

A shortcut to system.file(...,package="NMdata") has been removed. The
function was called NMdata_filepath and is no longer available. Use
system.file instead.

# NMdata 0.0.6.3
Meta information added about the input data.

# NMdata 0.0.6.2
A summary function is provided for NMdata objects. There is a print
function for the summary too. This is printet automatically by NMdata
unless quiet=TRUE.

A lot of meta information has been added in an attribute to NMdata
objects. This will help the user to understand and automatically
document what has been read.

The argument to NMscanData previously called name has been renamed to
modelname and generalized to take a function that derives the name
from the file path. Also an general option "NMdata.modelname" has been
added, so the default behavior can be configured.

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

An attribute called `vars` has been added to the NMdata objects coming
out of NMscanData. It features a table of the columns in the returned
object and information about where they originate from. More work is
still to be done on this, but hopefully it is useful already.

NMwriteData: Added support for passing arguments to saveRDS.

Last but far from least is a new vignette on using NMscanData. Check
it out with vignette("NMscanData").

# NMdata 0.0.2
This release contains bugfixes and experimental support for merging
nonmem input and output data without a row identifier.

A clearer cut has been made between the pmxtricks package (version
0.0.10) and NMdata. The packages should not overlap in exported
functionality, and they do not depend on each other.

