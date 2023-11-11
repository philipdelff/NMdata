# 0.1.3
* Better support for models with multiple estimation
  steps. Particularly reading output tables now better distinguishes
  between Nonmem table numbers and repetitions (like
  SUBPROBLEMS). Also, functions that read parameter estimates clearly
  separates Nonmem table numbers.

# 0.1.2
* Improved support for reading multiple models with NMreadExt and
NMreadPhi. 



# 0.1.2
## New features
* NMreadExt is a new function that reads parameter estimates,
  uncertainties if available, estimation iterations and other
  information from .ext files. 
* NMreadPhi is a new function that reads contents of Nonmem phi files.
* NMreadCov is a "new" function that reads Nonmem .cov files
  (parameter uncertainty as estimated by a covariance step). The
  function is not really new. It was developed by Matt Fidler for
  nonmem2rx based on NMdata's NMreadTab. It has been only slightly
  modified since. Thanks Matt!
* NMscanInput supports all combinations of `translate` and
  `recover.cols`.

## Other improvements
* NMreadCsv supports multiple prioritized formats in the `format`
  arguments.


# 0.1.1
## New features
* NMwriteSection can now handle functions to perform control stream
  editing. NMwriteSection provides methods to edit control
  streams. Until now by inserting, removing and replacing sections by
  user-provided text. This new feature allows the user to specify a
  function for editing the text, i.e. making it more suitable for
  doing changes to sections like $PK/$PRED or $THETA/$OMEGA/$SIGMA.
* NMcheckData has a new argument `type.data` which allows switching
  between estimation and simulation type data.

## Other improvements
* NMscanMultiple now by default looks for all .lst files if provided
  with a directory (`dir`) only.
* Minor bugfix in compareCols in case input is an unnamed list

# 0.1.0

## New features
* The super fast `fst` format is now supported. Data sets can be
  written to this format, and NMscanData() and related functions can
  read it. It can be used instead of `rds` which is the default
  full-featured data format used in the package.

* New function NMreplaceDataFile to replace the input data file in a
  control stream. A simple wrapper of NMwriteSection but useful for
  this specific purpose.

* New function editCharCols that allows for editing character columns
  in a dataset based on regular expressions or strings. This allows
  for instance for removal of special characters that are not allowed
  in the selected data format (like a comma can make trouble in a csv
  file).

* NMcheckData has a new argument, `cols.dup`, to include additional
  columns (to col.id, col.cmt, col.evid, and col.time) in search for
  duplicated events. This is useful for different assays run on the
  same compartment (say a DVID column) or maybe stacked datasets. If
  col.cmt is of length>1, this search is repeated for each cmt
  column. Thanks to Eric Anderson for suggesting and testing this.
  
* NMcheckData has improved checks of II and ADDL columns.

## Other improvements
* `NMwriteData` now uses the `formats` argument to specify the requested
  file formats. This replaces arguments like `write.csv` and
  `write.rds`. To get those two, do `formats=c("csv","rds")` (which is
  default). The argument `save` is used to control whether outputs are
  written altogether.
  
* Argument name format has been cleaned and aligned to follow the
  an.arg format rather than camel toe which was also used in some
  functions before. All deprecated arguments have been soft deprecated
  meaning they still work.

# 0.0.17

This release provides a few bugfixes, nothing major.

## New features
* NMscanTables (and hence NMscanData) now allows to skip missing
  output tables (files missing) and continue combining what is
  found. This is handled by the `skip.absent` argument.

## Bugfixes
* Filtering by the abbreviated IGN notation in Nonmem control
  statements would not always work when not using a row identifier for
  combining input and output data. This should now be fixed. However,
  it is still recommended to use a row identifier to merge input and
  output data.
  
* flagsCount now reports NA discards for total and analysis data. It
  used to report zero but these criteria are not applied at these
  steps.

## Other improvements
* NMcheckData has improved checks of some columns related to either
  observations (like MDV) or doses (like RATE). This will give less
  findings that Nonmem would not fail on anyway.

* addTAPD's col.ndoses argument has been renamed to col.doscumn and
  the default value is now "DOSCUMN". This makes it clear that it is a
  cumulative number and it aligns with col.doscuma which is the
  cumulative amount.

# 0.0.16
## New features
* NMwriteSection includes argument `location`. In combination with
  `section`, this determines where the new section is
  inserter. Possible values are "replace" (default), "before", "after",
  "first", "last".
  
* NMreadSection adds support for partial matching of section
  names. Specifically, this means that the first three characters will
  be matched only, i.e. allowing say "$SIMULATION" to match "$SIM" or
  "$ESTIMATION" to match "$EST".

## Bugfixes
NMcheckData did not check columns listed in cols.num for NA
elements. Now it does.

NMcheckData now only checks col.dv to be non-NA for col.mdv==0 if
col.mdv is present.

NMscanInput would fail if there was no column called ID in the dataset
on file. This has been fixed to support cases where renaming or a
pseudonym is being used to generate an ID column in $INPUT.


# 0.0.15
This update is of no difference to users. A technicality has been
chaned to ensure consistent test results once data.table 1.14.7 is

# 0.0.14
## New features
* fnExtension has been generalized. It now ignores leading spaces in
  new extension, and extensions with zero or one leading period are
  treated identically (so asking for xml or .xml is the same). Also,
  by providing "" as the new extension will now remove the extension,
  and if extension is not provided, fnExtension will retrieve the
  extension rather than replace it.
  
* NMscanData now supports repeated output tables, like those created
  using the SUBPROBLEM option.
  
* NMwriteData has a new argument csv.trunc.as.nm. If TRUE, csv file
  will be truncated horizontally (columns will be dropped) to match
  the $INPUT text generated for Nonmem (genText must be TRUE for this
  option to be allowed). This can be a great advantage when dealing
  with large datasets that can create problems in
  parallellization. Combined with write.rds=TRUE, the full data set
  will still be written to an rds file, so this can be used when
  combining output and input data when reading model results. This is
  done by default by NMscanData. This means writing a lean (narrow)
  csv file for Nonmem while keeping columns of non-numeric class like
  character and factor for post-processing.
  
* NMwriteData has got an arguement 'genText' to control whether text
  for Nonmem should be generated. Default is to do so. Also, support
  is added for script=NULL which now means the same as not specifying
  script.
  
* addTAPD now includes SDOS, a scalar to be applied when computing
  last dose amount and cumulative dose amount from AMT. Sometimes, AMT
  is in one unit, and other variables related to doses is in
  another. Say that dose is in mg and concentrations are in ng/mL,
  then AMT should be in mcg. But you may want everything else
  related to doses to be in mg. Then use SDOS=1000.

* addTAPD includes convenient prefix.cols and suffix.cols arguments
  that will prepend or append strings to all created columns. This is
  useful if dosing more than one drug, and you want to run addTAPD for
  both (different suffixes?), or if you want to run for nominal and
  actual time (prefix A and N?).

* flagsAssign now reports that data is empty and return the data
  if nothing is left after applying subset. It used to return an
  error.
  
* NMgenText has a new argument, width, passed to strwrap to control
  the width of the $INPUT text for Nonmem.

## Bugfixes
* NMapplyFilters (and then NMscanInput and NMscanData) gave an error
  when multiple filters were applied on the same column. Fixed.

* addTAPD was not respecting subset.dos for all generated columns. 

* NMisNumeric would interpret a NA of class character or logical as
  non-numeric. Fixed.

## Other improvements
* Internally, combination of input and output data without a row
  identifier is simplified.
  
* NMdata version added to welcome message.
# 0.0.13

## New functions
* NMexpandDoses - Transform repeated dosing events (ADDL/II) to
  individual dosing events
* addTAPD - Add cumulative number of doses, time of last dose,
  previous dose amount, cumulative dose amount, and time since
  previous dose to data
* tmpcol provides column names not already used in data sets. tmpcol
  has long been part of NMdata but has not been exported until now.

## New data
* A new data set called mad is included. It is based on the
  mad_missing_duplicates from the xgxr package. Doses are implemented
  using ADDL and II (so only one dosing row per subject). It is
  included for testing the new NMexpandDoses and addTAPD functions.

## Bugfixes
* Non-critical bugfix in mergeCheck dimensions overview printed to
  console. One column too many was reported in input and result
  data. No implications on results from mergeCheck.

# NMdata 0.0.12

* Vignettes are no longer included in R package and can only be read
  online at https://philipdelff.github.io/NMdata/ They are still being
  maintained, and the exclusion from the package releases is only due
  to CRAN's restrictive requirements to the package size.

* New function, NMscanMultiple, to read multiple models and stack
  results in one data set. This is very useful for meta
  analysis. NMscanMultiple is a wrapper of NMscanData. It keeps track
  of warnings and errors for reading of individual models rather than
  getting stuck. You can either specify a vector of model paths or a
  directory plus a regular expression (just like for NMwriteSection).

* Improved tests of order of age of output control streams and input
  data in NMscanData. So far, all data were tested on file
  modification times which is not useful in the common case that data
  files and/or nonmem control streams are moved around between
  systems. Now NMscanData can look for a time stamp in the output
  control stream and if NMwriteData was use to write the input data to
  file, the creation time is taken from meta data. This will make the
  warnings about the order of age of files more reliable. Notice
  however, that for the output control stream, the timezone has to be
  set using the tz.lst argument or using NMdataConf - at least for
  now.
  
* Checks of unique subject identifier (usubjid) included in
  NMcheckData. This is mostly to detect the potential issue that the
  subject IDs generated for analysis are not unique across actual
  subjects. If a usubjid (e.g. from clinical data sets) is included in
  data, NMcheckData can check this for basic properties and check the
  analysis subject ID and the usubjid against each other.

* New function: cl - creates factors, ordered by the appearance of the
  elements when created. cl("b","a") results in a factor with levels
  "b" and "a". This can save quite some typing in data set
  preparation.

* New function: fnAppend - append a string to a file name before the
  file name extension. fnAppend("data.csv","subset") results in
  "data_subset.csv".

* General support for a file.data argument when a specific input data
  file is to be used instead of finding this information in the
  control streams. This is very useful if you archive input data
  together with a nonmem run in a way that the path in the control
  stream has to be overruled. Like many other of this type of
  arguments in NMdata, it can be a function that systematically
  converts the path to the control stream to the input data
  archive. Running Nonmem this way breaks the link between an input
  dataset that may change over time and the model runs that become
  self-contained packages of input and output data.

* Support for `NOHEADER` option in Nonmem `$TABLE` blocks. If `NMdata` is
  used to read the results, there is no need to use NOHEADER (which
  opens the door to mistakes if manually renaming the columns in
  results), but NMdata should now also be able to handle this. 

* If found in data, `CMT` is added to the breakdown of rows when
  summarizing results from NMscanData. Before, it was broken down on
  EVID only. Also, a total line is included with total number of rows
  in each of input-only, output, and result.

* Support for non-event (say for `$PRED`) datasets in `NMcheckData`.

* Support for custom column names for `DV` (`col.dv`) and MDV (col.mdv),
  ID (col.ID), AMT (col.amt) in NMcheckData.

* Support for file.mod and dir.data arguments in NMcheckData when
  running on a control stream.

* `NMgenText` now has an argument called until that specifies the last
  column(s) to include in $INPUT.

* `compareCols` takes the list.data argument same way as dims()
  does. This is often easier to use in programming.

* If `NMgenText` does not find any Nonmem-compatible columns to report,
  it now throws a warning and returns NULL.

## Bugfixes
* NMextractDataFile was not cleaning all paths correctly. This mostly
  impacts `NMcheckData`'s ability to find data files when using the file
  argument in `NMcheckData`. Only affects certain models.
* NMextractDataFile was not working with dir.data. Fixed.
* NMextractDataFile Now handles absolute paths correctly.

## Minor changes 
* compareCols now by default lists the columns where no differences
  were found. 

* `NMreadTab` throws a message instead of a warning in case duplicate
  column names are found and removed.
	  
* `NMwriteData` now runs `NMgenText` in try, just in case.

* `fnExtension` now supports adding extensions to strings without
  extensions, i.e. `fnExtension("file",".txt")`.

# NMdata 0.0.11

* The cols.num argument in NMcheckData has been improved to support a
  list specifying columns that are numeric within subsets of
  data. This is often useful for columns that are only meaningful for
  say samples (eg LLOQ) or doses (eg injection site).
* When run on a control stream, NMcheckData now checks all columns
  specified in the control stream to be numeric.
* The NMcheckData argument, col.cmt now supports a vector of
  length>1. This is helpful to support data where switching CMT column
  is used to switch between definitions for different compounds or
  even just models where compartment numbers are not compatible.
* NMscanInput has the new argument recover.cols. Default is TRUE - use
  FALSE to not include columns that NONMEM did not read.
* mergeCheck now retains column order from x. It has a few other
  improvements too, like checking for missing values in by columns.
* listMissings is new function that looks for missing values or
  strings that represent missing values (like "" or ".") in multiple
  columns. This can be useful when combining source data
  sets. However, the function is still underdevelopment and what
  exactly is being reported may change in future releases.

## Bugfixes
* NMwriteData now respects NMdataConf()$args.fwrite
* flagsCount was not respecting by columns. Fixed.
* egdt was reporting one column too many
  in inputs (in terminal, not in results). Fixed.
* Column names in control stream $INPUT statements weren't adjusted
  for possible tabulator characters. Fixed.

# NMdata 0.0.10
## New functions
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

* NMextractDataFile is a function that identifies the input datafile
  used by a Nonmem model. It reports the string as in the Nonmem
  control stream, file path and whether the file exists. It also looks
  for the corresponding rds files. The function is not new in NMdata
  but was not exported until 0.0.10.

* cc is a function that creates character vectors from arguments
  without quotes. This is just to be able to skip typing quotes when
  listing say column names. So do cc(a,b,c) to get the exact same as
  c("a","b","c"). You cannot do this with strings that contain special
  characters. In that case do cc(a,"b+c") to get the same as
  c("a","b+c").

## Function improvements
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
  to observations, doses etc. separately. You can easily specify a
  subset of data to run flagsAssign on, and it will by default check
  for whether values of EVID are unique. This is similar to what
  flagsCount does.

* NMgenText is a new function that provides the generation of $INPUT
  and $DATA. This used to be part of NMwriteData. NMwriteData still
  calls NMgenText but the separation of the two functionalities allows
  for more intuitive separate uses of one dataset for different
  models. 
  
* NMcompareCols now takes the argument "cols.wanted" which is a
  character vector of column names of special interest. Helpful when
  building a data set with specific column names in mind.

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

* Translation table includes a column ranking the match between input
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
(doubled) with both pseudonyms as column names.

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

The tools to assign and count exclusion flags, flagsAssign and
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
data.tables. It mimics the behavior of merge.data.frame on objects
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
character flags. The default can be configured using NMdataConf.

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
function for the summary too. This is printed automatically by NMdata
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
considered in an analysis have been run on the same system, so it makes
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
change is that it makes it easier for users to post-process only one
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

