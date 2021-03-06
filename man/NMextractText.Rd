% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NMextractText.R
\name{NMextractText}
\alias{NMextractText}
\title{Versatile text extractor from Nonmem (input or output) control streams}
\usage{
NMextractText(
  file,
  lines,
  text,
  section,
  char.section,
  char.end = char.section,
  return = "text",
  keepEmpty = FALSE,
  keepName = TRUE,
  keepComments = TRUE,
  asOne = TRUE,
  simplify = TRUE,
  cleanSpaces = FALSE,
  type = "mod",
  linesep = "\\n"
)
}
\arguments{
\item{file}{A file path to read from. Normally a .mod or .lst. See
lines and text as well.}

\item{lines}{Text lines to process. This is an alternative to
using the file and text arguments.}

\item{text}{Use this argument if the text to process is one long
character string, and indicate the line seperator with the
linesep argument. Use only one of file, lines, and text.}

\item{section}{The name of section to extract. Examples: "INPUT",
"PK", "TABLE", etc. It can also be result sections like
"MINIMIZATION".}

\item{char.section}{The section denoter as a string compatible
with regular expressions. "\\$" for sections in .mod files,
"0" for results in .lst files.}

\item{char.end}{A regular expression to capture the end of the
section. The default is to look for the next occurrence of
char.section.}

\item{return}{If "text", plain text lines are returned. If "idx",
matching line numbers are returned. "text" is default.}

\item{keepEmpty}{Keep empty lines in output? Default is FALSE.}

\item{keepName}{Keep the section name in output (say, "$PROBLEM")
Default is TRUE. It can only be FALSE, if return"idx".}

\item{keepComments}{Keep comment lines?}

\item{asOne}{If multiple hits, concatenate into one. This will
most often be relevant with name="TABLE". If FALSE, a list
will be returned, each element representing a table. Default
is TRUE. So if you want to process the tables separately, you
probably want FALSE here.}

\item{simplify}{If asOne=FALSE, do you want the result to be
simplified if only one table is found? Default is TRUE which
is desirable for interactive analysis. For programming, you
probably want FALSE.}

\item{cleanSpaces}{If TRUE, leading and trailing are removed, and
multiplied succeeding white spaces are reduced to single white
spaces.}

\item{type}{Either mod, res or NULL. mod is for information that
is given in .mod (.lst file can be used but results section is
disregarded). If NULL, NA or empty string, everything is
considered.}

\item{linesep}{If using the text argument, use linesep to indicate
how lines should be separated.}
}
\value{
character vector with extracted lines.
}
\description{
If you want to extract input sections like $PROBLEM, $DATA etc,
see NMreadSection. This function is more general and can be used to
extract eg result sections.
}
\details{
This function is planned to get a more general name and
    then be called by NMreadSection.
}
\examples{
NMreadSection(system.file("examples/nonmem/xgxr001.lst", package = "NMdata"),section="DATA")
}
\seealso{
Other Nonmem: 
\code{\link{NMapplyFilters}()},
\code{\link{NMreadSection}()},
\code{\link{NMwriteData}()},
\code{\link{NMwriteSection}()}
}
\concept{Nonmem}
