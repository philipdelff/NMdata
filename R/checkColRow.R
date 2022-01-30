##' check that col.row is not edited in Nonmem control stream
##'
##' @description In order to safely merge by a unique row identifier,
##'     that row identifier must not be edited from input to
##'     output. checkColRow helps checking that based on the control
##'     stream.
##' @param col.row The name of the unique row identifier (say "ROW").
##' @param file a list file or input control stream file path.
##' @return TRUE if no issues found
##' @keywords internal

## not to be exported. This is a helper function for NMscanData.

checkColRow <- function(col.row,file){

    if(is.null(col.row)) {
        messageWrap("when use.input=TRUE and merge.by.row=TRUE, col.row cannot be NULL, NA, or empty.",fun.msg=stop)
    }
    
    ## col.row must not be on the left-hand side of an assign statement (a single =) in the input control stream.

    sections <- NMreadSection(file)

### get rid of PROBLEM, INPUT, DATA, SUBROUTINE, THETA, OMEGA, SIGMA, ESTIMATION, TABLE
    names.sections <- names(sections)
    sections <- sections[setdiff(names.sections,c("PROBLEM", "INPUT", "DATA", "SUBROUTINE", "THETA", "OMEGA", "SIGMA", "ESTIMATION", "TABLE"))]
    
    sections.one <- Reduce(c,sections)
    grep.assign <- grepl(paste0("^ *",col.row,"[ ]*[=]{1}[^=]+"),sections.one)
    if(any(grep.assign)){
        stop(paste("col.row seems to be modified. You can't use this for merging. From Nonmem control stream:",paste(sections.one[grep.assign],collapse=", ")))
    }
    invisible(TRUE)
}
