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


   ##          if(col.row%in%cnames.input) {
   ##              if(data.input$data[,any(duplicated(get(col.row)))]) {
   ##                  messageWrap("use.input=TRUE and merge.by.row=TRUE. Hence, input data and output data must be merged by a unique row identifier (col.row), but col.row has duplicate values in _input_ data. col.row must be a unique row identifier when use.input=TRUE and merge.by.row=TRUE.",fun.msg=stop)
   ##              }
   ##          }

   ## if( tab.row[,any(duplicated(get(col.row)))]) {
   ##                  messageWrap("use.input is TRUE, but col.row has duplicate values in _output_ data. col.row must be a unique row identifier. It is unique in input data, so how did rows get repeated in output data? Has input data been edited since the model was run?",fun.msg=stop)
   ##              }
