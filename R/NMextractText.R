##' Versatile text extractor from Nonmem (input or output) control streams
##'
##' If you want to extract input sections like $PROBLEM, $DATA etc,
##' see NMreadSection. This function is more general and can be used to
##' extract eg result sections.
##'
##' @param file A file path to read from. Normally a .mod or .lst. See
##'     lines and text as well.
##' @param lines Text lines to process. This is an alternative to
##'     using the file and text arguments.
##' @param text Use this argument if the text to process is one long
##'     character string, and indicate the line separator with the
##'     linesep argument. Use only one of file, lines, and text.
##' @param section The name of section to extract. Examples: "INPUT",
##'     "PK", "TABLE", etc. It can also be result sections like
##'     "MINIMIZATION".
##' @param char.section The section denoted as a string compatible
##'     with regular expressions. "\\$" for sections in .mod files,
##'     "0" for results in .lst files.
##' @param char.end A regular expression to capture the end of the
##'     section. The default is to look for the next occurrence of
##'     char.section.
##' @param return If "text", plain text lines are returned. If "idx",
##'     matching line numbers are returned. "text" is default.
##' @param keepEmpty Keep empty lines in output? Default is FALSE.
##' @param keepName Keep the section name in output (say, "$PROBLEM")
##'     Default is TRUE. It can only be FALSE, if return"idx".
##' @param keepComments Keep comment lines?
##' @param asOne If multiple hits, concatenate into one. This will
##'     most often be relevant with name="TABLE". If FALSE, a list
##'     will be returned, each element representing a table. Default
##'     is TRUE. So if you want to process the tables separately, you
##'     probably want FALSE here.
##' @param simplify If asOne=FALSE, do you want the result to be
##'     simplified if only one table is found? Default is TRUE which
##'     is desirable for interactive analysis. For programming, you
##'     probably want FALSE.
##' @param cleanSpaces If TRUE, leading and trailing are removed, and
##'     multiplied succeeding white spaces are reduced to single white
##'     spaces.
##' @param type Either mod, res or NULL. mod is for information that
##'     is given in .mod (.lst file can be used but results section is
##'     disregarded). If NULL, NA or empty string, everything is
##'     considered.
##' @param linesep If using the text argument, use linesep to indicate
##'     how lines should be separated.
##' @return character vector with extracted lines.
##' @details This function is planned to get a more general name and
##'     then be called by NMreadSection.
##' @family Nonmem
##' @examples
##' NMreadSection(system.file("examples/nonmem/xgxr001.lst", package = "NMdata"),section="DATA")
##' @export


NMextractText <- function(file, lines, text, section, char.section,
                          char.end=char.section, return="text",
                          keepEmpty=FALSE, keepName=TRUE,
                          keepComments=TRUE, asOne=TRUE,
                          simplify=TRUE, cleanSpaces=FALSE,
                          type="mod", linesep="\n"){

    

    if(sum(c(!missing(file)&&!is.null(file),
             !missing(lines)&&!is.null(lines),
             !missing(text)&&!is.null(text)
             ))!=1) stop("Exactly one of file, lines, or text must be supplied")
    if(!missing(file)&&!is.null(file)) {
        if(!file.exists(file)) stop("When using the file argument, file has to point to an existing file.")
        lines <- readLines(file,warn=FALSE)
    }
    if(!missing(text)&&!is.null(text)) {
        lines <- strsplit(text,split=linesep)[[1]]
    }

    
    
    if(!return%in%c("idx","text")) stop("text must be one of text or idx.")
    
    ## works with both .mod and .lst
    if(length(type)>1) stop("type must be a single-element character.")
    if(is.null(type)||is.na(type)||grepl("^ *$",type)){
        type <- "all"
    }

    ## This line can give problems because of possible special characters in company names or the registerred trademark character. We are not using it anyway.
    lines <- lines[!grepl("^ *License Registered to:",lines,useBytes=TRUE)]
    lines <- switch(type,
                    mod={
                        lines[1:(min(c(length(lines),grep("NM-TRAN MESSAGES|WARNINGS AND ERRORS \\(IF ANY\\) FOR PROBLEM",lines)-1)))]
                    },
                    res={
                        idx.res.start <- min(grep("NM-TRAN MESSAGES|WARNINGS AND ERRORS \\(IF ANY\\) FOR PROBLEM",lines))
                        if(length(idx.res.start)==0) stop("type=res, but there are no results in the file/text to be analyzed.")
                        lines[idx.res.start:length(lines)]
                    },
                    all={lines}
                    )
    
    ## Find all the lines that start with the $section
    idx.starts <- grep(paste0("^ *",char.section,section),lines)
    idx.ends <- grep(paste0("^ *",char.end),lines)

    ## get the sections
    idx.sections <- lapply(idx.starts,function(idx.st){
        idx.dollars.after <- idx.ends[idx.ends>idx.st]
        if(length(idx.dollars.after)==0) {
            idx.end <- length(lines)
        } else {
            idx.end <- min(idx.dollars.after)-1
        }
        idx.section <- idx.st:idx.end
    })
    result <- idx.sections

    if(!keepEmpty){
        result <- lapply(result,function(x)
            x[!grepl("^ *$",lines[x])]
            )
    }
    
    if(!keepComments){
        result <- lapply(result,function(x)
            x[!grepl("^ *;",lines[x])]
            )
        lines <- sub(pattern=";.*$",replacement="",x=lines)
    }
    
    if(return=="text"){
        result <- lapply(result,function(x)lines[x])
    }
    
    if(!keepName){
        if(!return=="text") {
            stop("keepName can only be FALSE if return=='text'")
        }
        result <- lapply(result, function(x)sub(paste0("^ *\\$",section),"",x))
    }

    if(cleanSpaces){
        if(!return=="text") {
            stop("cleanSpaces can only be TRUE if return=='text'")
        }
        result <- lapply(result, function(x)sub(paste0("^ +"),"",x))
        result <- lapply(result, function(x)sub(paste0(" +$"),"",x))
        result <- lapply(result, function(x)sub(paste0(" +")," ",x))
    }
    
    if(asOne) {result <- do.call(c,result)}

    if(simplify && length(result)==1) result <- result[[1]]
    

    return (result)
    
}
