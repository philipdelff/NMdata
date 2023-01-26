##' Extract sections of Nonmem control streams
##'
##' This is a very commonly used wrapper for the input part of the
##' model file. Look NMextractText for more general functionality
##' suitable for the results part too.
##'
##' @param file A file path to read from. Normally a .mod or .lst. See
##'     lines also.
##' @param lines Text lines to process. This is an alternative to
##'     using the file argument.
##' @param text Use this argument if the text to process is one long
##'     character string, and indicate the line separator with the
##'     linesep argument (handled by NMextractText). Use only one of
##'     file, lines, and text.
##' @param section The name of section to extract without
##'     "$". Examples: "INPUT", "PK", "TABLE", etc. Not case
##'     sensitive.
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
##'     simplified if only one section is found? Default is TRUE which
##'     is desirable for interactive analysis. For programming, you
##'     probably want FALSE.
##' @param cleanSpaces If TRUE, leading and trailing are removed, and
##'     multiplied succeeding white spaces are reduced to single white
##'     spaces.
##' @param ... Additional arguments passed to NMextractText
##' @return character vector with extracted lines.
##' @family Nonmem
##' @examples
##' NMreadSection(system.file("examples/nonmem/xgxr001.lst", package="NMdata"),section="DATA")
##'
##' @export


NMreadSection <- function(file=NULL, lines=NULL, text=NULL, section, return="text",
                          keepEmpty=FALSE, keepName=TRUE,
                          keepComments=TRUE, asOne=TRUE,
                          simplify=TRUE, cleanSpaces=FALSE, ...){

    if(missing(section)||is.null(section)){
        section="."
        asOne=FALSE
        simplify=FALSE
        keepName.arg <- keepName
        keepName=TRUE
        
    } else {
        section <- toupper(section)
    }
    
    res <- NMextractText(file=file, lines=lines, text=text, section=section,
                         ## this wrapper is especially made for "$" sections
                         char.section="\\$",
                         return=return,
                         keepEmpty=keepEmpty,
                         keepName=keepName,
                         keepComments=keepComments,
                         asOne=asOne,
                         simplify=simplify,
                         cleanSpaces=cleanSpaces,
                         ## we only consider the model definition, not results.
                         type="mod",
                         match.exactly=FALSE,
                         ...)

    
    if(section=="."){
        names(res) <-
            unlist(
                lapply(res,function(x) sub("\\$([^ ]+)","\\1",strsplit(x[1]," ")[[1]][1]))
            )
        
        res2 <- lapply(unique(names(res)),function(x)do.call(c,res[names(res)==x]))
        names(res2) <- unique(names(res))
        res2 <- lapply(res2,function(x){names(x) <- NULL
            x}
            )
        if(keepName.arg==FALSE){
            
            names.res2 <- names(res2)
            res2 <- lapply(1:length(res2),function(x)sub(paste0(" *\\$",names.res2[x]," *"),"",res2[[x]]))
            names(res2) <- names.res2
        }

        res <- res2
    }
    
    res
    
}

#' @describeIn NMreadSection Deprecated function name. Use NMreadSection.
NMgetSection <- function(...){
    .Deprecated("NMreadSection")
    NMreadSection(...)
    
}

