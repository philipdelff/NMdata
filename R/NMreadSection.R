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
##' @param keep.comments Default is to keep comments. If FALSE, the
##'     will be removed.
##' @param keep.empty Keep empty lines in output? Default is
##'     FALSE. Notice, comments are removed before empty lines are
##'     handled if `keep.comments=TRUE`.
##' @param keep.name Keep the section name in output (say, "$PROBLEM")
##'     Default is FALSE. It can only be FALSE, if return="text".
##' @param as.one If multiple hits, concatenate into one. This will
##'     most often be relevant with name="TABLE". If FALSE, a list
##'     will be returned, each element representing a table. Default
##'     is TRUE. So if you want to process the tables separately, you
##'     probably want FALSE here.
##' @param simplify If asOne=FALSE, do you want the result to be
##'     simplified if only one section is found? Default is TRUE which
##'     is desirable for interactive analysis. For programming, you
##'     probably want FALSE.
##' @param clean.spaces If TRUE, leading and trailing are removed, and
##'     multiplied succeeding white spaces are reduced to single white
##'     spaces.
##' @param keepComments Deprecated. See keep.comments.
##' @param keepEmpty Deprecated. See keep.empty.
##' @param keepName Deprecated. See keep.name.
##' @param asOne Deprecated. See as.one.
##' @return character vector with extracted lines.
##' @param ... Additional arguments passed to NMextractText
##' @return character vector with extracted lines.
##' @family Nonmem
##' @examples
##' NMreadSection(system.file("examples/nonmem/xgxr001.lst", package="NMdata"),section="DATA")
##'
##' @export


NMreadSection <- function(file=NULL, lines=NULL, text=NULL, section, return="text",
                          keep.empty=FALSE,keep.name=TRUE, keep.comments=TRUE,as.one=TRUE,
                          clean.spaces=FALSE, simplify=TRUE,
                          ## deprecated arguments
                          keepEmpty, keepName,
                          keepComments, asOne,
                          ...){

    ## args <- getArgs()
    args <- getArgs(sys.call(),parent.frame())
    as.one <- deprecatedArg("asOne","as.one",args=args)
    ## clean.spaces <- deprecatedArg("cleanSpaces","clean.spaces",args=args)
    ## if(!missing(cleanSpaces)) rm(cleanSpaces)
    keep.empty <- deprecatedArg("keepEmpty","keep.empty",args=args)
    keep.name <- deprecatedArg("keepName","keep.name",args=args)
    keep.comments <- deprecatedArg("keepComments","keep.comments",args=args)
    
    if(missing(section)||is.null(section)){
        section="."
        as.one=FALSE
        simplify=FALSE
        keepName.arg <- keep.name
        keep.name=TRUE
        
    } else {
        section <- toupper(section)
    }
    
    match.exactly <- FALSE
    if(section!="."){
        string.start <- substring(sub("^\\$","",cleanSpaces(section)),1,3)
        match.exactly <- !string.start%in%c("COV","EST","SIM")
    }
    
    res <- NMextractText(file=file, lines=lines, text=text, section=section,
                         ## this wrapper is especially made for "$" sections
                         char.section="\\$",
                         return=return,
                         keep.empty=keep.empty,
                         keep.name=keep.name,
                         keep.comments=keep.comments,
                         as.one=as.one,
                         simplify=simplify,
                         clean.spaces=clean.spaces,
                         ## we only consider the model definition, not results.
                         type="mod",
                         ## match.exactly=FALSE,
                         match.exactly=match.exactly,
                         ## deprecated args
                         ## keepEmpty=keepEmpty,
                         ## keepName=keepName,
                         ## keepComments=keepComments,
                         ## asOne=asOne,
                         ## cleanSpaces=cleanSpaces,
                         ...)

    
    if(section=="."&&return=="text"){
        
        ## if(return=="text") res.text <- res
        ## if(return=="idx"){
        ##     all.lines <- readLines()
        ##     res.text <- lapply(res,function(x)all.lines[min(x):max(x)])
        ## }
        res.text <- res
        
        names(res) <-
            unlist(
                lapply(res.text,function(x) sub("\\$([^ ]+)","\\1",strsplit(x[1]," ")[[1]][1]))
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

##' @describeIn NMreadSection Deprecated function name. Use NMreadSection.
## Deprecated way before 2023-06-12
NMgetSection <- function(...){
    .Deprecated("NMreadSection")
    NMreadSection(...)
    
}

