##' replace ($)sections of a nonmem control stream
##'
##' Just give the section name, the new lines and the file path, and the
##' "$section", and the input to Nonmem will be updated.
##'
##' @param file File path to the model (control stream) to edit.
##' @param section The name of the section to update. Example:
##'     section="EST" to edit the sections starting by $EST. See
##'     NMreadSection
##' @param newlines The new text (including "$SECTION"). Better be
##'     broken into lines in a character vector since this is simply
##'     past to writeLines.
##' @param list.sections Named list of new sections, each element
##'     containing a section. Names must be section names, contents of
##'     each element are the new section lines for each section.
##' @param newfile path to new run. If missing, path is used. If NULL,
##'     output is returned rather than written.
##' @param backup In case you are overwriting the old file, do you
##'     want to backup the file (to say, backup_run001.mod)?
##' @param blank.append Append a blank line to output?
##' @param write Default is to write to file. If write=FALSE,
##'     NMwriteSection returns the resulting input.txt without writing
##'     it.  to disk?  Default is FALSE.
##'
##' @details The new file will be written with unix-style line
##'     endings.
##' @return The new section text is returned. If write=TRUE, this is done invisibly.
##' @family Nonmem
##' 
##' @examples
##' newlines <- "$EST POSTHOC INTERACTION METHOD=1 NOABORT PRINT=5 MAXEVAL=9999 SIG=3"
##' NMwriteSection(file=system.file("examples/nonmem/xgxr001.mod", package = "NMdata"),
##' section="EST", newlines=newlines,newfile=NULL)
##' @export


NMwriteSection <- function(file,section,newlines,list.sections,newfile,
                          backup=TRUE,blank.append=TRUE,write=TRUE){

    
    
#### Section start: handle arguments ####
    
    file <- filePathSimple(file)
    stopifnot(file.exists(file))

    if(missing(newfile)) newfile <- file
    if(!is.null(newfile)){
        newfile <- filePathSimple(newfile)
    }

    ## supply either section and newlines or a list
    if(!( (!missing(section)&&!missing(newlines)) ||
          !missing(list.sections)
    )
    ){
        stop("Either both section and newlines or list.sections must be supplied.")
    }
    if(missing(list.sections)){
        ## this must be list, not as.list. as.list would translate multiple lines into multiple sections.
        list.sections=list(newlines)
        names(list.sections) <- section
    }
    
###  Section end: handle arguments
    

    ## see below why we need to read the lines for now
    lines <- readLines(file)

    ## put this part in a function to be sequentially applied for all elements in list.
    replaceOnePart <- function(lines,section,newlines){
        
        ## make sure section is capital and does not start with $.
        section <- gsub(" ","",section)
        ## if(!grepl("^\\$",section)){
        ##     section <- paste0("$",section)
        ## }
        section <- sub("^\\$","",section)
        section <- toupper(section)

        ## make sure newlines start with $SECTION
        newlines <- sub("^ +","",newlines)
### doesn't work. For now, user has to supply newlines including a valid $SECTION start.
        ## if(grepl(paste0("^\\",section),newlines,ignore.case=TRUE)){
        ##     newlines <-
        ##         sub("^([^ ]+)(\\s.*)","\\1",newlines,perl=TRUE)
        ## }
        
        if(blank.append) newlines <- c(newlines,"")
        
        idx.dlines <- NMreadSection(lines=lines,section=section,return="idx",keepEmpty=TRUE,
                                   keepName=TRUE,keepComments=TRUE,asOne=TRUE,
                                   cleanSpaces=FALSE)

        stopifnot(length(idx.dlines)>0)
        
        if(length(idx.dlines)>1) {
            ## if th
            stopifnot(max(diff(idx.dlines))==1)
        }
        min.dl <- min(idx.dlines)
        max.dl <- max(idx.dlines)

### these two cases need to be handled slightly differently so not supported for now
        stopifnot(min.dl>1)
        stopifnot(max.dl<=length(lines))

        if(min.dl>1){
            newlines <- c(lines[1:(min.dl-1)],
                          newlines)
        }
        if(max.dl<length(lines)){
            newlines <- c(newlines,
                          lines[(max.dl+1):length(lines)])
        }

        ##  function end
        newlines
    }
    
    newlines <- lines
    for (I in 1:length(list.sections)) newlines <- replaceOnePart(lines=newlines,section=names(list.sections)[I],newlines=list.sections[[I]])
    
    if(is.null(newfile)) return(newlines)
    
    if(file==newfile && backup ) file.copy (file,
                                            sub("(.+/)([^/].+$)","\\1backup_\\2",x=file)
                                            )

    if(!write){
        return(newlines)
    }

    con.newfile <- file(newfile,"wb")
    writeLines(newlines,con=con.newfile)
    close(con.newfile)
    return(invisible(newlines))
}
