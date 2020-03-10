##' replace ($)sections of a nonmem control stream
##'
##' Just give the section name, the new lines and the file path, and the
##' "$section", and the input to Nonmem will be updated.
##'
##' @param path The run to edit. If a directory is given, the file is assumed to
##'     be called input.txt in that folder.
##' @param section The name of the section to update. Example: section="EST" to
##'     edit the sections starting by $EST. See NMgetSection
##' @param newlines The new text (including "$SECTION"). Better be broken into
##'     lines in a character vector since this is simply written with
##'     writeLines.
##' @param newpath path to new run. If missing, path is used. If NULL, output is
##'     returned rather than written.
##' @param backup In case you are overwriting the old file, do you want to
##'     backup the file (to say, backup_input.txt)?
##' @param blank.append Append a blank line to output?
##' @param test Want to see the resulting input.txt and not write it to disk?
##'     Default is FALSE.
##' @param debug start by running browser()?
##'
##' @details The new file will be written with unix-style line endings.
##' @family Nonmem
##' 
##' @examples
##' newlines <- "$EST POSTHOC INTERACTION METHOD=1 NOABORT PRINT=5 MAXEVAL=9999 SIG=3"
##' NMreplacePart(path=NMdata_filepath("examples/nonmem/run001.mod"),
##' section="EST", newlines=newlines)
##' @export


NMreplacePart <- function(path,section,newlines,newpath,backup=T,blank.append=T,test=F,debug=F){
    if(debug) browser()

#### handle arguments
    path <- filePathSimple(path)
    file <- path
    if(dir.exists(path)) file <- file.path(path,"input.txt")
    stopifnot(file.exists(file))

    if(missing(newpath)) newpath <- path
    if(!is.null(newpath)){
        newfile <- filePathSimple(newpath)
        if(dir.exists(newpath)) newfile <- file.path(newpath,"input.txt")
        stopifnot(file.exists(newfile))
    }

    ##### I think this split is unnecessary and can cause trouble newlines is a vector of lines.
    ### a \n means a new line, so split by that
    ## newlines <- strsplit(newlines,"\n")[[1]]
    if(blank.append) newlines <- c(newlines,"")
    
######
    

## browser()
    
    ## see below why we need to read the lines for now
    lines <- readLines(file)
    idx.dlines <- NMgetSection(lines=lines,section=section,return="idx",keepEmpty=T,
                               keepName=T,keepComments=T,asOne=T,
                               cleanSpaces=F)

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

    if(is.null(newpath)) return(newlines)

    if(file==newfile && backup ) file.copy (file,
                                            sub("(.+/)([^/].+$)","\\1backup_\\2",x=file)
                                            )

    if(test){
        return(newlines)
    }

    con.newfile <- file(newfile,"wb")
    writeLines(newlines,con=con.newfile)
    close(con.newfile)
    
}
