##' Translate Nonmem $PK, $PRED sections or other Nonmem code to R code
##'
##' @param text the Nonmem code.
##' @details You probably want to run this on text obtained by using
##'     the NMreadSection function.
##' @return R code as text
##' @keywords internal

### Do not export. For an exported function, see the pmxtricks package.

NMcode2R <- function(text){

### not there yet.
    ## preceed by begin of line, (, { or whitespace
    ## followed by end of line, (, { or whitespace
    require.word <- function(string) paste0("(^| *)",string,"( *|\\(|$)")
    fix.word <- function(string) paste0("\\1",string,"\\2")

    ## not sure this belongs here
    text <- gsub("\\$PK","",x=text)

    ## tabulators are just a blank
    text <- gsub("\t"," ",x=text)
    
    text <- gsub(require.word("ELSEIF"),fix.word("} else if"),x=text)
    text <- gsub(require.word("IF"),fix.word("if"),x=text)
    text <- gsub(require.word("THEN"),fix.word("{"),x=text)
    text <- gsub(require.word("ENDIF"),fix.word("}"),x=text)
    ## maybe this will make trouble if an else is in one line, ie without IF THEN
    text <- gsub("ELSE",fix.word("} else {"),x=text)
    
### logical
    text <- gsub("\\.GT\\.",">",x=text)
    text <- gsub("\\.GE\\.",">=",x=text)
    text <- gsub("\\.LT\\.","<",x=text)
    text <- gsub("\\.LE\\.","<=",x=text)
    text <- gsub("\\.EQ\\.","==",x=text)
    text <- gsub("\\.NE\\.","!=",x=text)
    text <- gsub("\\.AND\\.","&&",x=text)
    text <- gsub("\\.OR\\.","||",x=text)
    text <- gsub("/=","!=",x=text)
    
### functions
    text <- gsub("EXP *\\(","exp(",x=text)
    text <- gsub("LOG *\\(","log(",x=text)

### variable indexing
    text <- gsub("\\(([0-9]*)\\)","[\\1]",x=text)

### comments
    text <- gsub(";","#",x=text)
    
    text
    
}
