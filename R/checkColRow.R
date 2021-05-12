## not to be exported. This is a helper function for NMscanData.

checkColRow <- function(col.row,file){

    ## col.row must not be on the left-hand side of an assign statement (a single =) in the input control stream.

    sections <- NMreadSection(file)

### get rid of PROBLEM, INPUT, DATA, SUBROUTINE, THETA, OMEGA, SIGMA, ESTIMATION, TABLE
##    names.sections <- names(sections)
    
    

}
