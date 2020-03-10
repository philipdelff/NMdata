##' stamp a dataset or any other object
##'
##' Dataset metadata makes it possible to trace an archived dataset back to the
##' code that generated it. The metadata added by stampData can be accessed
##' using the function dsInfo.
##'
##' @param data The dataset to stamp.
##' @param script path to the script where the dataset was generated.
##' @param ... other named metadata elements to add to the dataset. Example:
##'     Description="PK data for ph1 trials in project".
##' @seealso objInfo
##' @family DataWrangling
##' @export


stampObj <- function(data,script,...){
    attr(data,"objInfo") <-
        list(
            DataCreateScript=script,
            CreationTime=Sys.time(),
            ...
        )
    data
}
