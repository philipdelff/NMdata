### getParLines
getParLines <- function(file,section){
    
    ## get theta comments
### due to a bug in NMreadSection in NMdata 0.1.3 we need to run this in two steps with keep.comments=TRUE and then remove comments lines
    lines.thetas <- NMreadSection(file=file,section=section,keep.name=FALSE,keep.empty=FALSE,keep.comments=TRUE)
    
    lines.thetas <- sub(pattern="^ *;.*$",replacement="",x=lines.thetas)
    ## these will confuse in omega/sigma sections with the current method. For those, numbering has to be done if off-diag elements are defined.
    lines.thetas <- gsub("BLOCK(.+)","",lines.thetas)
    lines.thetas <- lines.thetas[!grepl("^ *$",lines.thetas)]
    lines.thetas
}
### getParTab
getParTab <- function(input, fields) {
    
    ## Create a pattern from the fields string
    pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", fields)
    pattern <- paste0("^", pattern, "$")
    
                                        # Extract the variable names from the fields string
    var_names <- unlist(regmatches(fields, gregexpr("%[a-zA-Z0-9]+", fields)))
    var_names <- gsub("%", "", var_names)

                                        # Match the input string against the pattern
    matches <- regexec(pattern, input)
    matched <- regmatches(input, matches)
    
                                        # Check if there's a match
    if (length(matched[[1]]) <= 1) {
                                        # Try partial match by reducing the pattern incrementally
        for (i in seq_along(var_names)) {
            partial_pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", paste0("^", paste(var_names[1:i], collapse="(.*?)"), ".*$"))
            
            partial_matches <- regexec(partial_pattern, input)
            partial_matched <- regmatches(input, partial_matches)
            if (length(partial_matched[[1]]) > 1) {
                values <- partial_matched[[1]][-1]
                result <- setNames(as.list(values), var_names[1:i])
                return(result)
            }
        }
        warning("Input does not match the fields.")
        return(NULL)
    }

                                        # Extract the matched values
    values <- matched[[1]][-1]
    
                                        # Create a named list with the variable names and values
    result <- setNames(as.list(values), var_names)
    
    return(result)
}


getParTab2 <- function(input, fields) {
    
                                        # Create a pattern from the fields string
    pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", fields)
    pattern <- paste0("^", pattern, "$")
    
                                        # Extract the variable names from the fields string
    var_names <- unlist(regmatches(fields, gregexpr("%[a-zA-Z0-9]+", fields)))
    var_names <- gsub("%", "", var_names)

                                        # Function to process a single input string
    process_single_input <- function(single_input) {
                                        # Match the input string against the pattern
        matches <- regexec(pattern, single_input)
        matched <- regmatches(single_input, matches)
        
                                        # Check if there's a match
        if (length(matched[[1]]) <= 1) {
                                        # Try partial match by reducing the pattern incrementally
            for (i in seq_along(var_names)) {
                partial_pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", paste0("^", paste(var_names[1:i], collapse="(.*?)"), ".*$"))
                
                partial_matches <- regexec(partial_pattern, single_input)
                partial_matched <- regmatches(single_input, partial_matches)
                if (length(partial_matched[[1]]) > 1) {
                    values <- partial_matched[[1]][-1]
                    result <- setNames(as.list(values), var_names[1:i])
                    return(result)
                }
            }
            warning("Input does not match the fields.")
            return(NULL)
        }
        
                                        # Extract the matched values
        values <- matched[[1]][-1]
        
                                        # Create a named list with the variable names and values
        result <- setNames(as.list(values), var_names)
        
        return(result)
    }
    
                                        # Apply the process_single_input function to each element of the input vector
    results <- lapply(input, process_single_input)
    
    return(results)
}


getParTab3 <- function(input, fields) {
    
    # Create a pattern from the fields string
    pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", fields)
    pattern <- paste0("^", pattern, "$")
    
    # Extract the variable names from the fields string
    var_names <- unlist(regmatches(fields, gregexpr("%[a-zA-Z0-9]+", fields)))
    var_names <- gsub("%", "", var_names)

    # Function to process a single input string
    process_single_input <- function(single_input) {
        # Remove leading and trailing whitespace
        single_input <- trimws(single_input)
        
        # Match the input string against the pattern
        matches <- regexec(pattern, single_input)
        matched <- regmatches(single_input, matches)
        
        # Check if there's a match
        if (length(matched[[1]]) <= 1) {
            # Try partial match by reducing the pattern incrementally
            for (i in seq_along(var_names)) {
                partial_pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", paste0("^", paste(var_names[1:i], collapse="(.*?)"), ".*$"))
                
                partial_matches <- regexec(partial_pattern, single_input)
                partial_matched <- regmatches(single_input, partial_matches)
                if (length(partial_matched[[1]]) > 1) {
                    values <- partial_matched[[1]][-1]
                    result <- setNames(as.list(values), var_names[1:i])
                    return(result)
                }
            }
            warning("Input does not match the fields.")
            return(NULL)
        }
        
        # Extract the matched values
        values <- matched[[1]][-1]
        
        # Create a named list with the variable names and values
        result <- setNames(as.list(values), var_names)
        
        return(result)
    }
    
    # Apply the process_single_input function to each element of the input vector
    results <- lapply(input, process_single_input)
    
    return(results)
}


getParTab4 <- function(input, fields) {
    
    # Extract the variable names from the fields string
    var_names <- unlist(regmatches(fields, gregexpr("%[a-zA-Z0-9]+", fields)))
    var_names <- gsub("%", "", var_names)
    
    # Create a pattern from the fields string
    pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", fields)
    # Escape all non-alphanumeric characters in the pattern
    pattern <- gsub("([^a-zA-Z0-9%])", "\\\\\\1", pattern)
    pattern <- paste0("^", pattern, "$")

    # Function to process a single input string
    process_single_input <- function(single_input) {
        # Remove leading and trailing whitespace
        single_input <- trimws(single_input)
        
        # Match the input string against the pattern
        matches <- regexec(pattern, single_input)
        matched <- regmatches(single_input, matches)
        
        # Check if there's a match
        if (length(matched[[1]]) <= 1) {
            # Try partial match by reducing the pattern incrementally
            for (i in seq_along(var_names)) {
                partial_pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", paste0("^", paste(var_names[1:i], collapse="(.*?)"), ".*$"))
                
                partial_matches <- regexec(partial_pattern, single_input)
                partial_matched <- regmatches(single_input, partial_matches)
                if (length(partial_matched[[1]]) > 1) {
                    values <- partial_matched[[1]][-1]
                    result <- setNames(as.list(values), var_names[1:i])
                    return(result)
                }
            }
            warning("Input does not match the fields.")
            return(NULL)
        }
        
        # Extract the matched values
        values <- matched[[1]][-1]
        
        # Create a named list with the variable names and values
        result <- setNames(as.list(values), var_names)
        
        return(result)
    }
    
    # Apply the process_single_input function to each element of the input vector
    results <- lapply(input, process_single_input)
    
    return(results)
}


getParTab5 <- function(input, fields) {
    # Extract the variable names from the fields string
    var_names <- unlist(regmatches(fields, gregexpr("%[a-zA-Z0-9]+", fields)))
    var_names <- gsub("%", "", var_names)
    
    # Create a pattern from the fields string by replacing variables with regex patterns
    pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", fields)
    # Escape special characters in the pattern
    pattern <- gsub("([\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}])", "\\\\\\1", pattern)
    
    # Ensure the pattern matches the entire string
    pattern <- paste0("^", pattern, "$")
    
    # Function to process a single input string
    process_single_input <- function(single_input) {
        # Remove leading and trailing whitespace
        single_input <- trimws(single_input)
        
        # Match the input string against the pattern
        matches <- regexec(pattern, single_input)
        matched <- regmatches(single_input, matches)
        
        # Check if there's a match
        if (length(matched[[1]]) <= 1) {
            # Try partial match by reducing the pattern incrementally
            for (i in seq_along(var_names)) {
                partial_pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", paste0("^", paste(var_names[1:i], collapse="(.*?)"), ".*$"))
                
                partial_matches <- regexec(partial_pattern, single_input)
                partial_matched <- regmatches(single_input, partial_matches)
                if (length(partial_matched[[1]]) > 1) {
                    values <- partial_matched[[1]][-1]
                    result <- setNames(as.list(values), var_names[1:i])
                    return(result)
                }
            }
            warning("Input does not match the fields.")
            return(NULL)
        }
        
        # Extract the matched values
        values <- matched[[1]][-1]
        
        # Create a named list with the variable names and values
        result <- setNames(as.list(values), var_names)
        
        return(result)
    }
    
    # Apply the process_single_input function to each element of the input vector
    results <- lapply(input, process_single_input)
    
    return(results)
}


getParTab6 <- function(input, fields) {
    
    # Extract the variable names from the fields string
    var_names <- unlist(regmatches(fields, gregexpr("%[a-zA-Z0-9]+", fields)))
    var_names <- gsub("%", "", var_names)
    
    # Create a pattern from the fields string by replacing variables with regex patterns
    pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", fields)
    # Escape special characters in the pattern
    pattern <- gsub("([\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}])", "\\\\\\1", pattern)
    
    # Ensure the pattern matches the entire string
    pattern <- paste0("^", pattern, "$")
    
    # Function to process a single input string
    process_single_input <- function(single_input) {
        
        # Remove leading and trailing whitespace
        single_input <- trimws(single_input)
        
        # Match the input string against the pattern
        matches <- regexec(pattern, single_input)
        matched <- regmatches(single_input, matches)
        
        # Check if there's a match
        if (length(matched[[1]]) <= 1) {
            # Try partial match by reducing the pattern incrementally
            for (i in seq_along(var_names)) {
                partial_pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", paste0("^", paste(var_names[1:i], collapse="(.*?)"), ".*$"))
                
                partial_matches <- regexec(partial_pattern, single_input)
                partial_matched <- regmatches(single_input, partial_matches)
                if (length(partial_matched[[1]]) > 1) {
                    values <- partial_matched[[1]][-1]
                    result <- setNames(as.list(values), var_names[1:i])
                    return(result)
                }
            }
            warning("Input does not match the fields.")
            return(NULL)
        }
        
        # Extract the matched values
        values <- matched[[1]][-1]
        
        # Create a named list with the variable names and values
        result <- setNames(as.list(values), var_names)
        
        return(result)
    }
    
    # Apply the process_single_input function to each element of the input vector
    results <- lapply(input, process_single_input)
    
    return(results)
}

getParTab7 <- function(input, fields) {
    
    # Extract the variable names from the fields string
    var_names <- unlist(regmatches(fields, gregexpr("%[a-zA-Z0-9]+", fields)))
    var_names <- gsub("%", "", var_names)
    
    # Create a pattern from the fields string by replacing variables with regex patterns
    pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", fields)
    # Escape special characters in the pattern
    pattern <- gsub("([\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}])", "\\\\\\1", pattern)
    
    # Ensure the pattern matches the entire string
    pattern <- paste0("^", pattern, "$")
    
    # Function to process a single input string
    process_single_input <- function(single_input) {
        # Remove leading and trailing whitespace
        single_input <- trimws(single_input)
        
        # Match the input string against the pattern
        matches <- regexec(pattern, single_input)
        matched <- regmatches(single_input, matches)
        
        # Check if there's a match
        if (length(matched[[1]]) <= 1) {
            # Try partial match by reducing the pattern incrementally
            for (i in seq_along(var_names)) {
                partial_fields <- paste0("%", var_names[1:i], collapse="; ")
                partial_pattern <- gsub("%[a-zA-Z0-9]+", "(.*?)", partial_fields)
                partial_pattern <- gsub("([\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}])", "\\\\\\1", partial_pattern)
                partial_pattern <- paste0("^", partial_pattern, ".*$")
                
                partial_matches <- regexec(partial_pattern, single_input)
                partial_matched <- regmatches(single_input, partial_matches)
                if (length(partial_matched[[1]]) > 1) {
                    values <- partial_matched[[1]][-1]
                    result <- setNames(as.list(values), var_names[1:i])
                    return(result)
                }
            }
            warning("Input does not match the fields.")
            return(NULL)
        }
        
        # Extract the matched values
        values <- matched[[1]][-1]
        
        # Create a named list with the variable names and values
        result <- setNames(as.list(values), var_names)
        
        return(result)
    }
    
    # Apply the process_single_input function to each element of the input vector
    results <- lapply(input, process_single_input)
    
    return(results)
}



if(F){
### orpphaned from get.comments. This should come afer getParTab
    thetas[,par.type:=toupper(section)]
    if(use.theta.nums){
        thetas[,i:=num]
    } else {
        thetas[,i:=.I]
    }
}

######## test
## fileRef <- "~/wdirs/NMdata/tests/testthat/testReference/NMreadParText_02.rds"
file.mod <- "~/wdirs/NMdata/tests/testthat/testData/nonmem/xgxr032.mod"
NMdataConf(reset=T)
NMdataConf(as.fun="data.table")

## res <- NMreadParsText(file.mod)
plines <- getParLines(file.mod,"THETA")
getParTab2(plines,fields="%init;%symbol;%num;%label;%unit")


paste("\"",paste(plines, collapse="\",\"") ,"\n\"") |> cat()

input <- c(
"   (.1)             ; LTVKA",
"  (3)             ; LTVV2",
"  (1)             ; LTVCL",
"  (4)             ; LTVV3",
"  (-1)             ; LTVQ "
)
##input

getParTab5(input,fields="%init; %symbol; %num; %label; %unit")

fields_format <- "%init; %symbol; %num; %label; %unit"
getParTab6(input,fields=fields_format)
getParTab7(input,fields=fields_format)
