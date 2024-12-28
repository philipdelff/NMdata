
library(devtools)
load_all("~/wdirs/NMdata")

if(F){
    library(data.table)

    text <- c("
; matches format
$OMEGA  BLOCK(3)
0.126303  ;    IIV.CL  ; 1   ;IIV     ;Between-subject variability on CL;-
  0.024  ; IIV.CL.V2.cov  ; 1-2 ;IIV     ;Covariance of BSV on CL and V2;-
  0.127  ;    IIV.V2  ; 2   ;IIV     ;Between-subject variability on V2;-
  0.2  ; IIV.CL.V3.cov  ; 1-3 ;IIV     ;Covariance of BSV on CL and V3;-
  0.2  ; IIV.V2.V3.cov  ; 2-3 ;IIV     ;Covariance of BSV on V2 and V3;-
  (0,0.38,9)  ;    IIV.V3  ; 3   ;IIV     ;Between-subject variability on V3;-
$OMEGA 0 FIX ; IIV.KA ; 4  ;IIV     ;Between-subject variability on KA;-
")

    lines <- strsplit(text,split="\n")[[1]]

    lines


    om <- NMreadSection(lines=lines,section="OMEGA",keep.comments=T,as.one=T,keep.empty=T,keep.name=F)

    dt.lines <- data.table(text=om)[,row:=.I]
    ## remove comments
    dt.lines[,code:=sub(";.*","",text)]
    ## make "0 . 3" into "0.3"
    dt.lines[,code:=gsub(" *\\. *",".",code)]
    dt.lines[,code:=gsub("\\( *","(",code)]
    dt.lines[,code:=gsub(" *)",")",code)]
    dt.lines[,code:=gsub(" *, *",",",code)]
### not needed: derive (lower,init,upper)
    ## remove SAME - this should rather remover everything that is not either BLOCK(), a numeric or (numerics,)
    dt.lines[,code:=gsub("SAME","",code)]
    dt.lines[,code:=gsub("FIX","",code)]
    ## remove extra spaces
    dt.lines[,code:=cleanSpaces(code)]
    all.elements <- paste(dt.lines[,code], collapse=" ")

    dt.lines[,code.blocks:=list(regmatches(code, gregexpr("BLOCK *\\( *([1-9][0-9]*) *\\)",code,perl=T)))]
    dt.lines[,gsub("BLOCK *\\( *([1-9][0-9]*) *\\)","\\1",code)]

    ## a dt with a column containing lists of i and j. Or i and j are
    ## columns wit vectors - I dont know if that's possible.



    ## identify number of parameters in each line

    ## assign te indexes

######## approach 2


    library(stringr)

                                        # Updated regex pattern to include FIX and FIXED
    pattern <- "BLOCK\\(\\d+\\)|\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)|\\bFIX\\b|\\bFIXED\\b"

    pattern <- paste(
        "BLOCK\\(\\d+\\)",                         # BLOCK(N)
        "\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)", # (ll,init,ul)
        "\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)", # (ll,init)
        "\\(-?\\d+(\\.\\d+)?\\)",                  # (init)
        "\\b-?\\d+(\\.\\d+)?\\b",                  # init (standalone number)
        "\\bFIX\\b",                               # FIX
        "\\bFIXED\\b",                             # FIXED
        sep = "|"
    )

    lines2 <- str_replace(lines, ";.*", "")

                                        # Extract matches
    matches <- str_extract_all(lines2, pattern)

                                        # Print results
    for (i in seq_along(lines2)) {
        cat("Line:", lines2[i], "\n")
        cat("Matches:", paste(matches[[i]], collapse = ", "), "\n\n")
    }

    for (i in seq_along(lines2)) {
        cat("Original Line:", lines[i], "\n")
        cat("Cleaned Line:", lines2[i], "\n")
        cat("Matches:", paste(matches[[i]], collapse = ", "), "\n\n")
    }


########### approach 3
    library(stringr)

                                        # Comprehensive regex pattern
    pattern <- paste(
        "BLOCK\\(\\d+\\)",                         # BLOCK(N)
        "\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)", # (ll,init,ul)
        "\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)", # (ll,init)
        "\\(-?\\d+(\\.\\d+)?\\)",                  # (init)
        "\\b-?\\d+(\\.\\d+)?\\b",                  # init (standalone number)
        "\\bFIX\\b",                               # FIX
        "\\bFIXED\\b",                             # FIXED
        sep = "|"
    )

                                        # Example input lines
    lines <- c(
        "BLOCK(3) and (1.0,2.0,3.0) FIX this ; ignore this comment",
        "FIXED BLOCK(10) some text (0.1,-1.5) ; another comment here",
        "Just a standalone number 42 and (3.14) ; comment again",
        "Nothing to match here ; no matches"
    )

                                        # Preprocess to remove comments (everything after ";")
    lines_cleaned <- str_replace(lines, ";.*", "")

                                        # Extract matches
    matches <- str_extract_all(lines_cleaned, pattern)

                                        # Function to classify matches and insert NA where applicable
    classify_matches <- function(matches) {
        categorized <- list(
            BLOCK = NA,
            FIX = NA,
            ll = NA,
            init = NA,
            ul = NA
        )
        
        for (match in matches) {
            if (str_detect(match, "^BLOCK\\(\\d+\\)$")) {
                categorized$BLOCK <- match
            } else if (str_detect(match, "^FIX|FIXED$")) {
                categorized$FIX <- match
            } else if (str_detect(match, "^\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)$")) {
                                        # Split ll, init, ul
                nums <- as.numeric(str_match_all(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
                categorized$ll <- nums[1]
                categorized$init <- nums[2]
                categorized$ul <- nums[3]
            } else if (str_detect(match, "^\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)$")) {
                                        # Split ll, init
                nums <- as.numeric(str_match_all(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
                categorized$ll <- nums[1]
                categorized$init <- nums[2]
            } else if (str_detect(match, "^\\(-?\\d+(\\.\\d+)?\\)$")) {
                                        # Extract init
                nums <- as.numeric(str_match(match, "-?\\d+(\\.\\d+)?")[1, 1])
                categorized$init <- nums
            } else if (str_detect(match, "^-?\\d+(\\.\\d+)?$")) {
                                        # Standalone init
                nums <- as.numeric(match)
                categorized$init <- nums
            }
        }
        
                                        # Return categorized matches
        return(categorized)
    }

                                        # Process and classify matches for each line
    results <- lapply(matches, classify_matches)

                                        # Display results
    for (i in seq_along(lines)) {
        cat("Original Line:", lines[i], "\n")
        cat("Cleaned Line:", lines_cleaned[i], "\n")
        cat("Categorized Matches:\n")
        print(results[[i]])
        cat("\n")
    }

    do.call(rbind,results)


### approach 4 - wrapped up
    library(stringr)

    processLines <- function(lines) {
        
                                        # Comprehensive regex pattern
        pattern <- paste(
            "BLOCK\\(\\d+\\)",                         # BLOCK(N)
            "\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)", # (ll,init,ul)
            "\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)", # (ll,init)
            "\\(-?\\d+(\\.\\d+)?\\)",                  # (init)
            "\\b-?\\d+(\\.\\d+)?\\b",                  # init (standalone number)
            "\\bFIX\\b",                               # FIX
            "\\bFIXED\\b",                             # FIXED
            sep = "|"
        )
        
                                        # Preprocess to remove comments (everything after ";")
        lines_cleaned <- str_replace(lines, ";.*", "")

                                        # Extract matches
        matches <- str_extract_all(lines_cleaned, pattern)

                                        # Function to classify matches and insert NA where applicable
        classify_matches <- function(matches) {
            categorized <- list(
                BLOCK = NA,
                FIX = NA,
                ll = NA,
                init = NA,
                ul = NA
            )
            
            for (match in matches) {
                if (str_detect(match, "^BLOCK\\(\\d+\\)$")) {
                    categorized$BLOCK <- match
                } else if (str_detect(match, "^FIX|FIXED$")) {
                    categorized$FIX <- match
                } else if (str_detect(match, "^\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)$")) {
                                        # Split ll, init, ul
                    nums <- as.numeric(str_match_all(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
                    categorized$ll <- nums[1]
                    categorized$init <- nums[2]
                    categorized$ul <- nums[3]
                } else if (str_detect(match, "^\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)$")) {
                                        # Split ll, init
                    nums <- as.numeric(str_match_all(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
                    categorized$ll <- nums[1]
                    categorized$init <- nums[2]
                } else if (str_detect(match, "^\\(-?\\d+(\\.\\d+)?\\)$")) {
                                        # Extract init
                    nums <- as.numeric(str_match(match, "-?\\d+(\\.\\d+)?")[1, 1])
                    categorized$init <- nums
                } else if (str_detect(match, "^-?\\d+(\\.\\d+)?$")) {
                                        # Standalone init
                    nums <- as.numeric(match)
                    categorized$init <- nums
                }
            }
            
                                        # Return categorized matches
            return(categorized)
        }
        
                                        # Process and classify matches for each line
        results <- lapply(matches, classify_matches)
        
        return(results)
    }

                                        # Example usage:
    lines.gen <- c(
        "BLOCK(3) and (1.0,2.0,3.0) FIX this ; ignore this comment",
        "FIXED BLOCK(10) some text (0.1,-1.5) ; another comment here",
        "Just a standalone number 42 and (3.14) ; comment again",
        "Nothing to match here ; no matches"
    )

                                        # Call the function
    results <- processLines(lines.gen)

                                        # Display results
    for (i in seq_along(lines)) {
        cat("Original Line:", lines[i], "\n")
        cat("Cleaned Line:", str_replace(lines[i], ";.*", ""), "\n")
        cat("Categorized Matches:\n")
        print(results[[i]])
        cat("\n")
    }

    text <- c("
; matches format
$OMEGA  BLOCK(3)
0.126303  ;    IIV.CL  ; 1   ;IIV     ;Between-subject variability on CL;-
  0.024  ; IIV.CL.V2.cov  ; 1-2 ;IIV     ;Covariance of BSV on CL and V2;-
  0.127  ;    IIV.V2  ; 2   ;IIV     ;Between-subject variability on V2;-
  0.2  ; IIV.CL.V3.cov  ; 1-3 ;IIV     ;Covariance of BSV on CL and V3;-
  0.2  ; IIV.V2.V3.cov  ; 2-3 ;IIV     ;Covariance of BSV on V2 and V3;-
  (0,0.38,9)  ;    IIV.V3  ; 3   ;IIV     ;Between-subject variability on V3;-
$OMEGA 0 FIX ; IIV.KA ; 4  ;IIV     ;Between-subject variability on KA;-
")

    lines <- strsplit(text,split="\n")[[1]]

    lines

    results <- processLines(lines)
                                        # Display results
    for (i in seq_along(lines)) {
        cat("Original Line:", lines[i], "\n")
        cat("Cleaned Line:", str_replace(lines[i], ";.*", ""), "\n")
        cat("Categorized Matches:\n")
        print(results[[i]])
        cat("\n")
    }

    do.call(rbind,results)

}

###### approach 2024-12-22 07:16

library(stringr)
library(data.table)




text <- c("
; matches format
$OMEGA  BLOCK(2) 0.126303  ;    IIV.CL  ; 1   ;IIV     ;Between-subject variability on CL;-
  0.024  ; IIV.CL.V2.cov  ; 1-2 ;IIV     ;Covariance of BSV on CL and V2;-
  0.127  ;    IIV.V2  ; 2   ;IIV     ;Between-subject variability on V2;-
  0.2  ; IIV.CL.V3.cov  ; 1-3 ;IIV     ;Covariance of BSV on CL and V3;-
  0.2  ; IIV.V2.V3.cov  ; 2-3 ;IIV     ;Covariance of BSV on V2 and V3;-
  (0,0.38,9)  ;    IIV.V3  ; 3   ;IIV     ;Between-subject variability on V3;-
$OMEGA 0 FIX ; IIV.KA ; 4  ;IIV     ;Between-subject variability on KA;-
")

lines <- strsplit(text,split="\n")[[1]]
lines

if(F){
    res1 <- processLines(lines)
    res1
}

##### get info back on lines. First priority is i and j on the
##### lines. Look at result of NMreadParsText to see
##### how. NMreadParsText() should call this function.
if(F){
    file.mod <- "~/wdirs/NMdata/tests/testthat/testData/nonmem/xgxr033.mod"
    file.mod <- "/home/philip/wdirs/NMsim/devel/example_nonmem_models/lorlatinib_sim_est/mod_lorlatinib_estimate.mod"
    load_all("~/wdirs/NMdata")

    NMreadSection(file=file.mod,section="OMEGA")

    NMreadParsText(file=file.mod,format="%init;%par")

    lines.theta <- NMreadSection(file=file.mod,section="THETA")
    processLines(lines.theta)


    lines.theta <- NMreadSection(file=file.mod,section="THETA")
    lines.omega <- NMreadSection(file=file.mod,section="OMEGA")
    lines.sigma <- NMreadSection(file=file.mod,section="SIGMA")

    lines.omega
    
    processLines(lines.theta)
    processLines(lines.omega)
    processLines(lines.sigma)

    
}


###### write lines ctl style

parsToCtl <- function(res1){
}




## Ellis is a fun girl! I am proud to be her dad.

## brody

## yujjklmmmmmmmmmmmbzzxcvbnmasdfghjklqweertyuioooop12345678900`1234567890qwert890-sxcvhjkl <- 'fgnhmj,k.l 


#### combine NMreadParsText with 
