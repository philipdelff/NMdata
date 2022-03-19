##' PK data borrowed from the xgxr package
##'
##' This dataset is used to illustrate functionality of the package. It is a
##' slightly modified edition of xgxr::case1_pkpd.
##' 
##' @format A data.table with 1500 rows, 24 columns. 150 subjects, 1 dose and 9
##'     observations each.
##' 
##' \describe{
##' \item{ROW}{A row identifier}
##' \item{ID}{Subject ID.}
##' \item{NOMTIME}{Nominal time since first dose.}
##' \item{TIME}{Actual time since first dose.}
##' \item{EVID}{0 for dose.}
##' \item{CMT}{Compartment identifier. 2 for central compartment.}
##' \item{AMT}{Dose amount.}
##' \item{DV}{Observation value.}
##' \item{BLQ}{Observation below LLOQ (0/1)}
##' \item{CYCLE}{Treatment cycle (all 1, not used).}
##' \item{DOSE}{Nominal dose (3-300).}
##' \item{FLAG}{Exclusion flag. All rows where FLAG is not 0 to be ignored by Nonmem.}
##' \item{PART}{Study part (all 1, not used).}
##' \item{PROFDAY}{Profile day (all 1, not used).}
##' \item{PROFTIME}{Profile nominal time (same as NOMTIME, not used).}
##' \item{STUDY}{Study number (all 1, not used).}
##' \item{WEIGHTB}{Bodyweight at baseline (kg).}
##' \item{eff0}{efficacy, subjet level.}
##' \item{EVENTU}{Unit as in the xgxr dataset.}
##' \item{NAME}{Event name (dosing/PK concentration)}
##' \item{TIMEUNIT}{Time unit (Hours)}
##' \item{TRTACT}{Treatment arm name}
##' \item{flag}{Character explanations to FLAG}
##' \item{trtact}{Same as TRTACT}
##' }
##' @source simulated
"xgxr1"

##' Same data as xgxr1
##'
##' Only difference from xgxr1 is that it is stored as both csv and rds.
"xgxr2"

##' Variant of xgxr1 for testing
##'
##' Only difference from xgxr1 is that column DOSE has been duplicated.
"xgxr3"

