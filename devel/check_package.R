remotes::install_github("thinkr-open/checkhelper")
library(checkhelper)
path.pkg <- "~/wdirs/NMdata"
finds <- find_missing_tags(path.pkg)
globals <- get_no_visible(path.pkg, quiet = TRUE)
print_globals(globals)

######## NO!!!!!
### check_as_cran(check_dir = )
