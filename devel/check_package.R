remotes::install_github("thinkr-open/checkhelper")
library(checkhelper)
path.pkg <- "~/wdirs/NMdata"
finds <- find_missing_tags(path.pkg)
as.data.table(finds)
file.csv <- tempfile()
fwrite(finds,file=file.csv)
as.data.table(finds)
globals <- get_no_visible(path.pkg, quiet = TRUE)
print_globals(globals)



######## NO!!!!!
### check_as_cran(check_dir = )
check_dir <- tempfile("example")
# Check the current directory
check_as_cran(check_dir = check_dir)
