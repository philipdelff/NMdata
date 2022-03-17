
test_that("multiple levels",{
    fileRef <- "testReference/findCovs_findVars_1.rds"

    dt1 <- data.table(ID=c(1,1,2,2),OCC=c(1,2,1,2),
                      ## ID level
                      eta1=c(1,1,3,3)
                      ## occasion level
                     ,eta2=c(1,3,1,5)
                      ## not used
                     ,eta3=0
                      )
    res <- list(
        ## model level
        findCovs(dt1)
        ## ID level
       ,findCovs(dt1,"ID")
        ## acual ID level
       ,findVars(findCovs(dt1,"ID"))
        ## occasion level
       ,findCovs(findVars(dt1,"ID"),c("ID","OCC"))

    )
    expect_equal_to_reference(res,fileRef,version=2)
}
)
