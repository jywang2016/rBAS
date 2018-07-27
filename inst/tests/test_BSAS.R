library(testthat)
test_that(desc = 'BSAS application on Michalewicz function',
          code = {
            mich <- function(x){
              y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
              y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
              return(y1+y2)
            }
            res_BAS2 <- BSASoptim(fn = mich,
                                  lower = c(-6,0), upper = c(-1,2),
                                  seed = 12, n = 100,k=5)
          })