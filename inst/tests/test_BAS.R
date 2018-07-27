library(testthat)
test_that(desc = 'BAS application on Michalewicz function',
          code = {
            mich <- function(x){
              y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
              y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
              return(y1+y2)
            }
            res_BAS <- BASoptim(fn = mich,
                                lower = c(-6,0), upper = c(-1,2),
                                seed = 1, n = 100)
          })

