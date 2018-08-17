rm(list = ls())
library(devtools)
library(roxygen2)
library(testthat)

load_all()

mich <- function(x){
  y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
  y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
  return(y1+y2)
}
res_BSAS <- BSASoptim(fn = mich,
                       lower = c(-6,0), upper = c(-1,2),
                       seed = 12, n = 100,k=5)
test_file(paste0(getwd(),'/inst/tests/test_BAS.R'))
test_file(paste0(getwd(),'/inst/tests/test_BSAS.R'))

document()

check()

mich <- function(x){
  y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
  y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
  return(y1+y2)
}
run_BAS_App(func = mich, theme = 'united')
pkgdown:::build_site_external()

