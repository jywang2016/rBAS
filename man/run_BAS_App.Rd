% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_BAS_App.R
\name{run_BAS_App}
\alias{run_BAS_App}
\title{Run shiny interface for BAS/BSAS algorithms}
\usage{
run_BAS_App(func, constr = NULL, theme = "united")
}
\arguments{
\item{func}{objective function; see example for more informations}

\item{constr}{constraints function; see example for more informations}

\item{theme}{shiny interface themes, default `united`; `theme` should be one of c("cerulean","cosmo","cyborg","darkly","flatly",
"journal","lumen","paper","readable","sandstone","simplex","slate","spacelab","superhero","united","yeti")
see \code{\link[shinythemes]{shinytheme}} for more information.}
}
\description{
`run_BAS_App` is built based on `shiny` and `shinythemes`. All you have to do is building your objective function
in R code. Then, you can change the parameters in Shiny interface and run BAS/BSAS algorithm.
}
\examples{
#======== examples start =======================
# BSAS application on Michalewicz function
library(rBAS)
mich <- function(x){
   y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
   y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
   return(y1+y2)
}
#run_BAS_App(func = mich)
#======== examples end =======================
}
