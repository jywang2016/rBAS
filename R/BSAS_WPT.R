#' Implementation of the Beetle Swarm Antennae Search without parameters tunning (BSAS-WPT) algorithm
#' for optimization problems.
#'
#' @description \emph{BSAS-WPT} is integration of \emph{BSAS} and \emph{BAS-WPT}. The main difference
#' bettwen \emph{BSAS-WPT} and \emph{BSAS}
#' is the parameters \emph{c2} used for searching distance update. The users just need specify \emph{c2}
#' instead of adjusting
#' much parameters about searching distance.
#' @param c2 ratio of step-size and searching distance.\deqn{d = \frac{step}{c2}}
#' @param pen penalty conefficient usually predefined as a large enough value, default 1e5
#' @param constr constraint function. For example, you can formulate \eqn{x<=10} as
#' \eqn{constr = function(x) return(x - 10)}.
#' @param \dots see \code{\link{BSASoptim}}.
#' @return A list including best beetle position (parameters) and corresponding objective function value.
#' @references X. Y. Jiang, and S. Li, "Beetle Antennae Search without Parameter Tuning (BAS-WPT) for Multi-objective
#' Optimization," arXiv:1711.02395v1.\url{https://arxiv.org/abs/1711.02395}
#'
#' J. Y. Wang, and H. X. Chen, "BSAS: Beetle Swarm Antennae Search Algorithm for Optimization Problems,"
#' arXiv:1807.10470v1.\url{https://arxiv.org/abs/1807.10470}
#' @importFrom stats runif
#' @examples
#' #======== examples start =======================
#' # >>>> example with constraint: Mixed integer nonlinear programming <<<<
#' pressure_Vessel <- list(
#' obj = function(x){
#'   x1 <- floor(x[1])*0.0625
#'   x2 <- floor(x[2])*0.0625
#'   x3 <- x[3]
#'   x4 <- x[4]
#'   result <- 0.6224*x1*x3*x4 + 1.7781*x2*x3^2 +3.1611*x1^2*x4 + 19.84*x1^2*x3
#' },
#' con = function(x){
#'   x1 <- floor(x[1])*0.0625
#'   x2 <- floor(x[2])*0.0625
#'   x3 <- x[3]
#'   x4 <- x[4]
#'   c(
#'     0.0193*x3 - x1,#<=0
#'     0.00954*x3 - x2,
#'     750.0*1728.0 - pi*x3^2*x4 - 4/3*pi*x3^3
#'   )
#' }
#' )
#' result <- BSAS_WPT(fn = pressure_Vessel$obj,
#'                    k = 8,
#'                    lower =c( 1, 1, 10, 10),
#'                    upper = c(100, 100, 200, 200),
#'                    constr = pressure_Vessel$con,
#'                    c2 = 10, n = 200, step = 2,
#'                    seed = 1,
#'                    n_flag = 3,
#'                    trace = FALSE,
#'                    steptol = 1e-6)
#' result$par
#' result$value
#' # >>>> example without constraint: Michalewicz function <<<<
#' mich <- function(x){
#' y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
#' y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
#' return(y1+y2)
#' }
#' result <- BSAS_WPT(fn = mich,
#'                    lower = c(-6,0), upper = c(-1,2),
#'                    seed = 11, n = 200,
#'                    k=5,
#'                    step = 1,
#'                    c2 = 5,
#'                    trace = FALSE)
#' result$par
#' result$value
#' # >>>> example with constraint: Himmelblau function <<<<
#' himmelblau <- list(
#'   obj = function(x){
#'     x1 <- x[1]
#'     x3 <- x[3]
#'     x5 <- x[5]
#'     result <- 5.3578547*x3^2 + 0.8356891*x1*x5 + 37.29329*x[1] - 40792.141
#'   },
#'   con = function(x){
#'     x1 <- x[1]
#'     x2 <- x[2]
#'     x3 <- x[3]
#'     x4 <- x[4]
#'     x5 <- x[5]
#'     g1 <- 85.334407 + 0.0056858*x2*x5 + 0.00026*x1*x4 - 0.0022053*x3*x5
#'     g2 <- 80.51249 + 0.0071317*x2*x5 + 0.0029955*x1*x2 + 0.0021813*x3^2
#'     g3 <- 9.300961 + 0.0047026*x3*x5 + 0.0012547*x1*x3 + 0.0019085*x3*x4
#'     c(
#'       -g1,
#'       g1-92,
#'       90-g2,
#'       g2 - 110,
#'       20 - g3,
#'       g3 - 25
#'     )
#'   }
#' )
#' result <- BSAS_WPT(fn = himmelblau$obj,
#'                    k = 10,
#'                    lower =c(78,33,27,27,27),
#'                    upper = c(102,45,45,45,45),
#'                    constr = himmelblau$con,
#'                    c2 = 5, n = 200, step = 1.6,
#'                    pen = 1e5,trace = FALSE,seed = 11)
#' result$par   # 78.00000 33.00000 27.07176 45.00000 44.96713
#' result$value # -31025.47
#' #======== examples end =======================
#' @export
#'
BSAS_WPT <- function(fn,init = NULL,
                     lower = c(-6,0),upper = c(-1,2),
                     k = 5,
                     constr = NULL,
                     c2 = 5,
                     step = 1, eta_step = 0.95,
                     n = 200,
                     seed = NULL,trace = T,steptol = 0.001,
                     p_min = 0.2, p_step = 0.2,n_flag = 2,
                     pen = 1e5){

  ustep<- function(x){
    result <- sapply(x,function(x) ifelse(x > 0,1,0))
    return(result)
  }

  xmin = lower
  xmax = upper

  unnor <- function(x,min = lower,max = upper){
    x *(max - min) + min
  }
  nor <- function(x,min = lower,max = upper){
    (x - min)/(max - min)
  }

  if(!is.null(seed)){
    set.seed(seed)
  }
  #d <- d1
  d <- step/c2
  #l <- l1
  npar <- length(lower)

  if(is.null(init)){
    x0 <- runif(min = 0, max = 1, n = npar)
  }else{
    x0 <- nor(init,max = upper,min = lower)
  }

  if(!is.null(names(lower))){
    names(x0) <- names(lower)
  }

  len <- length(x0)
  parname <- names(x0) #NULL for unnamed input

  handle.bounds <- function(u){
    temp <- u
    bad <- temp > 1
    if(any(bad)){
      temp[bad] <- 1
    }
    bad <- temp < 0
    if(any(bad)){
      temp[bad] <- 0
    }
    temp
  }


  # redefine the fn
  # redefine the fn
  if(is.null(constr)){
    fnew <- fn
  }else{
    fnew <- function(x) fn(x) + pen*sum(ustep(constr(x)))
  }

  #
  fnor <- function(x) x/(1e-16 + sqrt(sum(x^2)))
  # first iteration ---------------------------------
  x <- x0
  xbest <- unnor(x0) #un-nor
  fbest <- fnew(xbest)
  x_store <- xbest
  f_store <- fbest
  x <- xbest

  if(trace){
    cat('============ Initialization ===========','\n')
    cat('Iter: ',0,' xbest: ','[',xbest,'], fbest= ',fbest,'\n')
  }

  # npar rows & k cols
  upper <- matrix(rep(upper,k),ncol = k,byrow = F)
  lower <- matrix(rep(lower,k),ncol = k,byrow = F)
  rownames(upper) <- parname
  rownames(lower) <- parname

  flag_step <- 0
  # iteration loop -----------------------------------
  for (i in 1:n){

    if(trace){
      cat('============ Iter ',i,' start ===========','\n')
    }

    dir <- runif(min = -1, max = 1, n = npar*k)
    dir <- matrix(dir,nrow = k,byrow = T)
    dir <- apply(dir,1,fnor) #k rows npar cols

    x <- nor(x,min = xmin, max = xmax) # x should be nor when iter starts, un-nor when iter ends

    xleft <- handle.bounds(x + dir * d) # x + dir * d
    rownames(xleft) <- parname
    xleft <- apply(xleft,2,function(x) unnor(x,min = xmin, max = xmax)) #
    fleft <- apply(xleft,2,fnew)

    xright <- handle.bounds(x - dir * d) #x - dir * d
    rownames(xright) <- parname
    xright <- apply(xright,2,function(x) unnor(x,min = xmin, max = xmax))#un-nor
    fright <- apply(xright,2,fnew)#fn(xright)

    x_mat <- matrix(rep(x,k),ncol = k,byrow = F)# npar rows k cols
    dotdir <- matrix(rep(sign(fleft - fright),len),nrow = len,byrow = T) # npar rows k cols
    x_mat <- x_mat - step * dir * dotdir  # npar rows k cols

    x_temp <- handle.bounds(x_mat)
    rownames(x_temp) <- parname

    x_temp <- apply(x_temp,2,function(x) unnor(x,min = xmin, max = xmax)) #un-nor
    f_temp <-  apply(x_temp,2,fnew)

    if(trace){
      cat(k,'beetles\'objective value :','[',f_temp,']','\n')
    }


    if(min(f_temp) < fbest){
      if(trace){
        cat('***position updates***','\n')
      }
      r_temp <- runif(1)
      if(r_temp > p_min){
        index <- which.min(f_temp)
      }else{
        index_temp <- which(f_temp < fbest)
        index <- ifelse(length(index_temp) ==1,
                        index_temp,
                        sample(index_temp,size = 1))
      }

      x <- x_temp[,index]
      xbest <- x
      fbest <- f_temp[index]
      if(trace){
        cat('Position/Params updates','[',xbest,']\nCorresponding Objective value:',fbest,'\n')
      }


    }else{
      if(trace){
        cat('Position & Objective remain the same',fbest,'\nStep-size should reduce\n')
      }

      r_temp <- runif(1)
      if(r_temp > p_step | flag_step > n_flag){
        step <- step*eta_step
        flag_step <- 0
      }else{
        step <- step
        flag_step <- flag_step + 1
      }
      if(trace){
        cat('Step-size:',step,'\n')
      }
      x <- unnor(x, min = xmin, max = xmax) #un-nor...
    }
    d <- step/c2

    x_store <- c(x_store,x)
    f_store <- c(f_store,fbest)

    if(step < steptol){
      message('----step < steptol----','-----stop the iteration------')
      break
    }
  }

  x_store <- matrix(x_store,ncol = npar, byrow = T)
  f_store <- matrix(f_store,ncol = 1, byrow = T)

  result <- list(par = xbest,
                 value = fbest,
                 df = list(x = x_store,
                           f = f_store))
  return(result)
}

