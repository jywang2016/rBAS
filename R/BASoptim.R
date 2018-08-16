#' Implementation of the BAS algorithm for optimization problems.
#'
#' @description You could find more information about BAS in \url{https://arxiv.org/abs/1710.10724}.
#' @param fn objective function; function need to be optimized
#' @param init default = NULL, it will generate randomly; Of course, you can specify it.
#' @param lower lower of parameters to be estimated; Default = c(-6,0) because of the test on
#' Michalewicz function of which thelower is c(-6,0); By the way, you should set one of
#' \emph{init} or \emph{lower} parameter at least to make the code know the dimensionality
#' of your problem.
#' @param upper upper of parameters; Default = c(-1,2).
#' @param d0 a constant to gurantee that sensing length of antennae \emph{d} doesn't equal to
#' zero. More specifically, \deqn{d^t = \eta_d * d^{t-1} + d_0}where attenuation coefficient
#'  \eqn{\eta_d} belongs to \eqn{[0,1]}
#' @param d1 initial value of antenae length. You can specify it according to your problem scale
#' @param eta_d attenuation coefficient of sensing length of antennae
#' @param l0 position jitter factor constant.Default = 0.
#' @param l1 initial position jitter factor.Default = 0.\deqn{x = x - step * dir * sign(fn(left) - fn(right)) + l *random(npars)}
#' @param eta_l attenuation coefficient of jitter factor.\deqn{l^t = \eta_l * l^{t-1} + l_0}
#' @param step initial step-size of beetle
#' @param eta_step attenuation coefficient of step-size.\deqn{step^t = \eta_step * step^{t-1}}
#' @param n iterations times
#' @param seed random seed; default = NULL ; The results of BAS depend on random init value and random directions.
#' Therefore, if you set a random seed, for example,\code{seed = 1}, the results will remain the same
#' no matter how many times you repeat your experiments.
#' @param trace default = T; trace the process of BAS iteration.
#' @param steptol default = 0.01; Iteration will stop if step-size in current moment is less than
#' steptol.
#' @param pen penalty conefficient usually predefined as a large enough value, default 1e5
#' @param constr constraint function. For example, you can formulate \eqn{x<=10} as
#' \eqn{constr = function(x) return(x - 10)}.
#' @references X. Y. Jiang, and S. Li, BAS: beetle antennae search algorithm for
#' optimization problems, arXiv:1710.10724v1.
#' @return A list including best beetle position (parameters) and corresponding objective function value.
#' @examples
#' #======== examples start =======================
#' # BAS application on Michalewicz function
#' library(rBAS)
#' mich <- function(x){
#'   y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
#'   y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
#'   return(y1+y2)
#' }
#' BASoptim(fn = mich,
#'          lower = c(-6,0), upper = c(-1,2),
#'          seed = 1, n = 100)
#' #======== examples end =======================
#' @importFrom stats runif
#' @export
BASoptim <- function(fn,init = NULL,
                    lower = c(-6,0),upper = c(-1,2),
                    constr = NULL,
                    d0 = 0.001, d1 = 3, eta_d = 0.95,
                    #c2 = 5,
                    l0 = 0, l1 = 0.0, eta_l = 0.95,
                    step = 0.8, eta_step = 0.95,
                    n = 200,seed = NULL,trace = T,
                    steptol = 0.01,pen = 1e5){

  ustep<- function(x){
    result <- sapply(x,function(x) ifelse(x > 0,1,0))
    return(result)
  }

  if(!is.null(seed)){
    set.seed(seed)
  }
  d <- d1
  #d <- step/c2
  l <- l1
  npar <- length(lower)
  if(is.null(init)){
    x0 <- runif(min = lower, max = upper, n = npar)
  }else{
    x0 <- init
  }

  if(!is.null(names(lower))){
    names(x0) <- names(lower)
  }
  # redefine the fn
  if(is.null(constr)){
    fnew <- fn
  }else{
    fnew <- function(x) fn(x) + pen*sum(ustep(constr(x)))
  }
  # first iteration---------------------------------
  x <- x0
  xbest <- x0
  fbest <- fnew(xbest)
  if(trace){
    cat('Iter: ',0,' xbest: ','[',xbest,'], fbest= ',fbest,'\n')
  }

  x_store <- x
  f_store <- fbest
  fb_store <- fbest
  xb_store <- xbest

  handle.bounds <- function(u){
    #temp <- u - step * dir * sign(fleft - fright) + w
    temp <- u
    bad <- temp > upper
    if(any(bad)){
      temp[bad] <- upper[bad] #+ u[bad]
    }
    bad <- temp < lower
    if(any(bad)){
      temp[bad] <- lower[bad]
    }
    temp
  }

  # iteration loop -----------------------------------
  for (i in 1:n){
    #normalized direction
    dir <- runif(min = -1,max = 1, n = npar)
    dir <- dir/(1e-16 + sqrt(sum(dir^2)))


    xleft <- handle.bounds(x + dir * d)#x + dir * d
    fleft <- fnew(xleft)

    xright <- handle.bounds(x - dir*d)#x - dir * d
    fright <- fnew(xright)

    #random work
    w <- l*runif(min = -1,max = 1, n = npar)
    x <- handle.bounds(u = x - step * dir * sign(fleft - fright) + w)
    f <- fnew(x)

    # best position updates
    if (f < fbest){
      xbest <- x
      fbest <- f
    }

    x_store <- c(x_store,x)
    f_store <- c(f_store,f)
    fb_store <- c(fb_store,fbest)
    xb_store <- c(xb_store,xbest)

    # trace
    if(trace){
      cat('Iter: ',i,' xbest: ','[',xbest,'], fbest= ',fbest,'\n')
    }

    #l update
    l <- l * eta_l + l0
    #step update
    step <- step*eta_step
    #d update
    #d <- step/c2
    d <- d*eta_d + d0

    if(step < steptol){
      if(trace){
        message('----step < steptol----','-----stop the iteration------')
      }
      break
    }
  }
  x_store <- matrix(x_store,ncol = npar, byrow = T)
  f_store <- matrix(f_store,ncol = 1, byrow = T)
  xbest_store <- matrix(xb_store, ncol = npar, byrow = T)
  fbest_store <- matrix(fb_store, ncol = 1, byrow = T)

  result <- list(par = xbest,
                 value = fbest,
                 df = list(x = x_store,
                           f = f_store,
                           xbest = xbest_store,
                           fbest = fbest_store))
  return(result)
}
