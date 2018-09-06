#' Implementation of the BAS with momentum (also named as second-order BAS) for optimization problems.
#'
#' @description BAS with momentum introduces 'velocity' to the detecting rules of basic BAS. It is more
#' like the individual version of BSO The paper of this algorithm is not available now.
#' If you have any question, please contact Xiaoxiao Li (xiaoxiaoli1993@sina.com)
#' @param fn objective function; function need to be optimized
#' @param init default = NULL, it will generate randomly; Of course, you can specify it.
#' @param lower lower of parameters to be estimated; Default = c(-6,0) because of the test on
#' Michalewicz function of which thelower is c(-6,0); By the way, you should set one of
#' \emph{init} or \emph{lower} parameter at least to make the code know the dimensionality
#' of your problem.
#' @param upper upper of parameters; Default = c(-1,2).
#' @param c the ratio of step-size and search distance d. \deqn{d = \frac{step}{c}}
#' @param l0 position jitter factor constant.Default = 0.
#' @param l1 initial position jitter factor.Default = 0.
#' @param eta_l attenuation coefficient of jitter factor.\deqn{l^t = \eta_l * l^{t-1} + l_0}
#' @param step0 the minimal resolution of step-size
#' @param step initial step-size of beetle.\deqn{step = \eta_{step}(step-step0) + step0}
#' @param eta_step attenuation coefficient of step-size.\deqn{step^t = \eta_{step} * step^{t-1}}
#' @param n iterations times
#' @param seed random seed; default = NULL ; The results of BAS depend on random init value and random directions.
#' Therefore, if you set a random seed, for example,\code{seed = 1}, the results will remain the same
#' no matter how many times you repeat your experiments.
#' @param trace default = T; trace the process of BAS iteration.
#' @param steptol default = step0/2; Iteration will stop if step-size in current moment is less than
#' steptol.
#' @param pen penalty conefficient usually predefined as a large enough value, default 1e5
#' @param constr constraint function. For example, you can formulate \eqn{x<=10} as
#' \eqn{constr = function(x) return(x - 10)}.
#' @param w0 inertia weight; default = 0.7
#' @param w1 weight of the difference between left and right antenae. default = 0.2
#' \deqn{v = w_0v-w_1dir(f_{left}-f_{right})}
#' @param c0 the ratio of maximal speed and initial step.\deqn{v_{max} = c_0step}
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
#' fit<-
#'   BASoptim2(fn = mich,
#'             lower = c(-6,0),
#'             upper = c(-1,2),
#'             n = 100,
#'             trace = F,
#'             c = 0.4,#d = 1.2/0.4 = 3
#'             step = 1.2,
#'             seed = 1,
#'             w0 = 0.4,w1 = 0.2, c0 = 0.6)
#' fit$par;fit$value
#' func1 <- function(x){
#'   sum(x^2)
#' }
#' fit<-
#'   BASoptim2(fn = func1,
#'             lower = c(-100,-100),
#'             upper = c(100,100),
#'             n = 100,
#'             trace = F,
#'             c = 20,
#'             step = 100,
#'             seed = 1,
#'             w0 = 0.5,w1 = 0.2, c0 = 0.6)
#' fit$par;fit$value
#' func2 <- function(x){
#'   sum((abs(x)-5)^2)
#' }
#' fit<-
#'   BASoptim2(fn = func2,
#'             lower = c(-10,-10),
#'             upper = c(10,10),
#'             n = 100,
#'             trace = F,
#'             c = 5,
#'             step = 5,
#'             seed = 1,
#'             w0 = 0.2,w1 = 0.2, c0 = 0.6)
#' fit$par;fit$value
#' #======== examples end =======================
#' @importFrom stats runif
#' @export
BASoptim2 <- function(fn,init = NULL,
                     lower = c(-6,0),upper = c(-1,2),
                     constr = NULL,
                     c = 2,
                     l0 = 0, l1 = 0.0, eta_l = 0.95,
                     step0 = 5e-5,step = 0.8, eta_step = 0.95,
                     n = 200,seed = NULL,trace = T,
                     steptol = step0/2,pen = 1e5,
                     w0 = 0.7,w1 = 0.2,c0 = 0.6){

  ustep<- function(x){
    result <- sapply(x,function(x) ifelse(x > 0,1,0))
    return(result)
  }

  if(!is.null(seed)){
    set.seed(seed)
  }
  #d <- d1
  d <- step/c
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
  v <- 0
  vMax <- step*c0
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
  eps <- .Machine$double.eps
  # iteration loop -----------------------------------
  for (i in 1:n){
    #normalized direction
    dir <- runif(min = -1,max = 1, n = npar)
    dir <- dir/(eps + sqrt(sum(dir^2)))


    xleft <- handle.bounds(x + dir * d)#x + dir * d
    fleft <- fnew(xleft)

    xright <- handle.bounds(x - dir*d)#x - dir * d
    fright <- fnew(xright)

    #random walk
    # w <- l*runif(min = -1,max = 1, n = npar)
    # x <- handle.bounds(u = x - step * dir * sign(fleft - fright) + w)

    v <- w0*v - w1*dir*(fleft - fright)
    v <- vMax*(v>vMax) - vMax*(v < -vMax) + v*(v<=vMax)*(v>-vMax)
    x <- handle.bounds(u = x + v)

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
    step <- (step-step0)*eta_step+step0
    #d update
    d <- step/c
    #d <- d*eta_d + d0

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
