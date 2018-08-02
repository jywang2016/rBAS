#' Implementation of the Beetle Swarm Antennae Search (BSAS) algorithm for optimization problems.
#'
#' @description You could find more information about BSAS in \url{https://arxiv.org/abs/1807.10470}.
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
#' @param p_min a constant belongs to [0,1]. If random generated value is lager than p_min and there are better positions
#' in \emph{k} beetles than current position, the next position of beetle will be the best position in \emph{k} beetles.
#' \deqn{x_{best} = argmin(fn(x_i^t))}
#' where \eqn{i belongs [1,2,...,k]}
#' If random generated value is smaller than(<=) p_min and there are better positions in \emph{k} beetles than current position,
#' the next position of the beetle could be the random position within better positions.
#' @param p_step a constant belongs to [0,1].If  no better position in \emph{k} beetles than current position,
#' there is still a samll probability that step-size doesn't update. If you set a little \emph{k}, you could set p_step
#' slightly large.
#' @param n_flag an positive integer; default = 2; If step-size doesn't update for successive n_flag times because p_step is
#' larger than random generated value, the step-size will be forced updating. If you set a large p_step, set a small n_flag
#' is suggested.
#' @param k a positive integer.\emph{k} is the number of beetles for exploring in every iteration.
#' @return A list including best beetle position (parameters) and corresponding objective function value.
#' @references X. Y. Jiang, and S. Li, BAS: beetle antennae search algorithm for
#' optimization problems, arXiv:1710.10724v1.
#' @importFrom stats runif
#' @examples
#' #======== examples start =======================
#' # BSAS application on Michalewicz function
#' library(rBAS)
#' mich <- function(x){
#'    y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
#'    y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
#'    return(y1+y2)
#' }
#' BSASoptim(fn = mich,
#'           lower = c(-6,0), upper = c(-1,2),
#'           seed = 12, n = 100,k=5)
#' #======== examples end =======================
#' @export
BSASoptim <- function(fn,init = NULL,
                      lower = c(-6,0),upper = c(-1,2),
                      k = 5,
                      d0 = 0.001, d1 = 3, eta_d = 0.95,
                      l0 = 0, l1 = 0.0, eta_l = 0.95,
                      step = 0.8, eta_step = 0.95,
                      n = 200,seed = NULL,trace = T,steptol = 0.01,
                      p_min = 0.2, p_step = 0.2,n_flag = 2){

  if(!is.null(seed)){
    set.seed(seed)
  }
  d <- d1
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

  len <- length(x0)
  parname <- names(x0)
  handle.bounds <- function(u){
    dotdir <- matrix(rep(sign(fleft - fright),len),nrow = len,byrow = T)
    temp <- u - step * dir * dotdir + w
    bad <- temp > upper
    if(any(bad)){
      temp[bad] <- upper[bad]
    }
    bad <- temp < lower
    if(any(bad)){
      temp[bad] <- lower[bad]
    }
    return(temp)
  }
  handle.bounds.LR <- function(x,act_dir = 'left'){

    if(act_dir == 'left'){
      temp <- x + dir * d
      bad <- temp > upper
      if(any(bad)){
        temp[bad] <- upper[bad]
      }
      bad <- temp < lower
      if(any(bad)){
        temp[bad] <- lower[bad]
      }

    }else{
      temp <- x - dir * d
      bad <- temp > upper
      if(!is.null(bad)){
        temp[bad] <- upper[bad]
      }
      bad <- temp < lower
      if(any(bad)){
        temp[bad] <- lower[bad]
      }
    }

    return(temp)
  }

  fnor <- function(x) x/(1e-16 + sqrt(sum(x^2)))
  # first iteration ---------------------------------
  x <- x0
  xbest <- x0
  fbest <- fn(xbest)
  x_store <- x
  f_store <- fbest

  if(trace){
    cat('============ Initialization ===========','\n')
    cat('Iter: ',0,' xbest: ','[',xbest,'], \nfbest= ',fbest,'\n')
  }


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
    dir <- apply(dir,1,fnor)

    xleft <- handle.bounds.LR(x,act_dir = 'left')
    rownames(xleft) <- parname
    fleft <- apply(xleft,2,fn)#fn(xleft)

    xright <- handle.bounds.LR(x,act_dir = 'right')
    rownames(xright) <- parname
    fright <- apply(xright,2,fn)#fn(xright)

    w <- l*runif(min = -1,max = 1, n = npar*k)
    w <- matrix(w,ncol = k)

    x_mat <- matrix(rep(x,k),ncol = k,byrow = F)
    x_temp <- handle.bounds(u = x_mat)
    rownames(x_temp) <- parname
    f_temp <-  apply(x_temp,2,fn)

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
        index <- sample(x = which(f_temp < fbest), size = 1)
      }

      x <- x_temp[,index]
      xbest <- x
      fbest <- min(f_temp)
      if(trace){
        cat('Position/Params updates','[',xbest,']\nCorresponding Objective value:',fbest,'\n')
      }


    }else{
      if(trace){
        cat('Position & Objective remain the same',fbest,'\nStep-size should reduce\n')
      }

      d <- d * eta_d + d0
      l <- l * eta_l + l0
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
    }
    x_store <- c(x_store,x)
    f_store <- c(f_store,fbest)

    if(step < steptol){
      message('----step < steptol----','-----stop the iteration------\n')
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
