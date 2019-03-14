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
#' @param pen penalty conefficient usually predefined as a large enough value, default 1e5
#' @param constr constraint function. For example, you can formulate \eqn{x<=10} as
#' \eqn{constr = function(x) return(x - 10)}.
#' @return A list including best beetle position (parameters) and corresponding objective function value.
#' @references X. Y. Jiang, and S. Li, BAS: beetle antennae search algorithm for
#' optimization problems, arXiv:1710.10724v1.
#' @importFrom stats runif
#' @examples
#' #======== examples start =======================
#' # >>>>>> example without constraint: Michalewicz function <<<<<<
#' library(rBAS)
#' mich <- function(x){
#'    y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
#'    y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
#'    return(y1+y2)
#' }
#' result <- BSASoptim(fn = mich,
#'                     lower = c(-6,0), upper = c(-1,2),
#'                     seed = 1, n = 100,k=5,step = 0.6,
#'                     trace = FALSE)
#' result$par
#' result$value
#'
#' # >>>> example with constraint: Mixed integer nonlinear programming <<<<
#' pressure_Vessel <- list(
#'   obj = function(x){
#'     x1 <- floor(x[1])*0.0625
#'     x2 <- floor(x[2])*0.0625
#'     x3 <- x[3]
#'     x4 <- x[4]
#'     result <- 0.6224*x1*x3*x4 + 1.7781*x2*x3^2 +3.1611*x1^2*x4 + 19.84*x1^2*x3
#'   },
#'   con = function(x){
#'     x1 <- floor(x[1])*0.0625
#'     x2 <- floor(x[2])*0.0625
#'     x3 <- x[3]
#'     x4 <- x[4]
#'     c(
#'       0.0193*x3 - x1,
#'       0.00954*x3 - x2,
#'       750.0*1728.0 - pi*x3^2*x4 - 4/3*pi*x3^3
#'     )
#'   }
#' )
#' result <- BSASoptim(fn = pressure_Vessel$obj,
#'                     k = 10,
#'                     lower =c( 1, 1, 10, 10),
#'                     upper = c(100, 100, 200, 200),
#'                     constr = pressure_Vessel$con,
#'                     n = 200,
#'                     step = 100,
#'                     d1 = 4,
#'                     pen = 1e6,
#'                     steptol = 1e-6,
#'                     n_flag = 2,
#'                     seed = 2,trace = FALSE)
#'
#' result$par
#' result$value
#'
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
#' result <- BSASoptim(fn = himmelblau$obj,
#'                     k = 5,
#'                     lower =c(78,33,27,27,27),
#'                     upper = c(102,45,45,45,45),
#'                     constr = himmelblau$con,
#'                     n = 200,
#'                     step = 100,
#'                     d1 = 10,
#'                     pen = 1e6,
#'                     steptol = 1e-6,
#'                     n_flag = 2,
#'                     seed = 11,trace = FALSE)
#' result$par # 78.01565 33.00000 27.07409 45.00000 44.95878
#' result$value # -31024.17
#' #======== examples end =======================
#' @export
BSASoptim <- function(fn,init = NULL,
                      lower = c(-6,0),upper = c(-1,2),
                      k = 5,
                      constr = NULL,
                      d0 = 0.001, d1 = 3, eta_d = 0.95,
                      #c2 = 5,
                      l0 = 0, l1 = 0.0, eta_l = 0.95,
                      step = 0.8, eta_step = 0.95,
                      n = 200,seed = NULL,trace = T,steptol = 0.01,
                      p_min = 0.2, p_step = 0.2,n_flag = 2,
                      pen = 1e5){
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

  if(is.null(init)){
    #stop('Please initialize parameters : init ')
    if(is.null(lower) & is.null(upper)){
      stop('Please specify at least one of the init, lower or upper.\n If one of the bound is infinite, please specify init!')
    }else if(is.null(lower)){
      npar <- length(upper)
      lower <- rep(-Inf,npar)
      warning('Please specify init when your bound is NULL or INFINITE')
    }else if(is.null(upper)){
      npar <- length(lower)
      upper <- rep(Inf,npar)
      warning('Please specify init when your bound is NULL or INFINITE')
    }else{
      if(!(length(lower) ==length(upper))){
        stop('Please check the length of lower and upper!')
      }else{
        npar <- length(lower)
      }
    }
    x0 <- runif(min = lower, max = upper, n = npar)

  }else{
    x0 <- init
    npar <- length(init)
  }
  if(is.null(upper)){
    upper <- rep(Inf,npar)
  }
  if(is.null(lower)){
    lower <- rep(-Inf,npar)
  }
  if(!(length(x0) == npar & length(lower) ==npar & length(upper)==npar)){
    stop('Please check the length of init, lower and upper!')
  }
  if(!is.null(names(lower))){
    names(x0) <- names(lower)
  }else if(!is.null(names(upper))){
    names(x0) <- names(upper)
  }

  len <- length(x0)
  parname <- names(x0)

  handle.bounds <- function(u){
    temp <- u
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
  xbest <- x0
  fbest <- fnew(xbest)
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
    if(npar == 1) dir <- matrix(dir, ncol = k)
    xleft <- handle.bounds(x + dir * d)#x + dir * d #
    rownames(xleft) <- parname
    fleft <- apply(xleft,2,fnew)

    xright <- handle.bounds(x - dir * d)#x - dir * d #
    rownames(xright) <- parname
    fright <- apply(xright,2,fnew)#fn(xright)

    w <- l*runif(min = -1,max = 1, n = npar*k)
    w <- matrix(w,ncol = k)

    x_mat <- matrix(rep(x,k),ncol = k,byrow = F)
    dotdir <- matrix(rep(sign(fleft - fright),len),nrow = len,byrow = T)
    x_mat <- x_mat - step * dir * dotdir + w

    x_temp <- handle.bounds(x_mat)
    rownames(x_temp) <- parname
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
        #when length(x) == 1 sample(x,size) will deal with x as 1:x
        #therefore, length of index need to be judged
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
        d <- d*eta_d + d0
        l <- l * eta_l + l0
        flag_step <- 0
      }else{
        step <- step
        flag_step <- flag_step + 1
      }
      if(trace){
        cat('Step-size:',step,'\n')
      }
    }
    #d <- d*eta_d + d0
    #l <- l * eta_l + l0
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

