#' Implementation of the Beetle Swarm Optimization(BSO) algorithm, which is mixture of BAS and PSO.
#'
#' @description \emph{BSO} is the combination of \emph{BAS} and \emph{PSO}. It improves the \emph{BAS} algorithm
#' by expanding an individual to a group in the way of \emph{PSO}. Note: In a word, 'swarm' in \emph{BSAS} is more like
#' that a beetle has many antennae pairs(searching direction) while the 'swarm' in \emph{BSO} means group just like PSO.
#' @param fn objective function; function need to be optimized
#' @param init default = NULL, it will generate randomly; Or you can specify it as a matrix. For example, if your problem
#' is defined as m beetles and n dimensions, you can specify init as a \eqn{m*n} matrix.
#' @param constr constraint function. For example, you can formulate \eqn{x<=10} as
#' \eqn{constr = function(x) return(x - 10)}.
#' @param lower lower of parameters to be estimated
#' @param upper upper of parameters
#' @param n maximum number of iterations
#' @param s a positive integer, beetles number. Default = \eqn{floor(10+2*\sqrt{dimensions})}
#' @param w a vector for calculating inertia weight, \eqn{(\omega_{max},\omega_{min})}, default c(0.9,0.4).
#' the strategy of decreasing inertia weight is as follows.
#' \deqn{\omega = \omega_{max} - (\omega_{max}-\omega_{min})\frac{k}{n}}.
#' \eqn{k} is the current number of iteration and \eqn{n} is the maximum number of iterations.
#' @param w_vs a positive constant belongs to [0,1]. Default = 0.4.
#' \deqn{X_{is}^{k+1}=X_{is}^k+ \lambda V_{is}^k+(1-\lambda)\xi_{is}^k}
#' \emph{w_vs} is \eqn{\lambda}, which means the weight of speed(PSO) and beetle movement(BAS).
#' @param step initial step-size of beetles
#' @param step_w a vector used for step-size updating, default c(0.9,0.4).
#' \deqn{\delta_{k+1} = \eta \delta_{k}}
#' \deqn{\eta = \delta_{w1}(\frac{\delta_{w0}}{\delta_{w1}})^{\frac{1}{1+10*k/n}}} step_w = \eqn{(\delta_{w0},\delta_{w1})}
#' @param c ratio of step-size and searching distance.\deqn{d = \frac{step}{c}}
#' @param v the speed range of beetles. Default = c(-5.12, 5.12)
#' @param trace default = T; trace the process of BAS iteration.
#' @param seed random seed; default = NULL
#' @param pen penalty conefficient usually predefined as a large enough value, default 1e6
#'
#' @return A list including best beetle position ($par) and corresponding objective function value($value).
#' @references Wang T, Yang L, Liu Q. Beetle Swarm Optimization Algorithm:Theory and Application.2018. arXiv:1808.00206v1.\url{https://arxiv.org/abs/1808.00206}
#'
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
#' result<-
#' BSOoptim(fn = pressure_Vessel$obj,
#'          init = NULL,
#'          constr = pressure_Vessel$con,
#'          lower = c( 1, 1, 10, 10),
#'          upper = c(100, 100, 200, 200),
#'          n = 1000,
#'          w = c(0.9,0.4),
#'          w_vs = 0.9,
#'          step = 100,
#'          step_w = c(0.9,0.4),
#'          c = 35,
#'          v = c(-5.12,5.12),
#'          trace = F,seed = 1,
#'          pen = 1e6)
#'  result$par; result$value
#' # >>>> example without constraint: Michalewicz function <<<<
#' mich <- function(x){
#' y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
#' y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
#' return(y1+y2)
#' }
#' result <-
#'  BSOoptim(fn = mich,
#'            init = NULL,
#'            lower = c(-6,0),
#'            upper = c(-1,2),
#'            n = 100,
#'            step = 5,
#'            s = 10,seed = 1, trace = F)
#' result$par; result$value
#' #======== examples end =======================
#' @export
#'
BSOoptim <- function(fn,
                     init = NULL,
                     constr = NULL,
                     lower = c(-50,-50),
                     upper = c(50,50),
                     n = 300,
                     s =floor(10+2*sqrt(length(lower))),
                     w = c(0.9,0.4),
                     w_vs = 0.4,
                     step = 10,
                     step_w = c(0.9,0.4),
                     c = 8,
                     v = c(-5.12,5.12),
                     trace = T,
                     seed = NULL,
                     pen = 1e6){
  # the second for loop makes code slowly
  if(!is.null(seed)){
     set.seed(seed)
  }
  # function -----------------------------------------------------
  ustep<- function(x){
    result <- sapply(x,function(x) ifelse(x > 0,1,0))
    return(result)
  }

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
  # parms init ---------------------------------------------------------
  # max generation and popluation size
  maxit <- n
  sizepop <- s

  # velocity ranges is [vmin, vmax]
  vmax <- v[2]
  vmin <- v[1]

  #upper <- matrix(rep(upper,s),nrow = s,byrow = T)
  #lower <- matrix(rep(lower,s),nrow = s,byrow = T)

  # weights in PSO
  wmax <- w[1]
  wmin <- w[2]

  #step updates weights
  step_w0 <- step_w[1]
  step_w1 <- step_w[2]

  k <- w_vs
  npar <- length(lower)

  # matrix init ----------------------------------------------------
  if(is.null(init)){
    # use apply instead of for loop
    pop <- apply(rbind(lower,upper),2,
                 function(x){runif(min = x[1],max = x[2], n = sizepop)})
  }else{
    pop <- init
  }

  #use uniform distribution to initialize the velocity
  V <- matrix(runif(sizepop*npar,-1,1),
              nrow = sizepop, ncol = npar, byrow = F) * 0.5
  fitness <- apply(pop,1,fnew)

  #best of individual and population
  bestfitness <- max(fitness)
  bestindex <- which(fitness == max(fitness))

  zbest_position <- pop[bestindex,] #position of best population
  gbest_position <- pop #position of best historical individual

  fitnessgbest <- fitness
  fitnesszbest <- bestfitness

  #distance of step influence in BAS algorithm
  Y <- matrix(0, nrow = sizepop, ncol = npar)
  # record fitness for every beetles in each iterations
  #fitnesstable <- matrix(0, nrow = maxit, ncol = sizepop)

  # record position for every beetles in each iterations
  #X <- array(data = 0, dim = c(sizepop,npar, maxit))

  # best fitness in each iterations
  ybest <- rep(0,maxit)
  xbest <- matrix(0,nrow = maxit,ncol = npar)
  # for loop ---------------------------------------------------------------
  for (i in 1:maxit){
    c1 <- 1.3 + 1.2 * cos(i*pi)/maxit #1.3
    c2 <- 2 - 1.2*cos(i*pi)/maxit

    d0 <- step/c
    w <- wmax - (wmax - wmin)*(i/maxit) #decreas linearly

    for(j in 1:sizepop){

      # NOTE: the position of left/right are based on VELOCITY
      xleft <- pop[j,] + V[j,]*d0/2
      fleft <- fnew(xleft)
      xright <- pop[j,] - V[j,]*d0/2
      fright <- fnew(xright)

      Y[j,] <- step*V[j,]*sign(fleft - fright) #distance of BAS step

      #velocity updates
      V[j,] <- w*V[j,] +
        c1 *runif(1)*(gbest_position[j,] - pop[j,]) +
        c2*runif(1)*(zbest_position - pop[j,])
      V[j,which(V[j,] > vmax)] <- vmax
      V[j,which(V[j,] < vmin)] <- vmin

      #position/population updates
      pop[j,] <- pop[j,] + k*V[j,]+(1-k)*Y[j,]
      pop[j,] <- handle.bounds(pop[j,])

      fitness[j] <- fnew(pop[j,])

      if(fitness[j] < fitnessgbest[j]){
        gbest_position[j,] <- pop[j,]
        fitnessgbest[j] <- fitness[j]
      }

      if(fitness[j] < fitnesszbest){
        zbest_position <- pop[j,]
        fitnesszbest <- fitness[j]
      }
    }

    #fitnesstable[i,] <- fitness
    #X[,,i] <- pop

    ybest[i] <- fitnesszbest
    xbest[i,] <- zbest_position
    eta <- step_w1*(step_w0/step_w1)^(1/(1+10*i/maxit))
    step <- eta*step
    if(trace){
      cat('iter',i,'par: [',zbest_position,']','fitness: ',fitnesszbest,'\n')
    }

  }

  return(list(par = zbest_position,
              value = fitnesszbest,
              df = list(x = xbest,
                        y = ybest)))
}


