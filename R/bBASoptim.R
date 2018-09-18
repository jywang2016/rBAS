#' Implementation of the binary BAS(bBAS) algorithm for optimization problems.
#'
#' @description bBAS is designed to solve the binary-integer-programming. It can also be employed on general
#' optimization problems.
#' @param fn objective function; function need to be optimized
#' @param init default = NULL, it will generate randomly; Of course, you can specify it. It should be noted
#' that you'd better specify an integer vector as init when dealing with 0-1 problem. If you don't mind the
#' the init vector is integer or double(numerical), just leave it alone.
#' @param lower lower of parameters to be estimated; Default = c(-6,0) because of the test on
#' Michalewicz function of which thelower is c(-6,0);
#' @param upper upper of parameters; Default = c(-1,2).
#' @param d0 a constant to gurantee that sensing length of antennae \emph{d} doesn't equal to
#' zero. More specifically, \deqn{d^t = \eta_d * d^{t-1} + d_0}where attenuation coefficient
#'  \eqn{\eta_d} belongs to \eqn{[0,1]}
#' @param d1 initial value of antenae length. You can specify it according to your problem scale
#' @param eta_d attenuation coefficient of sensing length of antennae
#' @param w the inertia term
#' @param c a constant belongs to (0,1).\deqn{V_i=wV_i \pm c*rand}
#' @param vmax maximum speed of beetle
#' @param n iterations times
#' @param resolution If there are non-integer parameters in the optimization problem, resolution should be taken into consideration.
#' You can use resolution parms to reduce the error generated in the process of translating the double(decimal) to binary. More specifically,
#' you can set resolution as c(1,1,1,1,1) when you deal with lot-sizing problem. But you should make the resolution large enough when
#' dealing with Michalewicz function. If the parameter belongs to [-2.048,2.048], bBAS will search binary number in [0, 4096] and translate
#' it into decimal when you set resolution as 1000. The examples below can be referenced.
#' @param seed random seed; default = NULL ; The results of BAS depend on random init value and random directions.
#' Therefore, if you set a random seed, for example,\code{seed = 1}, the results will remain the same
#' no matter how many times you repeat your experiments.
#' @param trace default = 20; it means the process is printed to the console every 10 iterations.
#' @references The algorithm is developed by Miss Ruan Yue. The documents or paper about bBAS will come soon.
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
#' fit <- bBASoptim(fn = mich,
#'                  init = c(-3,1),
#'                  resolution = rep(100,2),
#'                  trace = 20,
#'                  c = 0.6,
#'                  seed = 3)
#' fit$par;fit$value
#'
#' #==============lot-sizing problem==============#
#' lot_size2 <- function(x){
#'   R = c(100,60,40,50,80)
#'   A = 100
#'   c = 1
#'   x1 = 1 - x
#'
#'   I = rep(0,5)
#'
#'   for(m in 1:4){
#'     t = 0
#'     for (p in (m+1):5){
#'       if(x1[p] == 1){
#'         t = t + R[p]
#'       }
#'       else{break}
#'     }
#'     I[m] = t
#'   }
#'   if(x[1]!=1){
#'     pen = 1e5
#'   }else{
#'     pen = 0
#'   }
#'   cost = sum(A*x) + sum(c*I) + pen
#'
#'   return(cost)
#' }
#' fit <- bBASoptim(fn = lot_size2,
#'                  init = rep(1,5),
#'                  lower = rep(0,5),
#'                  upper = rep(1,5),
#'                  resolution = rep(1,5),
#'                  n = 200)
#' fit$par;fit$value
#' #======== examples end =======================
#' @importFrom stats runif
#' @export
bBASoptim <- function(fn,init = NULL,
                      lower = c(-6,0),
                      upper = c(-1,2),
                      d0 = 1e-30,
                      d1 = 3,
                      eta_d = 0.99,
                      w = 0.2,
                      c = 0.5,
                      n = 800,
                      vmax = 4,
                      seed = NULL,
                      trace = 20,
                      resolution = rep(1,length(lower))){

  if(!is.null(seed)){
    set.seed(seed)
  }

  d <- d1
  npar <- length(lower)

  resolu <- (upper - lower) * resolution
  bits <- floor(log(resolu)/log(2))+1

  bit_index <- matrix(1,nrow = npar, ncol = 2)
  bit_index[1,2] <- bits[1]

  # get the index of x1 ~ xn (binary )
  if(npar > 1){
    for(i in 2:npar){
      bit_index[i,1] <- bit_index[i-1,2]+1
      bit_index[i,2] <- sum(bits[1:i])
    }
  }

  upb <- 2^bits-1  #the up bound of binary
  lowb <- rep(0,length(lower)) #the low bound of binary

  # binary ===>> decimal
  fnbin <- function(x){
    if(is.list(x)){
      xbin <- strtoi(sapply(x,paste0,collapse = ''),2)
    }else{
      xbin <- NULL
      for(i in 1:npar){
        temp <- strtoi(paste0(x[bit_index[i,1]:bit_index[i,2]],collapse = ''),2)
        xbin <- c(xbin,temp)
      }
    }
    real_x <-  xbin/(upb - lowb)*(upper - lower) + lower
    real_y <- fn(real_x)
    return(list(x = real_x,
                y = real_y))
  }

  if(is.null(init)){
    init <- runif(npar,min = lower, max = upper)
  }

  init2temp <- (init-lower)/(upper-lower)*(upb-lowb) + lowb
  initBin <- sapply(init2temp,Dec2Bin)

  if(is.matrix(initBin)){
    temp <- t(initBin)
    initBin <- list()
    for(i in 1:npar){
      initBin[[i]] <- temp[i,]
    }
  }
  # the length of vectors in the list are decided by vector bits
  x <- list()
  xvec <- NULL
  for(i in 1:npar){
    x[[i]] <- c(rep(0,bits[i]-length(initBin[[i]])),initBin[[i]])
    xvec <- c(xvec,x[[i]])
  }

  xbest <- xvec
  xbestp <- init
  fbest <- fn(init)
  xstore <- xbestp
  fbest_store <- fbest
  gbest <- fbest

  cat('Iters 0','xbest = [',xbestp,'], fbest =',fbest,'\n')

  vel <- runif(sum(bits)) - 0.5
  one_vel <- runif(sum(bits)) - 0.5
  zero_vel <- runif(sum(bits)) - 0.5

  for(iter in 1:n){

    fx <- fnbin(xvec)
    xreal <- fx$x
    fx <- fx$y

    dir <- matrix(runif(n = sum(bits)),nrow = 1)

    xleft <- xvec + d*dir
    xleft <- round(xleft)
    xleft[which(xleft>1)] = 1
    fleft <- fnbin(x = xleft)
    xlreal <- fleft$x
    fleft <- fleft$y

    xright <- xvec - d*dir
    xright <- round(xright)
    xright[which(xright<0)] <- 0
    fright <- fnbin(x = xright)
    xrreal <- fright$x
    fright <- fright$y

    d <- d1 * eta_d + d0

    if(fx < fbest){
      xbest<-xvec
      xbestp <- xreal #real xbest for print
      fbest<-fx
    }

    if(fleft < fbest){
      xbest<-xleft
      xbestp <- xlreal
      fbest<-fleft
    }

    if(fright < fbest){
      xbest<-xright
      xbestp <- xrreal
      fbest<-fright
    }

    oneadd  <- rep(0,sum(bits))
    zeroadd <- rep(0,sum(bits))
    c3 <- c * runif(1)

    index <- which(xbest == 0)
    oneadd[index] <- oneadd[index] - c3
    zeroadd[index] <- zeroadd[index] + c3
    oneadd[-index] <- oneadd[-index] + c3
    zeroadd[-index] <- zeroadd[-index] - c3

    one_vel <- w*one_vel + oneadd
    zero_vel <- w*zero_vel + zeroadd

    index_v <- which(abs(vel)>vmax)
    zero_vel[index_v] <- vmax*sign(zero_vel[index_v])
    one_vel[index_v] <- vmax*sign(one_vel[index_v])

    index_x <- which(xvec==1)
    vel[index_x] <- zero_vel[index_x]
    vel[-index_x] <- one_vel[-index_x]

    veln <- 1/(1+exp(-vel))

    temp <- runif(sum(bits))

    index_veln <- which((temp - veln) <= 0)

    xvec[index_veln] <- !xvec[index_veln]

    xstore <- c(xstore,xbestp)
    fbest_store <- c(fbest_store, fbest)

    if(trace & iter %% trace == 0){
      cat('Iters',iter,'xbest = [',xbestp,'], fbest =',fbest,'\n')
    }

    if(gbest > fbest){
      gbest <- fbest
    }
  }

  xstore <- matrix(xstore, ncol = npar,byrow = T)

  result <- list(par = xbestp,
                 value = gbest,
                 df = list(x = xstore,y = fbest_store))
  return(result)
}

# decimal to binary
Dec2Bin <- function(x){

  x <- as.integer(x)

  if(x!=0){
    temp <- paste(as.integer(rev(intToBits(x))),collapse = '')
    index_start <- regexpr('1',temp)[[1]]
    index_end <- nchar(temp)
    charBin <- substr(temp,index_start,index_end)
    vectBin <- as.numeric(unlist(strsplit(charBin,split = '')))
  }else{
    vectBin <- 0
  }

  return(vectBin)
}
