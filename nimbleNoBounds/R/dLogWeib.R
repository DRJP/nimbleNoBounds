##' Log transformed Weibull distribution.
##'
##' \code{dLogWeib} and \code{rLogWeib} provide a log-transformed Weibull distribution.
##'
##' @name dLogWeib

##' @param x A continuous random variable on the real line, where y=exp(x) and y ~ dweib(shape,scale).
##' @param shape Shape parameter of y ~ dweib(shape,scale).
##' @param scale Scale parameter of y ~ dweib(shape,scale).
##' @param log Logical flag to toggle returning the log density.
##' @param n Number of random variables. Currently limited to 1, as is common in nimble. See ?replicate for an alternative.
##' @import nimble
##' @importFrom stats runif
##' @return The density or log density of x, such that x = log(y) and y ~ dweib(shape,scale).
##' @author David R.J. Pleydell
##' @examples
##'
##' n      = 100000
##' shape = 2
##' scale = 2
##' y      = rweibull(n=n, shape=shape, scale=scale)
##' x      = log(y)
##'
##' par(mfrow=n2mfrow(2))
##' ## Plot 1
##' hist(x, n=100, freq=FALSE, xlab="x = log(y)", main="Histogram of log-transformed random numbers from rweibull.")
##' curve(dLogWeib(x, shape=shape, scale=scale), -6, 3, n=1001, col="red", add=TRUE, lwd=3)
##' ## Plot 2: back-transformed
##' xNew = replicate(n=n, rLogWeib(n=1, shape=shape, scale=scale))
##' yNew = exp(xNew)
##' hist(yNew, n=100, freq=FALSE, xlab="y = exp(x)", main="Histogram of random numbers from rLogWeib.")
##' curve(dweibull(x, shape=shape, scale=scale), 0, 100, n=1001, col="red", lwd=3, add=TRUE)
##'
##' code = nimbleCode({
##'   log(y) ~ dLogWeib(shape=shape, scale=scale)
##' })
##' const = list (shape=shape, scale=scale)
##' modelR = nimbleModel(code=code, const=const)
##' simulate(modelR)
##' modelC = compileNimble(modelR)
##' conf  = configureMCMC(modelC)
##' mcmc  = buildMCMC(conf=conf)
##' cMcmc = compileNimble(mcmc)
##' x = as.vector(runMCMC(mcmc=cMcmc, niter=50000))
##' y = exp(x)
##' par(mfrow=n2mfrow(3))
##' ## Plot 1: MCMC trajectory
##' plot(x, typ="l")
##' ## Plot 2: taget density on unbounded sampling scale
##' hist(x, n=100, freq=FALSE, main="Histogram of MCMC samples", xlab="x = log(y)")
##' curve(dLogWeib(x, shape=shape, scale=scale), -5, 3, n=1001, col="red", lwd=3, add=TRUE)
##' ## Plot 3: taget density on bounded scale
##' hist(exp(x), n=100, freq=FALSE, main="Histogram of back-transformed MCMC samples", xlab="y = exp(x)")
##' curve(dweibull(x, shape=shape, scale=scale), 0, 8, n=1001, col="red", lwd=3, add=TRUE)

NULL


#' @rdname dLogWeib
#' @export
dLogWeib <- nimbleFunction (
  run = function(x     = double(0),
                 shape = double(0, default=1.0),
                 scale = double(0, default=1.0),
                 log   = integer(0, default=0)) {
    ## Returns density of x where
    ##                    y ~ Weibull(shape,scale)
    ##                    x = log(y)
    returnType(double(0))
    y = exp(x)
    logProbX = log(shape) - log(scale) + (shape-1)*log(y/scale) - (y/scale)^shape + x
    if (log)
      return(logProbX)
    return(exp(logProbX))
  }
)

#' @rdname dLogWeib
#' @export
rLogWeib <- nimbleFunction (
  run = function(n     = integer(0, default=1),
                 shape = double(0, default=1.0),
                 scale = double(0, default=1.0)) {
    ## Generates y ~ Weibull(shape, scale)
    ## Returns   x = log(y)
    returnType(double(0))
    if(n != 1)
      nimPrint("Warning: rLogWeib only allows n = 1; Using n = 1.\n")
    u <- runif(n=1)
    y <- scale*(-log(1-u))^(1/shape) ## Weibull random variable
    x <- log(y)
    return(x)
  }
)
