##' Log transformed gamma distribution.
##'
##' \code{dLogGamma} and \code{rLogGamma} provide a log-transformed gamma distribution.
##'
##' @name dLogGamma

##' @param x A continuous random varaible on the real line. Let y=exp(x). Then y ~ dgamma(shape,scale).
##' @param shape Shape parameter of y ~ dgamma(shape,scale).
##' @param scale Scale parameter of y ~ dgamma(shape,scale).
##' @param log Logical flag to toggle returning the log density.
##' @param n Number of random variables. Currently limited to 1, as is common in nimble. See ?replicate for an alternative.
##' @import nimble
##' @importFrom stats rgamma runif dgamma
##' @return The density or log density of x, such that x = log(y) and y ~ dgamma(shape,scale).
##' @author David R.J. Pleydell
##' @examples
##'
##' n      = 100000
##' shape = 3
##' scale = 6
##' y      = rgamma(n=n, shape=shape, scale=scale)
##' x      = log(y)
##'
##' par(mfrow=n2mfrow(2))
##' ## Plot 1
##' hist(x, n=100, freq=FALSE)
##' curve(dLogGamma(x, shape=shape, scale=scale), -15, 4, n=1001, col="red", add=TRUE, lty=3)
##' ## Plot 2: back-transformed
##' xNew = replicate(n=n, rLogGamma(n=1, shape=shape, scale=scale))
##' yNew   = exp(xNew)
##' hist(yNew, n=100, freq=FALSE, xlab="exp(x)")
##' curve(dexp(x, shape=shape, scale=scale), 0, 4, n=1001, col="red", lwd=3, add=TRUE)
##'
##' code = nimbleCode({
##'   x ~ dLogGamma(shape=shape, scale=scale)
##' })
##' modelR = nimbleModel(code=code)
##' modelC = compileNimble(modelR)
##' conf  = configureMCMC(modelC)
##' mcmc  = buildMCMC(conf=conf)
##' cMcmc = compileNimble(mcmc)
##' x = as.vector(runMCMC(mcmc=cMcmc, niter=50000))
##' par(mfrow=n2mfrow(3))
##' ## Plot 1: MCMC trajectory
##' plot(x, typ="l")
## Plot 2: taget density on unbounded sampling scale
##' hist(x, n=100, freq=FALSE)
##' curve(dLogGamma(x, shape=shape, scale=scale), -15, 5, n=1001, col="red", lwd=3, add=TRUE)
##' ## Plot 3: taget density on bounded scale
##' hist(exp(x), n=100, freq=FALSE)
##' curve(dexp(x, shape=shape, scale=scale), 0, 25, n=1001, col="red", lwd=3, add=TRUE)

NULL


#' @rdname dLogGamme
#' @export
dLogGamma <- nimbleFunction (
  run = function(x     = double(0),
                 shape = double(0, default=1.0),
                 scale = double(0, default=1.0),
                 log   = integer(0, default=0)) {
    ## Returns density of x where
    ##                    y ~ Gamma(shape,scale)
    ##                    x = log(y)
    returnType(double(0))
    y = exp(x)
    logProbX = shape * x - y/scale - lgamma(shape) - shape*log(scale) # + constant
    if (log)
      return(logProbX)
    return(exp(logProbX))
  }
)

#' @rdname dLogGamme
#' @export
rLogGamma <- nimbleFunction (
  run = function(n     = integer(0, default=1),
                 shape = double(0, default=1.0),
                 scale = double(0, default=1.0)) {
    ## Generates y ~ Gamma(shape, scale)
    ## Returns   x = log(y)
    returnType(double(0))
    if(n != 1)
      nimPrint("Warning: rLogGamma only allows n = 1; Using n = 1.\n")
    y <- rgamma(n=1, shape=shape, scale=scale)
    x <- log(y)
    return(x)
  }
)
