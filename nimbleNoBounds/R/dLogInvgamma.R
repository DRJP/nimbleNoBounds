##' Log transformed inverse-gamma distribution.
##'
##' \code{dLogInvgamma} and \code{rLogInvgamma} provide a log-transformed inverse gamma distribution.
##'
##' @name dLogInvgamma

##' @param x A continuous random varaible on the real line. Let y=exp(x). Then y ~ dinvgamma(shape,scale).
##' @param shape Shape parameter of y ~ dinvgamma(shape,scale).
##' @param scale Scale parameter of y ~ dinvgamma(shape,scale).
##' @param log Logical flag to toggle returning the log density.
##' @param n Number of random variables. Currently limited to 1, as is common in nimble. See ?replicate for an alternative.
##' @import nimble
##' @importFrom stats rgamma runif dgamma
##'
##' @return The density or log density of x, such that x = log(y) and y ~ dinvgamma(shape,scale).
##' @author David R.J. Pleydell
##' @examples
##'
##' n      = 100000
##' shape = 2.5
##' scale = 0.01
##' y      = rinvgamma(n=n, shape=shape, scale=scale)
##' x      = log(y)
##'
##' par(mfrow=n2mfrow(4))
##' ## Plot 1
##' hist(y, n=100, freq=FALSE, xlab="y")
##' curve(dinvgamma(x, shape=shape, scale=scale), 0, 1.0, n=5001, col="red", add=TRUE, lwd=2)
##' ## Plot 2
##' hist(x, n=100, freq=FALSE)
##' curve(dLogInvgamma(x, shape=shape, scale=scale), -8, 1, n=1001, col="red", add=TRUE, lwd=3)
##' ## Plot 3: back-transformed
##' z = rgamma(n=n, shape=shape, scale=1/scale)
##' hist(1/z, n=100, freq=FALSE, xlab="y")
##' curve(dinvgamma(x, shape=shape, scale=scale), 0, 1, n=5001, col="red", lwd=3, add=TRUE)
##' ## Plot 4: back-transformed
##' xNew = replicate(n=n, rLogInvgamma(n=1, shape=shape, scale=scale)) ## yNew = replicate(n=n, rinvgamma(n=1, shape=shape, scale=scale))
##' yNew   = exp(xNew)
##' hist(yNew, n=100, freq=FALSE, xlab="exp(x)")
##' curve(dinvgamma(x, shape=shape, scale=scale), 0, 1, n=5001, col="red", lwd=3, add=TRUE)
##'
##' code = nimbleCode({
##'   log(y) ~ dLogInvgamma(shape=shape, scale=scale)
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
##' hist(x, n=100, freq=FALSE)
##' curve(dLogInvgamma(x, shape=shape, scale=scale), -10, 1, n=1001, col="red", lwd=3, add=TRUE)
##' ## Plot 3: taget density on bounded scale
##' curve(dinvgamma(x, shape=shape, scale=scale), xlab="y = exp(x)", 0, 0.5, n=1001, col="red", lwd=3)
##' hist(exp(x), n=100, freq=FALSE, add=TRUE)
##' curve(dinvgamma(x, shape=shape, scale=scale), 0, 0.5, n=1001, col="red", add=TRUE, lwd=3)

NULL

#' @rdname dLogInvgamma
#' @export
dLogInvgamma <- nimbleFunction (
  run = function(x     = double(0),
                 shape = double(0),
                 scale = double(0, default=1.0),
                 log   = integer(0, default=0)) {
    ## Returns density of x where
    ##                    y ~ Inv-Gamma(shape,scale)
    ##                    x = log(y)
    returnType(double(0))
    y = exp(x)
    # logProbX = shape*log(scale) - lgamma(shape) - (shape+1)*log(y) - (scale/y) + x
    logProbX = dinvgamma(y, shape=shape, scale=scale, log=TRUE) + x
    if (log)
      return(logProbX)
    return(exp(logProbX))
  }
)

#' @rdname dLogInvgamma
#' @export
rLogInvgamma <- nimbleFunction (
  run = function(n     = integer(0, default=1),
                 shape = double(0),
                 scale = double(0, default=1.0)) {
    ## Generates y ~ Inv-Gamma(shape, scale)
    ## Returns   x = log(y)
    returnType(double(0))
    if(n != 1)
      nimPrint("Warning: rLogInvgamma only allows n = 1; Using n = 1.\n")
    y <- rgamma(n=1, shape=shape, scale=1/scale)
    x <- log(1/y)
    return(x)
  }
)
