##' Log transformed exponential distribution.
##'
##' \code{dLogExp} and \code{rLogExp} provide a log-transformed exponential distribution.
##'
##' @name dLogExp

##' @param x A continuous random varaible on the real line. Let y=exp(x). Then y ~ dexp(rate).
##' @param rate Rate parameter of y ~ dexp(rate).
##' @param log Logical flag to toggle returning the log density.
##' @param n Number of random variables. Currently limited to 1, as is common in nimble. See ?replicate for an alternative.
##' @import nimble
##' @importFrom stats rexp runif dexp
##' @return The density or log density of x, such that x = log(y) and y ~ dexp(rate).
##' @author David R.J. Pleydell
##' @examples
##'
##' n      = 100000
##' lambda = 3
##' y      = rexp(n=n, rate=lambda)
##' x      = log(y)
##'
##' par(mfrow=n2mfrow(2))
##' ## Plot 1
##' hist(x, n=100, freq=FALSE)
##' curve(dLogExp(x, rate=lambda), -15, 4, n=1001, col="red", add=TRUE, lty=3)
##' ## Plot 2: back-transformed
##' xNew = replicate(n=n, rLogExp(n=1, rate=lambda))
##' yNew   = exp(xNew)
##' hist(yNew, n=100, freq=FALSE, xlab="exp(x)")
##' curve(dexp(x, rate=lambda), 0, 4, n=1001, col="red", lwd=3, add=TRUE)
##'
##' code = nimbleCode({
##'   x ~ dLogExp(rate = 0.5)
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
##' ## Plot 2: taget density on unbounded sampling scale
##' hist(x, n=100, freq=FALSE)
##' curve(dLogExp(x, rate=0.5), -15, 5, n=1001, col="red", lwd=3, add=TRUE)
##' ## Plot 3: taget density on bounded scale
##' hist(exp(x), n=100, freq=FALSE)
##' curve(dexp(x, rate=0.5), 0, 25, n=1001, col="red", lwd=3, add=TRUE)

NULL

#' @rdname dLogExp
#' @export
dLogExp <- nimbleFunction (
  run = function(x = double(0),
                 rate = double(0, default=1.0),
                 log = integer(0, default=0)) {
    ## Returns density of x where
    ##                    y ~ Exp(r)
    ##                    x = log(y)
    returnType(double(0))
    y = exp(x)
    logProbX = log(rate) - (rate*y) + x # Via change of variable
    if (log)
      return(logProbX)
    return(exp(logProbX))
  }
)

#' @rdname dLogExp
#' @export
rLogExp <- nimbleFunction (
  run = function(n = integer(0, default=1),
                 rate = double(0, default=1.0)) {
    ## Generates y ~ Exp(r)
    ## Returns   x = log(y)
    returnType(double(0))
    if(n != 1)
      nimPrint("Warning: rLogExp only allows n = 1; Using n = 1.\n")
    y <- rexp(1, rate=rate)
    x <- log(y)
    return(x)
  }
)
