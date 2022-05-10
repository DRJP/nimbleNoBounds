##' Log transformed gamma distribution.
##'
##' \code{dLogGamma} and \code{rLogGamma} provide a log-transformed gamma distribution.
##'
##' @name dLogGamma

##' @param x A continuous random variable on the real line. Let y=exp(x). Then y ~ dgamma(shape,scale).
##' @param shape Shape parameter of y ~ dgamma(shape,scale).
##' @param scale Scale parameter of y ~ dgamma(shape,scale).
##' @param log Logical flag to toggle returning the log density.
##' @param n Number of random variables. Currently limited to 1, as is common in nimble. See ?replicate for an alternative.
##' @import nimble
##' @importFrom stats rgamma dgamma
##' @return The density or log density of x, such that x = log(y) and y ~ dgamma(shape,scale).
##' @author David R.J. Pleydell
##' @examples
##'
##' ## Create a gamma random variable, and transform it to the log scale
##' n      = 100000
##' shape = 2
##' scale = 2
##' y      = rgamma(n=n, shape=shape, scale=scale)
##' x      = log(y)
##'
##' ## Plot histograms of the two random variables
##' par(mfrow=n2mfrow(2))
##' ## Plot 1
##' hist(x, n=100, freq=FALSE)
##' curve(dLogGamma(x, shape=shape, scale=scale), -4, 5, n=1001, col="red", add=TRUE, lwd=3)
##' ## Plot 2: back-transformed
##' xNew = replicate(n=n, rLogGamma(n=1, shape=shape, scale=scale))
##' yNew   = exp(xNew)
##' hist(yNew, n=100, freq=FALSE, xlab="exp(x)")
##' curve(dgamma(x, shape=shape, scale=scale), 0, 100, n=1001, col="red", lwd=3, add=TRUE)
##'
##' ## Create a NIMBLE model that uses this distribution
##' code = nimbleCode({
##'   log(y)  ~ dLogGamma(shape=shape, scale=scale)
##'   log(y2) ~ dLogGamma(shape=shape, rate=1/scale)
##'   log(y3) ~ dLogGamma(mean=shape*scale, sd=scale * sqrt(shape))
##' })
##'
##' \dontrun{
##' ## Build & compile the model
##' const = list (shape=shape, scale=scale)
##' modelR = nimbleModel(code=code, const=const)
##' simulate(modelR)
##' modelC = compileNimble(modelR)
##'
##' ## Configure, build and compile an MCMC
##' conf  = configureMCMC(modelC, monitors2=c("y", "y2", "y3"))
##' mcmc  = buildMCMC(conf=conf)
##' cMcmc = compileNimble(mcmc)
##'
##' ## Run the MCMC & extract samples
##' samps = runMCMC(mcmc=cMcmc, niter=50000)
##' x  = as.vector(samps[[1]][,"log_y"])
##' x2 = as.vector(samps[[1]][,"log_y2"])
##' x3 = as.vector(samps[[1]][,"log_y3"])
##' y  = as.vector(samps[[2]][,"y"])
##' y2 = as.vector(samps[[2]][,"y2"])
##' y3 = as.vector(samps[[2]][,"y3"])
##'
##' ## Plot MCMC output
##' par(mfrow=n2mfrow(4))
##' ## Plot 1: MCMC trajectory
##' plot(x, typ="l")
##' ## Plot 2: taget density on unbounded sampling scale
##' hist(x, n=100, freq=FALSE)
##' curve(dLogGamma(x, shape=shape, scale=scale), -4, 3, n=1001, col="red", lwd=3, add=TRUE)
##' ## Plot 3: taget density on bounded scale
##' hist(y, n=100, freq=FALSE)
##' curve(dgamma(x, shape=shape, scale=scale), 0, 25, n=1001, col="red", lwd=3, add=TRUE)
##' ## Plot 4: different parameterisations
##' nBreaks=51
##' xLims = range(pretty(range(samps[[1]])))
##' hist(x, breaks=seq(xLims[1], xLims[2], l=nBreaks), col=rgb(1, 0, 0, 0.1))
##' hist(x2, breaks=seq(xLims[1], xLims[2], l=nBreaks), col=rgb(0, 1, 0, 0.1), add=TRUE)
##' hist(x3, breaks=seq(xLims[1], xLims[2], l=nBreaks), col=rgb(0, 0, 1, 0.1), add=TRUE)
##' }
##'


NULL


#' @rdname dLogGamma
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

#' @rdname dLogGamma
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
