##' Log transformed log-normal distribution.
##'
##' \code{dLogLnorm} and \code{rLogLnorm} provide a log-transformed log-normal distribution.
##'
##' @name dLogLnorm
##'
##' @param x A continuous random variable on the real line. Let y=exp(x). Then y ~ dlnorm(meanlog, sdlog).
##' @param meanlog mean of the distribution on the log scale with default values of ‘0’.
##' @param sdlog standard deviation of the distribution on the log scale with default values of ‘1’.
##' @param log Logical flag to toggle returning the log density.
##' @param n Number of random variables. Currently limited to 1, as is common in nimble. See ?replicate for an alternative.
##' @import nimble
##' @importFrom stats rnorm dnorm
##' @return The density or log density of x, such that x = log(y) and y ~ dlnorm(meanlog,sdlog).
##' @author David R.J. Pleydell
##' @examples
##'
##' ## Create a log-normal random variable, and transform it to the log scale
##' n       = 100000
##' meanlog = -3
##' sdlog   = 0.1
##' y       = rlnorm(n=n, meanlog=meanlog, sdlog=sdlog)
##' x       = log(y)
##'
##' ## Plot histograms of the two random variables
##' oldpar <- par()
##' par(mfrow=n2mfrow(2))
##' ## Plot 1
##' hist(x, n=100, freq=FALSE)
##' curve(dLogLnorm(x, meanlog=meanlog, sdlog=sdlog), -4, -2, n=1001, col="red", add=TRUE, lwd=3)
##' ## Plot 2: back-transformed
##' xNew = replicate(n=n, rLogLnorm(n=1, meanlog=meanlog, sdlog=sdlog))
##' yNew   = exp(xNew)
##' hist(yNew, n=100, freq=FALSE, xlab="exp(x)")
##' curve(dlnorm(x, meanlog=meanlog, sdlog=sdlog), 0, 0.1, n=1001, col="red", lwd=3, add=TRUE)
##' par(oldpar)
##'
##' ## Create a NIMBLE model that uses this transformed distribution
##' code = nimbleCode({
##'   log(y) ~ dLogLnorm(meanlog=meanlog, sdlog=sdlog)
##' })
##'
##' \donttest{
##' ## Build & compile the model
##' const = list (meanlog=meanlog, sdlog=sdlog)
##' modelR = nimbleModel(code=code, const=const)
##' simulate(modelR)
##' modelC = compileNimble(modelR)
##'
##' ## Configure, build and compile an MCMC
##' conf  = configureMCMC(modelC)
##' mcmc  = buildMCMC(conf=conf)
##' cMcmc = compileNimble(mcmc)
##'
##' ## Run the MCMC
##' x = as.vector(runMCMC(mcmc=cMcmc, niter=50000))
##' y = exp(x)
##'
##' ## Plot MCMC output
##' oldpar <- par()
##' par(mfrow=n2mfrow(3))
##' ## Plot 1: MCMC trajectory
##' plot(x, typ="l")
##' ## Plot 2: taget density on unbounded sampling scale
##' hist(x, n=100, freq=FALSE)
##' curve(dLogLnorm(x, meanlog=meanlog, sdlog=sdlog), -4, -2, n=1001, col="red", lwd=3, add=TRUE)
##' ## Plot 3: taget density on bounded scale
##' hist(y, n=100, freq=FALSE)
##' curve(dlnorm(x, meanlog=meanlog, sdlog=sdlog), 0, 0.1, n=1001, col="red", lwd=3, add=TRUE)
##' par(oldpar)
##' }


NULL

#' @rdname dLogLnorm
#' @export
dLogLnorm <- nimbleFunction (
  run = function(x       = double(0),
                 meanlog = double(0, default=0.0),
                 sdlog   = double(0, default=1.0),
                 log     = integer(0, default=0)) {
    ## Returns density of x where
    ##                    y ~ Lnorm(meanlog,sdlog)
    ##                    x = log(y)
    returnType(double(0))
    y = exp(x)
    logProbX = dnorm(x, mean=meanlog, sd=sdlog, log=log)
    return(logProbX)
  }
)

#' @rdname dLogLnorm
#' @export
rLogLnorm <- nimbleFunction (
  run = function(n       = integer(0, default=1),
                 meanlog = double(0, default=0.0),
                 sdlog   = double(0, default=1.0)) {
    ## Generates y ~ Lnorm(meanlog, sdlog)
    ## Returns   x = log(y)
    returnType(double(0))
    if(n != 1)
      nimPrint("Warning: rLogLnorm only allows n = 1; Using n = 1.\n")
    x <- rnorm(n=1, mean=meanlog, sd=sdlog)
    return(x)
  }
)
