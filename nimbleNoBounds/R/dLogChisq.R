##' Log transformed chi-squared distribution.
##'
##' \code{dLogChisq} and \code{rLogChisq} provide a log-transformed chi-squared distribution.
##'
##' @name dLogChisq
##'
##' @param x A continuous random varaible on the real line, where y=exp(x) and y ~ dchsq(df).
##' @param df df parameter of y ~ dchisq(df).
##' @param log Logical flag to toggle returning the log density.
##' @param n Number of random variables. Currently limited to 1, as is common in nimble. See ?replicate for an alternative.
##' @import nimble
##' @importFrom stats rchisq runif dchisq
##' @return The density or log density of x, such that x = log(y) and y ~ dchisq(df).
##' @author David R.J. Pleydell
##' @examples
##'
##' n  = 100000
##' df = 3
##' y  = rchisq(n=n, df=df)
##' x  = log(y)
##'
##' par(mfrow=n2mfrow(2))
##' ## Plot 1
##' hist(x, n=100, freq=FALSE, main="", xlab= "x = log(y)")
##' curve(dLogChisq(x, df=df), -7, 4, n=1001, col="red", add=TRUE, lwd=3)
##' ## Plot 2: back-transformed
##' xNew = replicate(n=n, rLogChisq(n=1, df=df))
##' yNew = exp(xNew)
##' hist(yNew, n=100, freq=FALSE, xlab="y = exp(x)", main="")
##' curve(dchisq(x, df=df), 0, 30, n=1001, col="red", lwd=3, add=TRUE)
##'
##' code = nimbleCode({
##'   x ~ dLogChisq(df = 0.5)
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
##' hist(x, n=100, freq=FALSE, main="Histogram of MCMC samples", xlab="x = log(y)")
##' curve(dLogChisq(x, df=0.5), -55, 5, n=1001, col="red", lwd=3, add=TRUE)
##' ## Plot 3: taget density on bounded scale
##' hist(exp(x), n=100, freq=FALSE, main="Histogram of back-transformed MCMC samples", xlab="y = exp(x)")
##' curve(dchisq(x, df=0.5), 0, 20, n=1001, col="red", lwd=3, add=TRUE)

NULL

#' @rdname dLogChisq
#' @export
dLogChisq <- nimbleFunction (
  run = function(x = double(0),
                 df = double(0, default=1.0),
                 log = integer(0, default=0)) {
    ## Returns density of x where
    ##                    y ~ Chisq(df)
    ##                    x = log(y)
    returnType(double(0))
    y = exp(x)
    logProbX = dchisq(y, df=df, log=TRUE) + x # change of variable
    if (log)
      return(logProbX)
    return(exp(logProbX))
  }
)

#' @rdname dLogChisq
#' @export
rLogChisq <- nimbleFunction (
  run = function(n = integer(0, default=1),
                 df = double(0, default=1.0)) {
    ## Genedfs y ~ Chisq(df)
    ## Returns x = log(y)
    returnType(double(0))
    if(n != 1)
      nimPrint("Warning: rLogChisq only allows n = 1; Using n = 1.\n")
    y <- rchisq(1, df=df)
    x <- log(y)
    return(x)
  }
)