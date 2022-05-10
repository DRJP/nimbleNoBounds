##' Logit transformed beta distribution.
##'
##' Transformation of uniform distribution, via scaled-logit transform, to the real line.
##' @name dLogitUnif
##'
##' @param x A continuous random variable on the real line, where y=ilogit(x)*(max-min)+min and y ~ dunif(min, max).
##' @param min lower limit of the distribution.  Must be finite.
##' @param max upper limit of the distribution.  Must be finite.
##' @param log logical flag. Returns log-density if TRUE.
##' @param n Number of random variables. Currently limited to 1, as is common in nimble. See ?replicate for an alternative.
##'
##' @return density, or log-density, of uniform distribution transformed to real line via scaling and logit function.
##'
##' @author David R.J. Pleydell
##'
##' @examples
##'
##' ## Create a uniform random variable, and transform it to the log scale
##' n      = 100000
##' lower  = -5
##' upper  = 11
##' y      = runif(n=n, lower, upper)
##' x      = logit((y-lower)/(upper-lower))
##'
##' ## Plot histograms of the two random variables
##' par(mfrow=n2mfrow(2))
##' ## Plot 1
##' hist(x, n=100, freq=FALSE)
##' curve(dLogitUnif(x, lower, upper), -15, 15, n=1001, col="red", add=TRUE, lwd=3)
##' ## Plot 2: back-transformed
##' xNew = replicate(n=n, rLogitUnif(n=1, lower, upper))
##' yNew   = ilogit(xNew) * (upper-lower) + lower
##' hist(yNew, n=100, freq=FALSE, xlab="exp(x)")
##' curve(dunif(x, lower, upper), -15, 15, n=1001, col="red", lwd=3, add=TRUE)
##'
##' ## Create a NIMBLE model that uses this transformed distribution
##' code = nimbleCode({
##'   x ~ dLogitUnif(lower, upper)
##' })
##'
##' \dontrun{
##' ## Build & compile the model
##' const = list(lower=lower, upper=upper)
##' modelR = nimbleModel(code=code, const=const)
##' modelC = compileNimble(modelR)
##'
##' ## Configure, build and compile an MCMC
##' conf  = configureMCMC(modelC)
##' mcmc  = buildMCMC(conf=conf)
##' cMcmc = compileNimble(mcmc)
##'
##' ## Run the MCMC
##' x = as.vector(runMCMC(mcmc=cMcmc, niter=50000))
##'
##' ## Plot MCMC output
##' par(mfrow=n2mfrow(3))
##' ## Plot 1: MCMC trajectory
##' plot(x, typ="l")
##' ## Plot 2: taget density on unbounded sampling scale
##' hist(x, n=100, freq=FALSE, xlab="x = logit(y)")
##' curve(dLogitUnif(x, lower, upper), -15, 15, n=1001, col="red", lwd=3, add=TRUE)
##' ## Plot 3: taget density on bounded scale
##' hist(ilogit(x)*(upper-lower)+lower, n=100, freq=FALSE, xlab="y")
##' curve(dunif(x, lower, upper), -15, 15, n=1001, col="red", lwd=3, add=TRUE)
##' }

NULL

#' @rdname dLogitUnif
#' @export
dLogitUnif <- nimbleFunction (
  ## Returns density of x where
  ##                    y ~ Unif(min,max)
  ##                    x = logit(y)
  run = function(x = double(0),
                 min=double(0, default=0.0),
                 max=double(0, default=1.0),
                 log = integer(0, default=0)) {
    returnType(double(0))
    y = (max*exp(x) + min) / (1 + exp(x))
    logProbX = log(max - y) - log(max - min) - log(1+exp(-x))
    if (log)
      return(logProbX)
    return(exp(logProbX))
  }
)


#' @rdname dLogitUnif
#' @export
rLogitUnif <- nimbleFunction (
    ## Generates y ~ Unif(shape1,shape2)
    ## Returns   x = logit(y)
    run = function(n   = integer(0, default=1),
                   min = double(0, default=0.0),
                   max = double(0, default=1.0)) {
        returnType(double(0))
        if(n != 1)
            nimPrint("Warning: rLogitUnif only allows n = 1; Using n = 1.\n")
        y <- runif(1, 0, 1)
        x <- logit(y)
        return(x)
    }
)
