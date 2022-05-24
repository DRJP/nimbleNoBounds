##' Logit transformed beta distribution.
##'
##' Logit transformation of beta random variable to the real line.
##' @name dLogitBeta
##'
##' @param x A continuous random variable on the real line, where y=ilogit(x) and y ~ dbeta(shape1, shape2).
##' @param shape1 non-negative parameter of the Beta distribution.
##' @param shape2 non-negative parameter of the Beta distribution.
##' @param log    logical flag. Returns log-density if TRUE.
##' @param n Number of random variables. Currently limited to 1, as is common in nimble. See ?replicate for an alternative.
##'
##' @return density or log-density of beta distributions transformed to real line via logit function.
##'
##' @author David R.J. Pleydell
##' @importFrom stats dbeta rbeta

##'
##' @examples
##'
##' ## Create a beta random variable, and transform it to the logit scale
##' n      = 100000
##' sh1    = 1
##' sh2    = 11
##' y      = rbeta(n=n, sh1, sh2)
##' x      = logit(y)
##'
##' ## Plot histograms of the two random variables
##' oldpar <- par()
##' par(mfrow=n2mfrow(2))
##' ## Plot 1
##' hist(x, n=100, freq=FALSE)
##' curve(dLogitBeta(x, sh1, sh2), -15, 4, n=1001, col="red", add=TRUE, lwd=3)
##' ## Plot 2: back-transformed
##' xNew = replicate(n=n, rLogitBeta(n=1, sh1, sh2))
##' yNew   = ilogit(xNew)
##' hist(yNew, n=100, freq=FALSE, xlab="exp(x)")
##' curve(dbeta(x, sh1, sh2), 0, 1, n=1001, col="red", lwd=3, add=TRUE)
##' par(oldpar)
##'
##' ## Create a NIMBLE model that uses this transformed distribution
##' code = nimbleCode({
##'   x ~ dLogitBeta(sh1, sh2)
##' })
##'
##' \donttest{
##' ## Build & compile the model
##' const = list(sh1=sh1, sh2=sh2)
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
##' oldpar <- par()
##' par(mfrow=n2mfrow(3))
##' ## Plot 1: MCMC trajectory
##' plot(x, typ="l")
##' ## Plot 2: taget density on unbounded sampling scale
##' hist(x, n=100, freq=FALSE, xlab="x = logit(y)")
##' curve(dLogitBeta(x, sh1, sh2), -15, 5, n=1001, col="red", lwd=3, add=TRUE)
##' ## Plot 3: taget density on bounded scale
##' hist(ilogit(x), n=100, freq=FALSE, xlab="y")
##' curve(dbeta(x, sh1, sh2), 0, 1, n=1001, col="red", lwd=3, add=TRUE)
##' par(oldpar)
##' }

NULL

#' @rdname dLogitBeta
#' @export
dLogitBeta <- nimbleFunction (
    ## Returns density of x where
    ##                    y ~ Beta(shape1,shape2)
    ##                    x = logit(y)
    run = function(x = double(0),
                   shape1=double(0, default=1.0),
                   shape2=double(0, default=1.0),
                   log = integer(0, default=0)) {
        returnType(double(0))
        y = ilogit(x)
        logProbX = log(y) + log(1 - y) + dbeta(y, shape1=shape1, shape2=shape2, log=TRUE) ## Via change of variable
        if (log)
            return(logProbX)
        return(exp(logProbX))
    }
)


#' @rdname dLogitBeta
#' @export
rLogitBeta <- nimbleFunction (
    ## Generates y ~ Beta(shape1,shape2)
    ## Returns   x = logit(y)
    run = function(n = integer(0, default=1),
                   shape1 = double(0, default=1.0),
                   shape2 = double(0, default=1.0)) {
        returnType(double(0))
        if(n != 1)
            nimPrint("Warning: rLogitBeta only allows n = 1; Using n = 1.\n")
        y <- rbeta(1, shape1=shape1, shape2=shape2)
        x <- logit(y)
        return(x)
    }
)
