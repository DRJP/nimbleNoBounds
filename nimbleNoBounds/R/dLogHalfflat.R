##' Log transformed half-flat distribution .
##'
##' \code{dLogHalfflat} and \code{rLogHalfflat} provide a log-transformed half-flat distribution.
##' Note, both dhalfflat and dLogHalfflat are improper. Thus, rLogHalfflat returns NAN, just as rhalfflat does.
##'
##' @name dLogHalfflat
##'
##' @param x A continuous random variable on the real line, where y=exp(x) and y ~ dhalflat().
##' @param log Logical flag to toggle returning the log density.
##' @param n Number of random variables. Currently limited to 1, as is common in nimble. See ?replicate for an alternative. Note, NAN will be returned because distribution is improper.
##' @import nimble
##'
##' @return A value proportional to the density, or the log of that value, of x, such that x = log(y) and y ~ dhalfflat().
##' @author David R.J. Pleydell
##' @examples
##'
##' par(mfrow=n2mfrow(2))
##' ## Plot 1
##' curve(dhalfflat(x), -11, 11, n=1001, col="red", lwd=3, xlab="y = exp(x)", ylab="dhalfflat(y)")
##' ## Plot 2: back-transformed
##' curve(dLogHalfflat(x), -5, 1.5, n=1001, col="red", lwd=3, , xlab="x = log(y)")
##' abline(v=0:1, h=c(0,1,exp(1)), col="grey")
##'

NULL

#' @rdname dLogHalfflat
#' @export
dLogHalfflat <- nimbleFunction (
  run = function(x = double(0),
                 log = integer(0, default=0)) {
    ## Returns density of x where
    ##                    y ~ half-flat()
    ##                    x = log(y)
    returnType(double(0))
    y = exp(x)
    logProbX = x # Change of variables
    if (log)
      return(logProbX)
    return(exp(logProbX))
  }
)

#' @rdname dLogHalfflat
#' @export
rLogHalfflat <- nimbleFunction (
  run = function(n = integer(0, default=1)) {
    ## Generates y ~ half-flat()
    ## Returns   x = log(y)
    returnType(double(0))
    if(n != 1)
      nimPrint("Warning: rLogHalfflat only allows n = 1; Using n = 1.\n")
    x <- rhalfflat(n=n)
    return(x)
  }
)
