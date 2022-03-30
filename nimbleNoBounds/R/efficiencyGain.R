#' Efficiency gain estimates from vignette example
#'
#' The toy example in the vignette provides a simple Monte Carlo experiment that tests the gain in sampling efficiency when switching to sampling on unbounded scales (i.e. the real line).
#' Since that Monte Carlo experiment takes 10 minutes to run, `efficiencyGain` is provided as an example set of output data - providing an alternative to re-running the Monte Carlo code.
#'
#' @name efficiencyGain
#' @docType data
#' @author David Pleydell
#'
#' @format A data frame with 111 rows (number of Monte Carlo replicates) and 8 variables
#' \describe{
#'   \item{beta}{Efficiency gain when sampling a beta distribution on the real line.}
#'   \item{chisq}{Efficiency gain when sampling a chi-squared distribution on the real line.}
#'   \item{exp}{Efficiency gain when sampling a exponential distribution on the real line.}
#'   \item{gamma}{Efficiency gain when sampling a gamma distribution on the real line.}
#'   \item{invgamma}{Efficiency gain when sampling a inverse-gamma distribution on the real line.}
#'   \item{lnorm}{Efficiency gain when sampling a log-normal distribution on the real line.}
#'   \item{unif}{Efficiency gain when sampling a uniform distribution on the real line.}
#'   \item{weib}{Efficiency gain when sampling a Weibull distribution on the real line.}
#' }
#' @source See `vignette("nimbleNoBounds")`
#'
#' @keywords data

NULL
