# This file registers all distributions when the package is loaded.
.onAttach <- function(libname, pkgname) {

  packageStartupMessage("Loading nimbleChangeOfVariables. \nRegistering the following user-defined functions:\n ",
                        "dLogExp, dLogGamma\n" )

# Register the distributions explicitly for two reasons:
# 1. Avoid message to user about automatic registrations upon first use in a nimbleModel
# 2. Establish default len = 0 via reparameterization mechanism.
  suppressMessages({

  registerDistributions(list(
    dLogExp = list(
      BUGSdist = "dLogExp(rate)",
      discrete = FALSE,
      types    = c("value=double(0)", "rate=double(0)"),
      pqAvail  = FALSE)), verbose = F
    )

  registerDistributions(list(dLogGamma = list(
    BUGSdist = "dLogGamma(shape, scale, rate, mean, sd)",
    Rdist    = c('dLogGamma(shape, scale = 1/rate)',
                 'dLogGamma(shape = mean^2/sd^2, scale = sd^2/mean)'),
    altParams= c('rate = 1/scale',
                 'mean = scale*shape',
                 'sd   = scale * sqrt(shape)'),
    discrete = FALSE,
    types    = c("value=double(0)", "shape=double(0)", "scale=double(0)", "rate=double(0)", "mean=double(0)", "sd=double(0)"),
    pqAvail  = FALSE)))

  registerDistributions(list(dLogitBeta = list(
    BUGSdist = "dLogitBeta(shape1, shape2)",
    discrete = FALSE,
    types    = c("value=double(0)", "shape1=double(0)", "shape2=double(0)"),
    pqAvail  = FALSE)))

  registerDistributions(list(dLogitUnif = list(
    BUGSdist = "dLogitUnif(min, max)",
    discrete = FALSE,
    types    = c("value=double(0)", "max=double(0)", "max=double(0)"),
    pqAvail  = FALSE)))


})}
