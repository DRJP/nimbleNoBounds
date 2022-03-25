# This file registers all distributions when the package is loaded.
.onAttach <- function(libname, pkgname) {

  packageStartupMessage("Loading nimbleChangeOfVariables. \nRegistering the following user-defined functions:\n ",
                        "dLogExp\n" )

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

})}
