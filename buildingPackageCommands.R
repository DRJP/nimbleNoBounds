sessionInfo()

detach("package:nimbleNoBounds")
unloadNamespace("nimbleNoBounds")

setwd(here::here('nimbleNoBounds'))
roxygen2::roxygenise()

setwd(here::here())
devtools::build("nimbleNoBounds", vignettes=TRUE) # FALSE

devtools::check("nimbleNoBounds")

devtools::install("nimbleNoBounds", build_vignettes = TRUE) # FALSE

help.start()

setwd("nimbleNoBounds")
devtools::build_vignettes()
