sessionInfo()

detach("package:nimbleChangeOfVariables")
unloadNamespace("nimbleChangeOfVariables")

setwd(here::here('nimbleChangeOfVariables'))
roxygen2::roxygenise()

setwd(here::here())
devtools::build("nimbleChangeOfVariables", vignettes=TRUE) # FALSE

devtools::check("nimbleChangeOfVariables")

devtools::install("nimbleChangeOfVariables", build_vignettes = TRUE) # FALSE

help.start()

setwd("nimbleChangeOfVariables")
devtools::build_vignettes()
