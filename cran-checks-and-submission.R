## How to write your own R package and publish it on CRAN
## https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/r-package/
## Work through the checks listed in this blog post

## Tests the package can build and install on various platforms
library(rhub)
library(here)
setwd(here::here("nimbleNoBounds"))
getwd()

## DON'T FORGET THIS STEP: "~/nimbleProjects/nimbleNoBounds/buildingPackageCommands.R"

##############################################
## All the rhub stuff below is depreicated. ##
## Here's how to use the new rhub           ##
## https://blog.r-hub.io/2024/04/11/rhub2/  ##
##############################################
rhub::rc_submit()



##################################
## Is this still functional ??? ##
##################################
devtools::check_rhub()
devtools::check()

# Or... alternatively, perhaps this can help...
# https://stackoverflow.com/questions/63586750/error-with-ubuntu-linux-when-running-check-rhub-due-to-xml-library
# platforms()
res  = rhub::check(platform="ubuntu-gcc-release") # Run from: /home/pleydell/nimbleProjects/nimbleNoBounds/nimbleNoBounds
## res2 = rhub::check(platform="ubuntu-gcc-devel")
## SUGGESTS IT'S AN EIGEN or RHUB PROBLEM
## It is an Rhub problem!!!
## https://github.com/r-hub/rhub/issues/448#issuecomment-1009922117
##
## 13h48ish ran command - 13h4
res$cran_summary()



# Generate your cran-comments.md, then you copy-paste the output from the function above
usethis::use_cran_comments()

# Spell checkl
devtools::spell_check()

# Checks for windows version
devtools::check_win_devel()

## Submit to CRAN with the following
devtools::release()
