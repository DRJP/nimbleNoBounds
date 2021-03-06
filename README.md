nimbleNoBounds
===============

A collection of continuous univariate probability distributions transformed to the (unbounded) real line.
Using these transformed distributions can result in more efficient MCMC,
particularly when adaptive Metropolis-Hastings is used to sample in the vicinity of bounds on the parameter-space.
See vignette("nimbleNoBounds") for several examples.


Installation
------------
The package can be installed from [CRAN](https://cran.r-project.org/web/packages/nimbleNoBounds/index.html). In the R console, just write
```
install.packages("nimbleNoBounds")
```

The package can be installed from github using the R package 'remotes' as follows.
```
remotes::install_git(url="https://github.com/DRJP/nimbleNoBounds.git", subdir="nimbleNoBounds", build_vignettes = TRUE)
```
Switching to 'build_vignettes = FALSE' makes for a faster installation, and thus may be a prefered option for users already familiar with the package.


Citation
--------
If you use this package in you analyses, please cite the following DOI.
[![DOI](https://zenodo.org/badge/474911114.svg)](https://zenodo.org/badge/latestdoi/474911114)

The full citation is available in BibTex format via the following R command.
```
citation("nimbleNoBounds")
```
