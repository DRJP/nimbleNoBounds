nimbleNoBounds
==============

A collection of common univariate probability distributions transformed to unbounded scales.
The idea is that, when posteriors fall close to the bounds of probability distribution,
the efficiency of adaptive Metropolis Hastings may be compromised by the bounds on the parameter space.
In which case, sampling on the unbounded real line could provide more efficent sampling.
