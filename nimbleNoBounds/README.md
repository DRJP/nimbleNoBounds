nimbleNoBounds
==============

A collection of common bounded univariate probability distributions transformed to the unbounded real line.
The idea is that, when posteriors fall close to the bounds of probability distribution,
the efficiency of adaptive Metropolis Hastings may be compromised by the bounds.
In which case, sampling on the unbounded real line could provide more efficent sampling.
The change of variables technique is used to identify equivelent distributions on the unbounded scale.
