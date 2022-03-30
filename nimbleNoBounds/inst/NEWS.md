                            CHANGES IN VERSION 1.0.0 (March 2022)

USER LEVEL CHANGES

-- nimbleNoBounds contains 'd' and 'r' functions for a collection of transformed distributions.
-- The functions are names via the convention d[Transform][Distribution]. For example, the density function for the log transformed log-normal distribution is 'dLogLnorm'.
-- Available transformed density functions include: dLogChisq, dLogExp, dLogGamma, dLogHalfflat, dLogInvgamma, dLogLnorm, dLogWeib, dLogitBeta, dLogitUnif.
-- A vignette illustrates potential efficiency gains from sampling on the real line.
