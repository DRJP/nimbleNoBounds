                            CHANGES IN VERSION 1.0.3 (November 2023)

USER LEVEL CHANGES
-- Bug fix in the vignette and clarification of back-transform in the dLogitUnif help file example.

                            CHANGES IN VERSION 1.0.2 (February 2023)

USER LEVEL CHANGES
-- Bug fix in rHalfflat. 'No' forces n=1, as everywhere else in nimble.

                            CHANGES IN VERSION 1.0.1 (May 2022)

USER LEVEL CHANGES
-- Modifications to examples in help files.

                            CHANGES IN VERSION 1.0.0 (March 2022)

USER LEVEL CHANGES

-- nimbleNoBounds contains 'd' and 'r' functions for a collection of transformed distributions.
-- The functions are names via the convention d[Transform][Distribution]. For example, the density function for the log transformed log-normal distribution is 'dLogLnorm'.
-- Available transformed density functions include: dLogChisq, dLogExp, dLogGamma, dLogHalfflat, dLogInvgamma, dLogLnorm, dLogWeib, dLogitBeta, dLogitUnif.
-- A vignette illustrates potential efficiency gains from sampling on the real line.
