##' Note, in rxode2 models \code{n} or \code{ncores} are not
##' accepted/included.  These are determined by the \code{rxSolve()}
##' and the event table.
##'
##' Care should be taken with this method not to encounter the
##' birthday problem, described
##' \url{https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/}.
##' Since the \code{sitmo} \code{threefry}, this currently generates
##' one random deviate from the uniform distribution to seed the
##' engine \code{threefry} and then run the code.
##'
##' Therefore, a simple call to the random number generated followed by a second
##' call to random number generated may have identical seeds.  As the number of
##' random number generator calls are increased the probability that the
##' birthday problem will increase.
##'
##' The key to avoid this problem is to either run all simulations in the
##' `rxode2` environment once (therefore one seed or series of seeds
##' for the whole simulation), pre-generate all random variables
##' used for the simulation, or seed the rxode2 engine with `rxSetSeed()`
##'
##' Internally each ID is seeded with a unique number so that the
##' results do not depend on the number of cores used.
##'
