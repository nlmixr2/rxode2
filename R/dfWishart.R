dfWishartCalcRse <- function(nu, omega, totN, rse, diag=TRUE) {
  .cv <- cvPost(nu, omega, totN)
  #.cv <- ivdoctr:::rinvwish(totN, nu, omega)
  #print(.cv)
  #max(omegaListRse(.cv)$rse)-rse
  if (diag) {
    mean(diag(omegaListRse(.cv)$rse))-rse
  } else {
    mean(as.vector(omegaListRse(.cv)$rse))-rse
  }
}

#' This uses simulations to match the rse
#'
#' @param omega represents the matrix for simulation
#' @param n This represents the number of subjects/samples this comes
#'   from (used to calculate rse).  When present it assumes the rse=
#'   sqrt(2)/sqrt(n)
#' @param rse This is the rse that we try to match, if not specified,
#'   it is derived from `n`
#' @param upper The upper boundary for root finding in terms of
#'   degrees of freedom.  If not specified, it is n*200
#' @param totN This represents the total number of simulated inverse
#'   wishart deviates
#' @param diag When `TRUE`, represents the rse to match is the
#'   diagonals, otherwise it is the total matrix.
#' @param seed to make the simulation reproducible, this represents
#'   the seed that is used for simulating the inverse Wishart
#'   distribution
#' @return output from `uniroot()` to find the right estimate
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' dfWishart(lotri::lotri(a+b~c(1, 0.5, 1)), 100)
#'
dfWishart <- function(omega, n, rse, upper, totN=1000, diag=TRUE, seed=1234) {
  checkmate::assertMatrix(omega, "numeric", min.rows=1, min.cols=1)
  if (!missing(rse) && !missing(n)) {
    stop("can only specify `n` or `rse` not both", call.=FALSE)
  }
  if (missing(rse) && !missing(n)) {
    checkmate::assertIntegerish(n, len=1, lower=1)
    rse <- sqrt(2)/sqrt(n)
    if (missing(upper)) {
      upper <- 200*n
    }
  } else if (missing(rse)) {
    stop("need to match rse with some metric", call.=FALSE)
  } else if (missing(upper)) {
    upper <- 200*(sqrt(2)/rse)^2
  }
  checkmate::assertNumeric(upper, len=1, lower=1)
  checkmate::assertIntegerish(totN, len=1, lower=1)
  .d <-  dim(omega)
  if (.d[1] != .d[2]) {
    stop("omega must be a square matrix",
         call.=FALSE)
  }
  # nu-p-3 > 0 so min for nu is
  .lower <- .d[1] + 3.1
  .upper <-  upper
  rxWithSeed(seed, {
    stats::uniroot(dfWishartCalcRse, lower=.lower, upper=.upper, omega=omega, totN=totN, rse=rse, diag=diag)
  })
}
#' Swaps the matrix list with a cube
#'
#' @param matrixListOrCube Either a list of 2-dimensional matrices or a cube of matrices
#' @return A list or a cube (opposite format as input)
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' # Create matrix list
#' matLst <- cvPost(10, lotri::lotri(a+b~c(1, 0.25, 1)), 3)
#' print(matLst)
#'
#' # Convert to cube
#' matCube <- swapMatListWithCube(matLst)
#' print(matCube)
#'
#' # Convert back to list
#' matLst2 <- swapMatListWithCube(matCube)
#' print(matLst2)
#'
swapMatListWithCube <- function(matrixListOrCube) {
  .dim <- dim(matrixListOrCube)
  if (length(.dim) == 3L) {
    return(.Call(`_rxode2_swapMatListWithCube_`, matrixListOrCube))
  } else if (length(.dim) > 0L) {
  } else if (inherits(matrixListOrCube, "list") && length(matrixListOrCube) > 0L) {
    .m0 <- matrixListOrCube[[1]]
    .dim <- dim(.m0)
    if (length(.dim) == 2L)     return(.Call(`_rxode2_swapMatListWithCube_`, matrixListOrCube))
  }
  stop("The input must be a cube or a list of matrices", call.=FALSE)
}
