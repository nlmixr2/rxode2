#' A description of Rode2 supported residual errors
#'
#' @format A data frame with 6 columns and 183 rows
#' \describe{
#' \item{Error model}{A description of the type of residual error}
#' \item{Functional Form}{For additive and proportional what functional form is used}
#' \item{Transformation}{The type of transformation that is done on the DV and the prediction}
#' \item{code}{Example code for the residual error type}
#' \item{addProp}{The type of add+prop residual error default that would be equivalent}
#' \item{lhs}{what the left handed side of the specification represents, either a response variable, or a compartment specification}
#' }
#' @references
#' The transformation-based and autocorrelated (`ar()`, AR(1)) residual
#' error models follow Karlsson MO, Beal SL, Sheiner LB. Three new residual
#' error models for population PK/PD analyses. J Pharmacokinet Biopharm.
#' 1995;23(6):651-672. doi:10.1007/BF02353466.
"rxResidualError"
