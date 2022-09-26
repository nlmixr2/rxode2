#' @importFrom rxode2ll llikBeta
#' @export
rxode2ll::llikBeta

#' @importFrom rxode2ll llikBinom
#' @export
rxode2ll::llikBinom

#' @importFrom rxode2ll llikCauchy
#' @export
rxode2ll::llikCauchy

#' @importFrom rxode2ll llikChisq
#' @export
rxode2ll::llikChisq

#' @importFrom rxode2ll llikExp
#' @export
rxode2ll::llikExp

#' @importFrom rxode2ll llikF
#' @export
rxode2ll::llikF

#' @importFrom rxode2ll llikGamma
#' @export
rxode2ll::llikGamma

#' @importFrom rxode2ll llikGeom
#' @export
rxode2ll::llikGeom

#' @importFrom rxode2ll llikNbinom
#' @export
rxode2ll::llikNbinom

#' @importFrom rxode2ll llikNbinomMu
#' @export
rxode2ll::llikNbinomMu

#' @importFrom rxode2ll llikNorm
#' @export
rxode2ll::llikNorm

#' @importFrom rxode2ll llikPois
#' @export
rxode2ll::llikPois

#' @importFrom rxode2ll llikT
#' @export
rxode2ll::llikT

#' @importFrom rxode2ll llikUnif
#' @export
rxode2ll::llikUnif

#' @importFrom rxode2ll llikWeibull
#' @export
rxode2ll::llikWeibull

#' @export
scale_type <- ggplot2::scale_type

#' @export
ggplot <- ggplot2::ggplot

#' @export
aes <- ggplot2::aes

#' @export
geom_line <- ggplot2::geom_line

#' @export
facet_wrap <- ggplot2::facet_wrap


#' @importFrom ggplot2 %+replace%
`%+replace%`

#' @importFrom ggplot2 aes
#' @export
ggplot2::aes

#' @importFrom ggplot2 facet_wrap
#' @export
ggplot2::facet_wrap


#' @importFrom ggplot2 geom_line
#' @export
ggplot2::geom_line


#' @importFrom ggplot2 ggplot
#' @export
ggplot2::ggplot

#' @importFrom ggplot2 scale_type
#' @export
ggplot2::scale_type

#' @importFrom ggplot2 ylab
#' @export
ggplot2::ylab

#' @importFrom ggplot2 xlab
#' @export
ggplot2::xlab

#' @importFrom ggplot2 waiver
#' @export
ggplot2::waiver

#' Empty Guide
#'
#' This empty guide draws nothing; It is included in rxode2 for
#' compatibility with ggplot 3.2
#'
#' @inheritParams ggplot2::guide_none
#' @return nothing, simply included to be compatible with ggplot 3.2
#' @export
#' @keywords internal
guide_none <- function(title = waiver(), position = waiver()) {
  stop("needs \"ggplot2\" 3.3.0", call. = FALSE)
}

#' @importFrom lotri lotri
#' @export
lotri::lotri

#' @importFrom ggplot2  label_value
#' @export
ggplot2::label_value

#' @importFrom ggplot2 label_both
#' @export
ggplot2::label_both

#' @importFrom ggplot2 label_context
#' @export
ggplot2::label_context

#' @importFrom ggplot2 label_wrap_gen
#' @export
ggplot2::label_wrap_gen

#' @importFrom ggplot2 label_context
#' @export
ggplot2::label_context

#' @importFrom ggplot2 scale_x_discrete
#' @export
ggplot2::scale_x_discrete

#' @importFrom ggplot2 scale_y_discrete
#' @export
ggplot2::scale_y_discrete

#' @importFrom ggplot2 scale_x_continuous
#' @export
ggplot2::scale_x_continuous

#' @importFrom ggplot2 scale_y_continuous
#' @export
ggplot2::scale_y_continuous

#' @importFrom ggplot2 scale_x_date
#' @export
ggplot2::scale_x_date

#' @importFrom ggplot2 scale_y_date
#' @export
ggplot2::scale_y_date

#' @importFrom ggplot2 expand_limits
#' @export
ggplot2::expand_limits

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

.SD <- NULL
`:=` <- function(...) { ## nocov start
  stop("this is only used in 'data.table'", .call = FALSE)
} ## nocov end
