#' @importFrom ggplot2 scale_type
#' @export
ggplot2::scale_type

#' @importFrom ggplot2 ggplot
#' @export
ggplot2::ggplot

#' @importFrom ggplot2 aes
#' @export
ggplot2::aes

#' @importFrom ggplot2 geom_line
#' @export
ggplot2::geom_line

#' @importFrom ggplot2 facet_wrap
#' @export
ggplot2::facet_wrap


#' @importFrom ggplot2 %+replace%
`%+replace%`

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

#' @importFrom digest sha1
#' @export
digest::sha1
