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

#' @importFrom rxode2random rxWithPreserveSeed
#' @export
rxode2random::rxWithPreserveSeed

#' @importFrom rxode2random rxSetSeed
#' @export
rxode2random::rxSetSeed

#' @importFrom rxode2random rxWithSeed
#' @export
rxode2random::rxWithSeed

#' @importFrom rxode2random rxSetSeed
#' @export
rxode2random::rxSetSeed

#' @importFrom rxode2random rxGetSeed
#' @export
rxode2random::rxGetSeed

#' @importFrom rxode2random cvPost
#' @export
rxode2random::cvPost

#' @importFrom rxode2random invWR1d
#' @export
rxode2random::invWR1d

#' @importFrom rxode2random phi
#' @export
rxode2random::phi

#' @importFrom rxode2random rLKJ1
#' @export
rxode2random::rLKJ1

#' @importFrom rxode2random rinvchisq
#' @export
rxode2random::rinvchisq

#' @importFrom rxode2random rxGetSeed
#' @export
rxode2random::rxGetSeed

#' @importFrom rxode2random rxRmvn
#' @export
rxode2random::rxRmvn

#' @importFrom rxode2random rxSetSeed
#' @export
rxode2random::rxSetSeed

#' @importFrom rxode2random rxWithSeed
#' @export
rxode2random::rxWithSeed

#' @importFrom rxode2random rxSeedEng
#' @export
rxode2random::rxSeedEng

#' @importFrom rxode2random .vecDf
#' @export
rxode2random::.vecDf

#' @importFrom rxode2random .cbindOme
#' @export
rxode2random::.cbindOme

#' @importFrom rxode2et et
#' @export
rxode2et::et

#' @importFrom rxode2et .s3register
#' @export
rxode2et::.s3register

#' @importFrom rxode2et add.dosing
#' @export
rxode2et::add.dosing

#' @importFrom rxode2et add.sampling
#' @export
rxode2et::add.sampling

#' @importFrom rxode2et as.et
#' @export
rxode2et::as.et

#' @importFrom rxode2et as.rxEvid
#' @export
rxode2et::as.rxEvid

#' @importFrom rxode2et as.rxRateDur
#' @export
rxode2et::as.rxRateDur

#' @importFrom rxode2et etExpand
#' @export
rxode2et::etExpand

#' @importFrom rxode2et etRbind
#' @export
rxode2et::etRbind

#' @importFrom rxode2et etRep
#' @export
rxode2et::etRep


#' @importFrom rxode2et etSeq
#' @export
rxode2et::etSeq

#' @importFrom rxode2et eventTable
#' @export
rxode2et::eventTable

#' @importFrom rxode2et rxEtDispatchSolve
#' @export
rxode2et::rxEtDispatchSolve

#' @importFrom rxode2et rxEvid
#' @export
rxode2et::rxEvid

#' @importFrom rxode2et rxRateDur
#' @export
rxode2et::rxRateDur

#' @importFrom rxode2et rxReq
#' @export
rxode2et::rxReq

#' @importFrom rxode2et .collectWarnings
#' @export
rxode2et::.collectWarnings

#' @importFrom rxode2et .clearPipe
#' @export
rxode2et::.clearPipe

#' @importFrom rxode2et rxCbindStudyIndividual
#' @export
rxode2et::rxCbindStudyIndividual


#' @importFrom rxode2et rxStack
#' @export
rxode2et::rxStack


.chin <- function(left, right) {
  .Call(`_rxode2_chin`, left, right)
}


.forderForceBase <- function(x1) {
  .Call(`_rxode2_forderForceBase`, x1)
}


.useForder <- function() {
  .Call(`_rxode2_useForder`)
}

.getForder <- function() {
  .Call(`_rxode2_getForder`)
}
