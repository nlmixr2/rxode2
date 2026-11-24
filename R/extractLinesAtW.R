#' Extract model lines at W
#'
#' @param modelLines modelLines list
#' @param w where to split the modelLines
#' @return model lines list with:
#'   list(pre=pre model lines, w=model lines, post=post model lines)
#' @noRd
#' @author Matthew L. Fidler
.extractModelLinesAtW <- function(modelLines, w) {
  checkmate::assertInteger(w, lower = 1, len = 1)
  .w <- w
  # .pre will be list() if .w is at 1
  .pre <- lapply(seq(1, .w)[-.w], function(i) {
    modelLines[[i]]
  })
  # .post will be list() if .w is at the end of the line
  .post <- lapply(seq(.w, length(modelLines))[-1], function(i) {
    modelLines[[i]]
  })
  list(pre = .pre, w = modelLines[[.w]], post = .post)
}
