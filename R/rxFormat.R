#' Format rxSolve and related objects as html.
#'
#' @param x rxode2 object
#' @param ... Extra arguments sent to kable
#' @return html code for rxSolve object
#' @author Matthew L. Fidler
#' @export
rxHtml <- function(x, ...) {
  UseMethod("rxHtml")
}
#' @rdname rxHtml
#' @export
rxHtml.rxSolve <- function(x, ...) {
  rxode2::rxReq("knitr")
  bound <- .getBound(x, parent.frame(2))
  cat("<style>
         th,td{
             padding:2px 5px 2px 5px;
         }
    </style>")
  cat("<table style=\"border-radius: 10px 10px 10px 10px; -moz-border-radius: 10px 10px 10px 10px; -webkit-border-radius: 10px 10px 10px 10px; border: 5px solid #7b3e3e;\"><tr><td style=\"text-align: center; padding: 5px;\"><strong>Solved rxode2 object</strong></td><tr><td><strong>Parameters</strong> (<span style=\"color: Orange;\">")
  cat(bound)
  cat("</span><strong \"color: Blue;\">&#36;params</strong>):</td></tr><tr><td>")
  cat(knitr::kable(x$params, "html", ...))
  df <- x$covs
  if (!is.null(df)) {
    cat(paste0("</td></tr><tr><td><strong>Covariates</strong> (<span style=\"color: Orange;\">", bound, "</span><strong \"color: Blue;\">&#36;covs</strong>):"))
    cat(knitr::kable(head(as.matrix(df)), "html", ...))
  }
  cat("</td></tr><tr><td><strong>Initial Conditions</strong> (<span style=\"color: Orange;\">", bound, "</span><strong \"color: Blue;\">&#36;inits</strong>):</td></tr><tr><td>")
  cat(paste0(knitr::kable(data.frame(t(x$inits)))))
  cat("<strong>First part of data (object):</strong>")
  cat(knitr::kable(head(as.data.frame(x)), "html", ...))
  cat("</td></tr></table>")
}
