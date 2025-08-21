#' @export
rxUdfUi.mix <- function(fun) {
  eval(fun)
}

.mixenv <- new.env(parent = emptyenv())
.mixenv$probs <- NULL
#' Specify a mixture model of variables
#'
#'
#' @param ... Arguments to the mixture model.
#'
#' The first call to the mixture model function takes an odd number of
#' arguments (at least 3).
#'
#' For example the model we could have:
#'
#' cl = mix(cl1, p1, cl2, p2, cl3)
#'
#' Here there is a mixture of three clearance variables, `cl1`, `cl2`,
#' and `cl3`, at a probability of `p1`, `p2`, and the last one is
#' assumed to be `1 - p1 - p2`.
#'
#' For simulations this is selected randomly.  For estimations this is
#' selected by the data for each individual.
#'
#' After the first call when the number of populations has been
#' established, you can also call the mixture model with the number of
#' populations, for example:
#'
#' v = mix(v1, v2, v3)
#'
#' The ui function will translate this to the following model:
#'
#' v = mix(v1, p1, v2, p2, v3)
#'
#' This is because the first call to `mix()` sets the probabilities.
#' In rxode2/nlmixr2 these probabilities should be conserved between
#' the models.  These probabilities also have to be defined in the ini
#' block directly.
#'
#' @return The mixture model replacement for the underlying rxode2 model.
#' @export
#' @author Matthew L. Fidler
#' @examples
mix <- function(...) {
  .call <- match.call(expand.dots = TRUE)
  .args <- lapply(seq_along(.call)[-1L],
                  function(i) { .call[[i]] })
  .df <- rxUdfUiIniDf()
  if (is.null(.df)) {
    stop("mix() in a ui model must have an ini block",
         call.= FALSE)
  }
  if (all(is.na(.df$neta1))) {
    stop("mix() in a ui model must have an ini block with eta variables",
         call.= FALSE)
  }
  .ret <- list(replace=.call)
  if (is.null(rxUdfUiMv())) {
    if (identical(rxUdfUiNum(), 1L)) {
      if (length(.args) < 3) {
        stop("mix() requires at least three arguments")
      }
      .mixenv$probs <- NULL
      .probs <- vapply(seq_along(.args),
                       function(i) {
                         if (i %% 2 == 0) {
                           as.character(.args[[i]])
                         } else {
                           ""
                         }
                       }, character(1))
      .mixenv$probs <- .probs[nzchar(.probs)]
      .mp <- vapply(.mixenv$probs,
                    function(p) {
                      p %in% .df$name
                    }, logical(1), USE.NAMES = TRUE)
      .w <- which(!.mp)
      if (length(.w) >= 1) {
        stop("the probabilities in a mixture must be in the model block, these variables were not: '",
             paste(names(.mp)[.w], collapse="', '"), "'")
      }
      .mp <- sum(vapply(.mixenv$probs,
                        function(p) {
                          .w <- which(.df$name == p)
                          .df$est[.w]
                        }, numeric(1), USE.NAMES = FALSE))
      if (.mp >= 1 || .mp <= 0)  {
        stop("the probabilities in a mixture must sum to a number between 0 and 1, they sum to: ",
             .mp)
      }
    }
  }
  if (length(.args) == length(.mixenv$probs)*2 + 1L) {
    .mixenv$np <- 1L
    .matchProbs <- all(vapply(seq_along(.args),
                              function(i) {
                                if (i %% 2 == 0) {
                                  .ret <- identical(.mixenv$probs[.mixenv$np], as.character(.args[[i]]))
                                  .mixenv$np <- .mixenv$np + 1L
                                  .ret
                                } else {
                                  TRUE
                                }
                              }, logical(1)))
    if (!.matchProbs) {
      stop("the probabilities in a mixture must match throughout the problem",
           call.= FALSE)
    }
  }
  if (length(.args) == length(.mixenv$probs) + 1L) {
    .mixenv$na <- 1L
    .mixenv$np <- 1L
    .ret <- list(replace=str2lang(paste0("mix(",
              paste(vapply(seq_len(length(.args)+ length(.mixenv$probs)),
                           function(i) {
                             if ((i %% 2) == 0) {
                               .ret <- as.character(.mixenv$probs[.mixenv$np])
                               .mixenv$np <- .mixenv$np + 1L
                             } else {
                               .ret <- deparse1(.args[[.mixenv$na]])
                               .mixenv$na <- .mixenv$na + 1L
                             }
                             .ret
                           }, character(1), USE.NAMES = FALSE),
                    collapse=", "),
              ")")))
  }
  .ret
}
