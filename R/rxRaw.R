#' Serialize an R Object to a Raw Vector
#'
#' @param x object to serialize
#' @param type serialization type; one of "qs2", "qdata", or "base".
#' @return raw vector
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#'
#' rxSerialize(mtcars, qs2=TRUE)
#'
#' rxRawToC(mtcars)
#'
#' rxSerialize(mtcars, qs2=FALSE)
#'
rxSerialize <- function(x, type=c("qs2", "qdata", "base")) {
  switch(match.arg(type),
         qs2 = {
           qs2::qs_serialize(x)
         },
         qdata = {
           qs2::qd_serialize(x)
         },
         base = {
           serialize(x, NULL)
         },
         stop("Unknown serialization type"))
}
#' Deserialize a Raw Vector or String to an R Object
#'
#' @param x raw vector or string to deserialize
#' @return R object
#' @keywords internal
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' rxDeserialize(rxSerialize(mtcars, qs2=TRUE))
#'
#' rxDeserialize(rxSerialize(mtcars, qs2=FALSE))
rxDeserialize <- function(x) {
  if (checkmate::testCharacter(x, len=1L, any.missing=FALSE)) {
    .x <- try(qs2::base91_decode(x))
    if (inherits(.x, "try-error")) {
      return(x)
    }
    x <- .x
  }
  if (!inherits(x, "raw")) return(x)
  .type <- .Call(`_rxode2_rxGetSerialType_`, x)
  switch(.type,
         qs2 = {
           qs2::qs_deserialize(x)
         },
         qdata = {
           qs2::qd_deserialize(x)
         },
         qs = {
           rxReq("qs")
           qs::qdeserialize(x)
         },
         base = {
           unserialize(x)
         },
         stop("Unknown serialization type"))
}
#' Convert a Raw Vector or R object to C Code
#'
#' This function converts a raw vector or R object to C code that that
#' is used in the rxode2 model
#'
#' @return character string of C code
#'
#' @keywords internal
#' @inherit rxSerialize
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' message(rxRawToC(mtcars))
#'
rxRawToC <- function(raw, qs2=TRUE) {
  if (inherits(raw, "raw")) {
    .env <- new.env(parent = emptyenv())
    .env$i <- -1L
    .ret <- paste0("    SEXP rw    = PROTECT(Rf_allocVector(RAWSXP, ",
                   length(raw),
                   "));pro++;\n",
                   "    RAW(rw)    ={",
                   paste(vapply(seq_along(raw),
                                function(i) {
                                  .env$i <- .env$i + 1L
                                  if (.env$i %% 10L == 0L) {
                                    paste0("\n                0x", toupper(format(raw[i], width=2)))
                                  } else {
                                    paste0("0x", toupper(format(raw[i], width=2)))

                                  }
                                },
                                character(1),
                                USE.NAMES=FALSE),
                         collapse=", "),
                   "\n                };\n")
    .ret
  } else {
    rxRawToC(rxSerialize(raw, qs2=qs2))
  }
}
