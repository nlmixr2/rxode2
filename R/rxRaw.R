#' Serialize an R Object to a Raw Vector
#'
#' @param x object to serialize
#' @param qs2 logical; if TRUE (default) use the 'qs2' package for
#'   serialization, otherwise use base R serialize
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
rxSerialize <- function(x, qs2=TRUE) {
  if (qs2) {
    qs2::qs_serialize(x)
  } else {
    serialize(x, NULL)
  }
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
  if (!inherits(x, "raw")) return(x)
  .type <- .Call(`_rxode2_rxGetSerialType_`, x)
  switch(.type,
         qs2 = {
           qs2::qs_unserialize(x)
         },
         qdata = {
           qs2::qdata_deserialize(x)
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
