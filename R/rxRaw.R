#' Determines if the object is a valid object for serialization
#'
#' Currently this is only data frames and rxModelVars objects
#'
#' @param obj object to test for validity
#' @return boolean, TRUE if valid for serialization in rxode2
#' @keywords internal
#' @noRd
#' @author Matthew L. Fidler
.validSerializationObject <- function(obj) {
  .cls <- class(obj)
  if (length(.cls) == 1L &&
        (inherits(obj, "rxModelVars") ||
           inherits(obj, "data.frame"))) {
    return(TRUE)
  }
  FALSE
}
#' Get the Default Serialization Type
#'
#' @return string indicating the default serialization type
#'
#' @export
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' rxGetDefaultSerialize()
#'
rxGetDefaultSerialize <- function() {
  op <- rxode2.serialize.type
  if (!op %in% c("qs2", "qdata", "base", "bzip2", "xz")) {
    stop("option 'rxode2.serialize.type' must be one of 'qs2', 'qdata', 'base', 'bzip2' or 'xz'", call.=FALSE)
  }
  op
}
#' Serialize an R Object to a Raw Vector
#'
#' @param x object to serialize
#'
#' @param type serialization type; one of "qs2", "qdata", "base", "xz" or "bzip2".
#'
#' @return raw vector
#'
#' @export
#'
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#'
#' rxSerialize(mtcars)
#'
#' rxRawToC(mtcars)
#'
rxSerialize <- function(x, type=c("xz", "bzip2", "qs2", "qdata", "base")) {
  ## Suggested for security reasons to limit what can be deserialized
  if (missing(type)) {
    type <- rxGetDefaultSerialize()
  }
  if (!.validSerializationObject(x)) {
    .cls <- class(x)
    stop("serialization object of class ",
         paste(.cls, collapse=", "),
         " is not supported")
  }
  switch(match.arg(type),
         qs2 = {
           qs2::qs_serialize(x)
         },
         qdata = {
           qs2::qd_serialize(x)
         },
         bzip2 = {
           memCompress(serialize(x, NULL), type="bzip2")
         },
         xz = {
           memCompress(serialize(x, NULL), type="xz")
         },
         base = {
           serialize(x, NULL)
         },
         stop("unknown serialization type") # nocov
         )
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
#' rxDeserialize(rxSerialize(mtcars))
#'
rxDeserialize <- function(x) {
  if (checkmate::testCharacter(x, len=1L, any.missing=FALSE)) {
    .x <- try(qs2::base91_decode(x))
    if (inherits(.x, "try-error")) {
      stop("Input must be a raw vector or base91 encoded string")
    }
    x <- .x
  }
  if (!inherits(x, "raw")) {
    stop("Input must be a raw vector or base91 encoded string")
  }
  .type <- .Call(`_rxode2_rxGetSerialType_`, x)
  .ret <- try(switch(.type,
                     qs2 = {
                       rxReq("qs2")
                       qs2::qs_deserialize(x)
                     },
                     qdata = {
                       rxReq("qs2")
                       qs2::qd_deserialize(x)
                     },
                     qs = {
                       rxReq("qs")
                       .Call(`_rxode2_qsDes`, x)
                     },
                     bzip2 = {
                        unserialize(memDecompress(x, type="bzip2"))
                     },
                     xz = {
                       unserialize(memDecompress(x, type="xz"))
                     },
                     base = {
                       unserialize(x)
                     },
                     stop("Unknown serialization type")), silent=TRUE)
  if (inherits(.ret, "try-error")) {
    stop("Deserialization failed")
  }
  .cls <- class(.ret)
  ## Suggested for security reasons to limit what can be deserialized
  if (.validSerializationObject(.ret)) {
    return(.ret)
  }
  stop("Deserialized object of class ",
       paste(.cls, collapse=", "),
       " is not supported")
}
#' Convert a Raw Vector or R object to C Code
#'
#' This function converts a raw vector or R object to C code that
#' is used in the rxode2 model
#'
#' @return character string of C code
#'
#' @keywords internal
#' @inherit rxSerialize
#' @param raw raw vector or R object to convert
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' message(rxRawToC(mtcars))
#'
rxRawToC <- function(raw, type=c("xz", "qs2", "qdata", "base", "bzip2")) {
  if (missing(type)) {
    type <- rxGetDefaultSerialize()
  }
  if (inherits(raw, "raw")) {
    .ret <- paste0("    SEXP rw    = PROTECT(Rf_allocVector(RAWSXP, ",
                   length(raw),
                   "));pro++;\n",
                   "    unsigned char r[]={",
                   paste(paste0(ifelse((seq_along(raw) - 1L) %% 10L == 0L, "\n                0x", "0x"),
                                sprintf("%02X", as.integer(raw))),
                        collapse=", "),
                   "\n                };\n",
                   "    memcpy(RAW(rw), r, sizeof(r));")
    .ret
  } else {
    rxRawToC(rxSerialize(raw, type=type))
  }
}
