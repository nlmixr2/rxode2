#' Current C user function registration of a name
#'
#' @param name function name
#' @return list with the argument count, C code, symengine function and
#'   derivative table entry (each NULL when absent)
#' @noRd
#' @author Matthew L. Fidler
.udfRegGet <- function(name) {
  .eq <- .udfEnv$rxSEeqUsr
  .code <- .udfEnv$rxCcode
  list(
    eq = if (any(names(.eq) == name)) .eq[[name]] else NULL,
    code = if (any(names(.code) == name)) .code[[name]] else NULL,
    sfs = get0(name, envir = .udfEnv$symengineFs, inherits = FALSE),
    d = get0(name, envir = rxode2parseD(), inherits = FALSE)
  )
}

#' Set (or remove) the C user function registration of a name
#'
#' @param name function name
#' @param reg registration, as from `.udfRegGet()`
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfRegSet <- function(name, reg) {
  .eq <- .udfEnv$rxSEeqUsr
  .eq <- .eq[names(.eq) != name]
  if (!is.null(reg$eq)) {
    .eq <- c(.eq, setNames(reg$eq, name))
  }
  .udfEnv$rxSEeqUsr <- .eq
  .code <- .udfEnv$rxCcode
  .code <- .code[names(.code) != name]
  if (!is.null(reg$code)) {
    .code <- c(.code, setNames(reg$code, name))
  }
  .udfEnv$rxCcode <- .code
  if (is.null(reg$sfs)) {
    if (exists(name, envir = .udfEnv$symengineFs, inherits = FALSE)) {
      rm(list = name, envir = .udfEnv$symengineFs)
    }
  } else {
    assign(name, reg$sfs, envir = .udfEnv$symengineFs)
  }
  .rxD <- rxode2parseD()
  if (is.null(reg$d)) {
    if (exists(name, envir = .rxD, inherits = FALSE)) {
      rm(list = name, envir = .rxD)
    }
  } else {
    assign(name, reg$d, envir = .rxD)
  }
  .rxSEstate$dTemplates <- NULL
  invisible()
}

#' Register C user functions, returning what they replaced
#'
#' @param regs named list of registrations
#' @return named list of the registrations replaced, for `.udfRegRestore()`
#' @noRd
#' @author Matthew L. Fidler
.udfRegActivate <- function(regs) {
  .saved <- list()
  for (.n in names(regs)) {
    .cur <- .udfRegGet(.n)
    .reg <- regs[[.n]]
    if (
      identical(.cur$code, .reg$code) &&
        identical(.cur$eq, .reg$eq) &&
        !is.null(.cur$sfs) &&
        (is.null(.reg$d) || !is.null(.cur$d))
    ) {
      next
    }
    .saved[[.n]] <- .cur
    .udfRegSet(.n, .reg)
  }
  .saved
}

#' Restore the registrations `.udfRegActivate()` replaced
#'
#' @param saved named list returned by `.udfRegActivate()`
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfRegRestore <- function(saved) {
  for (.n in rev(names(saved))) {
    .udfRegSet(.n, saved[[.n]])
  }
  invisible()
}
