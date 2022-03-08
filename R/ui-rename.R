#' This function asserts the requested rename makes sense
#'
#' It returns the new expression, old expression, new variable and old
#' variable
#'
#' @param line
#'
#' @param vars
#'
#' @return list(new, old, newChar, oldChar)
#'
#' @author Matthew L. Fidler
#' @noRd
.assertRenameErrorModelLine <- function(line, vars) {
  .var.name <- NULL
  if (is.name(line[[2]])) {
    .var.name <- as.character(line[[2]])
  } else {
    stop("to rename a variable you need to use 'newName=oldName' syntax",
         call.=FALSE)
  }
  if (!identical(line[[1]], quote(`<-`))) {
    stop("to rename a variable you need to use '", .var.name, "=oldName' syntax",
         call.=FALSE)
  }
  if (!is.name(line[[3]])) {
    stop("to rename a variable you need to use '", .var.name, "=oldName' syntax",
         call.=FALSE)
  } else {
    .var.name2 <- as.character(line[[3]])
  }
  if (.var.name %in% vars) {
    stop("the new variable '", .var.name, "' is already present in the model and cannot replace '", .var.name2, "'",
         call.=FALSE)
  }
  if (!(.var.name2 %in% vars)) {
    stop("the old variable '", .var.name2, "' is not present in the model and cannot be renamed to '", .var.name, "'",
         call.=FALSE)
  }
  list(line[[2]], line[[3]], .var.name, .var.name2)
}

#' Rename variables in the expression
#'
#' @param item Expression to recursively rename
#' @param new New name
#' @param old Old name
#' @return new expression with variable renamed
#' @author Matthew L. Fidler
#' @noRd
.rxRenameRecursive <- function(item, new, old) {
  if (is.atomic(item)) {
    return(item)
  }
  if (is.name(item)) {
    if (identical(item, old)) {
      return(new)
    } else {
      return(item)
    }
  } else if (is.call(item)) {
    return(as.call(c(list(item[[1]]), lapply(item[-1], .rxRenameRecursive, new=new, old=old))))
  } else {
    stop("unknown expression", call.=FALSE)
  }
}

.rxRename1 <- function(rxui, lst) {
  .iniDf <- rxui$iniDf
  .w <- which(.iniDf$name == lst[[4]])
  if (length(.w) == 1) {
    .iniDf$name[.w] <- lst[[3]]
    rxui$iniDf <- .iniDf
  }
  rxui$lstExpr <- lapply(seq_along(rxui$lstExpr),
                         function(i) {
                           .rxRenameRecursive(rxui$lstExpr[[i]], new=lst[[1]], old=lst[[2]])
                         })
}

#' Rename items inside of a `rxode2` ui model
#'
#' `rxRename()` changes the names of individual variables, lhs, and ode states using
#' `new_name = old_name` syntax
#'
#' @param rxui rxode2 ui function
#' @param ... rename items
#' @param envir Environment for evaluation
#' @return New model with items renamed
#' @author Matthew L. Fidler
#' @export
#' @examples
rxRename <- function(rxui, ..., envir=parent.frame()) {
  rxui <- assertRxUi(rxui)
  .vars <- unique(c(rxui$mv0$state, rxui$mv0$params, rxui$mv0$lhs, rxui$predDf$var, rxui$predDf$cond, rxui$iniDf$name))
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  .lst <- lapply(seq_along(.modelLines), function(i) {
    .assertRenameErrorModelLine(.modelLines[[i]], .vars)
  })
  rxui <- .copyUi(rxui) # copy ui so effects do not affect original
  lapply(seq_along(.lst), function(i) {
    .rxRename1(rxui, .lst[[i]])
  })
  rxui$fun()
}
