#' This function asserts the requested rename makes sense
#'
#' It returns the new expression, old expression, new variable and old
#' variable
#'
#' @param line quoted call information line
#'
#' @param vars Variables contained within the mdel
#'
#' @return list(new, old, newChar, oldChar)
#'
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
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
    stop("to rename a variable you need to use '", .var.name, "=oldName' syntax, where oldName is a variable",
         call.=FALSE)
  } else {
    .var.name2 <- as.character(line[[3]])
  }
  if (.var.name %in% vars) {
    stop("the new variable '", .var.name, "' is already present in the model; cannot replace '", .var.name2, "' with '",
         .var.name, "'",
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
.rxRenameRecursive <- function(item, new, old, isLhs=FALSE) {
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
    if (isLhs && identical(item[[1]], quote(`/`))) {
      # handle d/dt() differently so that d doesn't get renamed
      .num <- item[[2]]
      .denom <- item[[3]]
      if (is.call(.num)) .num <- as.call(lapply(.num, .rxRenameRecursive, new=new, old=old, isLhs=TRUE))
      if (is.call(.denom)) .denom <- as.call(lapply(.denom, .rxRenameRecursive, new=new, old=old, isLhs=TRUE))
      return(as.call(c(list(item[[1]]), .num, .denom)))
    } else if (isLhs && identical(item[[1]], old) && length(item) == 2L &&
                 is.numeric(item[[2]])) {
      # handle x(0) = items
      return(as.call(c(new, lapply(item[-1], .rxRenameRecursive, new=new, old=old, isLhs=isLhs))))
    }
    if (identical(item[[1]], quote(`=`)) ||
          identical(item[[1]], quote(`<-`)) ||
          identical(item[[1]], quote(`~`))) {
      .elhs <- lapply(item[c(-1, -3)], .rxRenameRecursive, new=new, old=old, isLhs=TRUE)
      .erhs <- lapply(item[c(-1, -2)], .rxRenameRecursive, new=new, old=old, isLhs=FALSE)
      return(as.call(c(item[[1]], .elhs, .erhs)))
    } else {
      return(as.call(c(list(item[[1]]), lapply(item[-1], .rxRenameRecursive, new=new, old=old, isLhs=isLhs))))
    }
  } else {
    stop("unknown expression", call.=FALSE)
  }
}
#' Rename one item in the rxui
#'
#' @param rxui rxui for renaming
#' @param lst list with (new, old, newChr, oldChr)
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
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
#' @param .data rxode2 ui function, named data to be consistent with `dplyr::rename()`
#'
#' @param ... rename items
#'
#' @param envir Environment for evaluation
#'
#' @return New model with items renamed
#'
#' @author Matthew L. Fidler
#'
#' @details
#'
#' This is similar to `dplyr`'s `rename()` function.  When `dplyr` is
#' loaded, the `s3` methods work for the ui objects.
#'
#' Note that the `.rxRename()` is the internal function that is called
#' when renaming and is likely not what you need to call unless you
#' are writing your own extension of the function
#'
#' @export
#'
#' @examples
#'
#' ocmt <- function() {
#'    ini({
#'      tka <- exp(0.45) # Ka
#'      tcl <- exp(1) # Cl
#'      ## This works with interactive models
#'      ## You may also label the preceding line with label("label text")
#'      tv <- exp(3.45) # log V
#'      ## the label("Label name") works with all models
#'      add.sd <- 0.7
#'    })
#'    model({
#'      ka <- tka
#'      cl <- tcl
#'      v <- tv
#'      d/dt(depot) = -ka * depot
#'      d/dt(center) = ka * depot - cl / v * center
#'      cp = center / v
#'      cp ~ add(add.sd)
#'    })
#' }
#'
#' ocmt %>% rxRename(cpParent=cp)
#'
rxRename <- function(.data, ..., envir=parent.frame()) {
  UseMethod("rxRename")
}

#' @rdname rxRename
#' @export
.rxRename <- function(.data, ..., envir=parent.frame()) {
  rxui <- assertRxUi(.data)
  .vars <- unique(c(rxui$mv0$state, rxui$mv0$params, rxui$mv0$lhs, rxui$predDf$var, rxui$predDf$cond, rxui$iniDf$name))
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  .lst <- lapply(seq_along(.modelLines), function(i) {
    .assertRenameErrorModelLine(.modelLines[[i]], .vars)
  })
  rxui <- .copyUi(rxui) # copy ui so effects do not affect original
  lapply(seq_along(.lst), function(i) {
    .rxRename1(rxui, .lst[[i]])
  })
  .ret <- rxui$fun()
  if (inherits(.data, "rxUi")) {
    .x <- rxUiDecompress(.data)
    .ret <- .newModelAdjust(.ret, .x, rename=TRUE)
    .ret <- rxUiCompress(.ret)
    .cls <- setdiff(class(.data), class(.ret))
    if (length(.cls) > 0) {
      class(.ret) <- c(.cls, class(.ret))
    }
  }
  .ret
}
#' @rdname rxRename
rename.rxUi <- function(.data, ...) {
  .lst <- as.list(match.call()[-1])
  .lst$.data <- .data
  do.call(.rxRename, c(.lst, list(envir=parent.frame(2))))
}
#' @rdname rxRename
rename.function <- function(.data, ...) {
  .lst <- as.list(match.call()[-1])
  .lst$.data <- .data
  do.call(.rxRename, c(.lst, list(envir=parent.frame(2))))
}
#' @export
#' @rdname rxRename
rxRename.rxUi <- function(.data, ...) {
  .lst <- as.list(match.call()[-1])
  .lst$.data <- .data
  do.call(.rxRename, c(.lst, list(envir=parent.frame(2))))
}
#' @export
#' @rdname rxRename
rxRename.function <- function(.data, ...) {
  .lst <- as.list(match.call()[-1])
  .lst$.data <- .data
  do.call(.rxRename, c(.lst, list(envir=parent.frame(2))))
}
#' @export
#' @rdname rxRename
rxRename.default <- function(.data, ...) {
  .lst <- as.list(match.call()[-1])
  .lst$.data <- .data
  do.call(.rxRename, c(.lst, list(envir=parent.frame(2))))
} 
