#' Search within a model to replace part of the model
#'
#' @param object function specifying the nlmixr2 model
#' @param find,replace Character scalars of parts of the model to replace
#' @return \code{object} with \code{find} replaced with \code{replace}
#' @keywords internal
#' @family Parameter and naming building blocks
#' @export
searchReplace <- function(object, find, replace) {
  # ensure that it is an rxode2 object
  object <- rxode(object)
  if (is.character(find)) {
    find <- str2lang(find)
  }
  if (is.character(replace)) {
    replace <- str2lang(replace)
  }
  objFunction <- object$fun
  if (is.null(objFunction)) {
    objFunction <- object$ui$fun
  }
  if (is.null(objFunction)) {
    stop("Could not extract the function from the object")
  }
  searchReplaceHelper(object = objFunction, find = find, replace = replace)
}

#' @describeIn searchReplace A helper function for searchReplace (not intended
#'   for users to use directly)
#' @export
searchReplaceHelper <- function(object, find, replace) {
  UseMethod("searchReplaceHelper")
}

#' @export
searchReplaceHelper.function <- function(object, find, replace) {
  methods::functionBody(object) <-
    searchReplaceHelper(
      object = methods::functionBody(object),
      find = find,
      replace = replace
    )
  object
}

#' @export
searchReplaceHelper.call <- function(object, find, replace) {
  if (identical(object[[1]], as.name("ini"))) {
    # no replacement within ini()
    return(object)
  } else {
    if (identical(object, find)) {
      return(replace)
    } else {
      for (idx in seq_along(object)) {
        object[[idx]] <- searchReplaceHelper(object[[idx]], find, replace)
      }
    }
  }
  object
}

#' @export
searchReplaceHelper.default <- function(object, find, replace) {
  if (identical(object, find)) {
    object <- replace
  } else if (length(object) != 1) {
    for (idx in seq_along(object)) {
      object[[idx]] <- searchReplaceHelper(object[[idx]], find = find, replace = replace)
    }
  }
  # if the length is 1 and it is not identical, then do nothing
  object
}
