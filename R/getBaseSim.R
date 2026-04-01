#' Get the base simulation model for simulation
#'
#' @param obj Fit Object
#' @return Simulation object
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
getBaseSimModel <- function(obj) {
  UseMethod("getBaseSimModel")
}

#' @rdname getBaseSimModel
#' @export
getBaseSimModel.default <- function(obj) {
  .ui <- assertRxUi(obj)
  .ret <- rxCombineErrorLines(.ui)
  if (identical(.ret[[2]][[2]], str2lang("params()"))) {
    .ret[[2]] <- .ret[[2]][-2]
  }
  .ret
}

#' Get the symengine for loading into symengine with `rxS()`
#'
#' @param obj Object
#' @return Simulation model ready to load into symeng
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
getBaseSymengineModel <- function(obj) {
  UseMethod("getBaseSymengineModel")
}

#'@export
getBaseSymengineModel.default <- function(obj) {
  .ui <- assertRxUi(obj)
  .x <- rxCombineErrorLines(.ui, cmtLines=FALSE, dvidLine=FALSE)
  if (identical(.x[[2]][[2]][[1]], quote(`params`))) {
    .x[[2]] <- .x[[2]][-2]
  }
  .x
}

#' Get the base simulation model for simulation with inis in the
#' underlying rxode2 model
#'
#' @param obj Fit Object
#' @return Simulation object
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
getBaseIniSimModel <- function(obj) {
  UseMethod("getBaseIniSimModel")
}

#' @rdname getBaseSimModel
#' @export
getBaseIniSimModel.default <- function(obj) {
  .ui <- assertRxUi(obj)
  .ret <- rxCombineErrorLines(.ui)
  .params <- .ret[[2]][[2]]
  .iniDf <- obj$iniDf
  .iniDf <- .iniDf[(is.na(.iniDf$neta1) | .iniDf$neta1 == .iniDf$neta2),]
  .ini <- lapply(seq_along(.iniDf$name), function(i) {
    if (!is.na(.iniDf$neta1[i])) {
      return(str2lang(paste0(.iniDf$name[i], "<- 0")))
    } else {
      return(str2lang(paste0(.iniDf$name[i], "<- ", .iniDf$est[i])))
    }
  })
  .predDf <- .ui$predDf
  if (is.null(.predDf)) {
    .sigma <- NULL
  } else {
    .sigma <- lapply(seq_along(.predDf$var), function(i) {
      if (.predDf$distribution[i] %in% c("dnorm",  "norm")) {
        str2lang(paste0("rxerr.", .predDf$var[i], "<- 1"))
      } else {
        NULL
      }
    })
    .w <- which(vapply(seq_along(.sigma),
                       function(i) {
                         !is.null(.sigma[[i]])
                       }, logical(1), USE.NAMES = FALSE))
    if (length(.w) == 0) {
      .sigma <- NULL
    } else {
      .sigma <- lapply(.w, function(i) {.sigma[[i]]})
    }
  }
  .mod <- lapply(seq_along(.ret[[2]])[-(1:2)], function(i){.ret[[2]][[i]]})
  if (identical(.params, str2lang("params()"))) {
    .params <- NULL
  }
  # now filter out the interpolation methods
  .mod <- .rxFilterOutPropsAndAdjustPredDf(.ui, predDf=NULL, lstExpr=.mod)
  .interp <- rxUiGet.interpLines(list(.ui))
  as.call(c(list(quote(`rxode2`)),
            as.call(c(list(quote(`{`)), .params, .interp, .sigma, .ini, .mod))))
}
