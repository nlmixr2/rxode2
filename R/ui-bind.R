#' Append two rxui models together
#'
#' @param model1 rxUi model 1
#' @param model2 rxUi model 2
#' @param ... Models sent (in order) to `rxAppendModel` to
#'   concatenate/bind the models together
#' @return New model with both models appended together
#' @author Matthew L. Fidler
#' @export
#' @examples
#' ocmt <- function() {
#'    ini({
#'      tka <- exp(0.45) # Ka
#'      tcl <- exp(1) # Cl
#'      tv <- exp(3.45); # log V
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
#'  }
#'
#' idr <- function() {
#'   ini({
#'      tkin <- log(1)
#'      tkout <- log(1)
#'      tic50 <- log(10)
#'      gamma <- fix(1)
#'      idr.sd <- 1
#'    })
#'   model({
#'      kin <- exp(tkin)
#'      kout <- exp(tkout)
#'      ic50 <- exp(tic50)
#'      d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma))
#'      eff ~ add(idr.sd)
#'   })
#' }
#'
#' rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)
#'
#' # You may also use c(...) or rbind(...) but this requires the first
#' # argument to be an compiled/evaluated rxode2 model.  This can be
#' # done by calling the function like 'ocmt()', or evaluating within
#' # rxode2 ('rxode2(ocmt)') or piping /modifying it
#'
#' c(ocmt %>% model(ceff=cp,append=TRUE), idr)
#'
#' rbind(ocmt %>% model(ceff=cp,append=TRUE), idr)
#'
rxAppendModel <- function(model1, model2) {
  model1 <- assertRxUi(model1)
  model1 <- .copyUi(model1) # so modifications do not affect first model
  model2 <- assertRxUi(model2)
  .ini1 <- model1$iniDf
  .ini2 <- model2$iniDf
  .bind <- intersect(model1$mv0$lhs, model2$allCovs)
  if (length(.bind) == 0) {
    stop("the first model does not have variables that are used by the second model",
         .call=FALSE)
  }
  .maxTheta <- suppressWarnings(max(.ini1$ntheta, na.rm=TRUE))
  if (!is.finite(.maxTheta)) {
    stop("there needs to be at least one population parameter in 'model1'",
         call.=FALSE)
  }
  .ini2$ntheta <- .ini2$ntheta + .maxTheta
  .maxEta <- suppressWarnings(max(.ini1$neta1, na.rm=TRUE))
  if (is.finite(.maxEta)) {
    .ini2$neta1 <- .ini12$neta1 + .maxEta
    .ini2$neta2 <- .ini12$neta2 + .maxEta
  }
  .ini <- rbind(.ini1, .ini2)
  .etas <- which(is.na(.ini$ntheta))
  if (length(.etas) > 0) {
    .iniT <- .ini[-etas, ]
    .iniT <- .iniT[order(.iniT$ntheta), ]
    .iniE <- .ini[.etas, ]
    .iniE <- .iniE[order(.iniE$neta1, .iniE$neta2), ]
    .ini <- rbind(.iniT, .iniE)
  } else {
    .ini <- .ini[order(.ini$ntheta), ]
  }
  # Add the meta information from model2 into the meta information of new model
  .ls <- ls(model2$meta, all.names=TRUE)
  for (.i in seq_along(.ls)) {
    assign(.ls[.i], model2$meta[[.ls[.i]]], envir=model1$meta)
  }
  model1$iniDf <- .ini
  model1$lstExpr <- c(model1$lstExpr, model2$lstExpr)
  model1$fun()
}


#' @rdname rxAppendModel
#' @export
c.rxUi <- function(...) {
  .list <- list(...)
  .m1 <- .list[[1]]
  .list <- .list[-1]
  for (i in seq_along(.list)) {
    .m1 <- rxAppendModel(.m1, .list[[i]])
  }
  .m1
}

#' @rdname rxAppendModel
#' @export
rbind.rxUi <- function(..., deparse.level = 1) {
  if (!missing(deparse.level)) warning("'deparse.level' not used with rxode2 ui models", call. = FALSE)
  do.call(c.rxUi, list(...))
}
