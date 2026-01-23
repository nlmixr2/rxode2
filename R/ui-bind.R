#' Combine Model Lines
#'
#' @param model1 rxUi model1
#' @param model2 rxUi model2
#' @param ini Final ini for model
#' @return new model with both models appended together and ini from input
#' @noRd
#' @author Matthew L. Fidler
.combineModelLines <- function(model1, model2, ini) {
  # Add the meta information from model2 into the meta information of new model
  .ls <- ls(model2$meta, all.names=TRUE)
  for (.i in seq_along(.ls)) {
    assign(.ls[.i], model2$meta[[.ls[.i]]], envir=model1$meta)
  }
  model1$iniDf <- ini
  model1$lstExpr <- c(model1$lstExpr, model2$lstExpr)
  model1$fun()
}
#' Append 2 models
#'
#'
#' @param model1 rxUi type of model
#' @param model2 rxUi type of model
#' @param common boolean; when `TRUE` require models to have variables in common
#' @return rxUi combined model of model1 and model2
#' @noRd
#' @author Matthew L. Fidler
rxAppendModel_ <- function(model1, model2, common=TRUE) {
  model1 <- assertRxUi(model1)
  model1 <- .copyUi(model1) # so modifications do not affect first model
  model2 <- assertRxUi(model2)
  model2 <- .copyUi(model2)
  .ini1 <- model1$iniDf
  .ini2 <- model2$iniDf
  .bind <- intersect(c(model1$mv0$lhs, model1$mv0$state), model2$allCovs)
  if (common && length(.bind) == 0) {
    stop("not all the models have variables in common (use `common=FALSE` to allow this)",
         call.=FALSE)
  }
  if (is.null(.ini1) && is.null(.ini2)) {
    return(.combineModelLines(model1, model2, NULL))
  }
  if (!is.null(.ini1) && is.null(.ini2)) {
    return(.combineModelLines(model1, model2, .ini1))
  }
  if (is.null(.ini1) && !is.null(.ini2)) {
    return(.combineModelLines(model1, model2, .ini2))
  }
  # both exist
  .ini1theta <- .ini1[!is.na(.ini1$ntheta),, drop = FALSE]
  .ini2theta <- .ini2[!is.na(.ini2$ntheta),, drop = FALSE]
  .both <- intersect(.ini1theta$name, .ini2theta$name)
  if (length(.both) > 0) {
    .minfo("duplicated population parameters when combining 2 models")
    .minfo(paste0("keeping initialization from first model: '",
                  paste(.both, collapse="', '"), "'"))
    .ini2theta <- .ini2theta[!.in(.ini2theta$name, .both),, drop =FALSE]
  }
  .ini2theta$ntheta <- length(.ini1theta$ntheta) + seq_along(.ini2theta$ntheta)
  .iniT <- rbind(.ini1theta, .ini2theta)
  # now look at the etas
  .ini1eta <- .ini1[is.na(.ini1$ntheta),, drop = FALSE]
  .ini2eta <- .ini2[is.na(.ini2$ntheta),, drop = FALSE]
  .both <- intersect(.ini1eta$name, .ini2eta$name)
  if (length(.both) > 0) {
    # See if any of the items have covariances defined
    .complex1 <- which(vapply(.both, function(v) {
      .eta <- .ini1eta[.ini1eta$name == v, "neta1"]
      any((.ini1eta$neta1 == .eta & .ini1eta$neta2 != .eta) |
            (.ini1eta$neta2 == .eta & .ini1eta$neta1 != .eta))
    }, logical(1), USE.NAMES = FALSE))
    .complex2 <- which(vapply(.both, function(v) {
      .eta <- .ini2eta[.ini2eta$name == v, "neta1"]
      any((.ini2eta$neta1 == .eta & .ini2eta$neta2 != .eta) |
            (.ini2eta$neta2 == .eta & .ini2eta$neta1 != .eta) )
    }, logical(1), USE.NAMES = FALSE))
    .err <- unique(c(.both[.complex1], .both[.complex2]))
    if (length(.err) > 0) {
      stop("duplicated parameter has covariance, will not append models: '",
           paste0(.err, collapse="', '"), "'",
           call.=FALSE)
    } else {
      # drop in the second
      .minfo("duplicated eta parameters when combining 2 models")
      .minfo(paste0("keeping initialization from first model: '",
                    paste(.both, collapse="', '"), "'"))

      .ini2eta <- .ini2eta[!.in(.ini2eta$name, .both),, drop =FALSE]
    }
  }
  .maxEta <- suppressWarnings(max(.ini1eta$neta1))
  if (is.finite(.maxEta)) {
    .ini2eta$neta1 <- .ini2eta$neta1 + .maxEta
    .ini2eta$neta2 <- .ini2eta$neta2 + .maxEta
  }
  .iniE <- rbind(.ini1eta, .ini2eta)
  if (length(.iniE$name) > 0) {
    .iniE <- .iniE[order(.iniE$neta1, .iniE$neta2), ]
    .ini <- rbind(.iniT, .iniE)
  } else {
    .ini <- .iniT[order(.iniT$ntheta), ]
  }
  .combineModelLines(model1, model2, .ini)
}

#' Append two rxui models together
#'
#' @param ... models to append together
#' @param common boolean that determines if you need a common value to bind
#' @return New model with both models appended together
#' @author Matthew L. Fidler
#' @export
#' @examples
#'
#' \donttest{
#'
#' ocmt <- function() {
#'   ini({
#'     tka <- exp(0.45) # Ka
#'     tcl <- exp(1) # Cl
#'     tv <- exp(3.45); # log V
#'     ## the label("Label name") works with all models
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- tka
#'     cl <- tcl
#'     v <- tv
#'     d/dt(depot) <- -ka * depot
#'     d/dt(center) <- ka * depot - cl / v * center
#'     cp <- center / v
#'     cp ~ add(add.sd)
#'   })
#' }
#'
#' idr <- function() {
#'   ini({
#'     tkin <- log(1)
#'     tkout <- log(1)
#'     tic50 <- log(10)
#'     gamma <- fix(1)
#'     idr.sd <- 1
#'   })
#'   model({
#'     kin <- exp(tkin)
#'     kout <- exp(tkout)
#'     ic50 <- exp(tic50)
#'     d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma))
#'     eff ~ add(idr.sd)
#'   })
#' }
#'
#' rxAppendModel(ocmt |>
#'   model(ceff=cp,append=TRUE), idr)
#'
#' }
#'
rxAppendModel <- function(..., common=TRUE) {
  .env <- new.env(parent=emptyenv())
  .env$ret <- NULL
  .lst <- list(...)
  lapply(seq_along(.lst), function(i) {
    .m <- .lst[[i]]
    if (is.null(.env$ret)) {
      .env$ret <- .m
    } else {
      .env$ret <- rxAppendModel_(.env$ret, .m, common=common)
    }
  })
  .env$ret
}
