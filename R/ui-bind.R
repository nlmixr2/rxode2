#' Append two rxUi models together
#'
#' @param model1,model2 Two rxUi models to combine
#' @param requireShared Require at least one shared variable between model 1 and
#'   2
#' @return New model with both models appended together
#' @author Matthew L. Fidler
#' @export
#' @examples
#'
#' \donttest{
#'
#' ocmt <- function() {
#'   ini({
#'     tka <- exp(0.45); label("Absorption rate")
#'     tcl <- exp(1); label("Clearance")
#'     tv <- exp(3.45); label("Volume of distribution")
#'     add.sd <- 0.7; label("Additive residual error")
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
#' rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)
#' }
rxAppendModel <- function(model1, model2, requireShared = TRUE) {
  model1 <- assertRxUi(model1)
  model1 <- .copyUi(model1) # so modifications do not affect first model
  model2 <- assertRxUi(model2)
  model2 <- .copyUi(model2)
  .ini1 <- model1$iniDf
  .ini2 <- model2$iniDf
  .bind <- intersect(c(model1$mv0$lhs, model1$mv0$state), model2$allCovs)
  if (requireShared & length(.bind) == 0) {
    stop("the first model does not have variables that are used by the second model (set requireShared=FALSE to allow this)",
         call.=FALSE)
  }
  .maxTheta <- suppressWarnings(max(.ini1$ntheta, na.rm=TRUE))
  if (!is.finite(.maxTheta)) {
    stop("there needs to be at least one population parameter in 'model1'",
         call.=FALSE)
  }
  .ini2$ntheta <- .ini2$ntheta + .maxTheta
  .maxEta <- suppressWarnings(max(.ini1$neta1, na.rm=TRUE))
  if (is.finite(.maxEta)) {
    .ini2$neta1 <- .ini2$neta1 + .maxEta
    .ini2$neta2 <- .ini2$neta2 + .maxEta
  }
  .ini <- rbind(.ini1, .ini2)
  .etas <- which(is.na(.ini$ntheta))
  if (length(.etas) > 0) {
    .iniT <- .ini[-.etas, ]
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
