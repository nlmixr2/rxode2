## nocov start
#' This creates the list of "blessed" rxode2 items
#'  
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.createRxUiBlessedList <- function() {
  message("querying default rxode2 object contents")
  .f <- function() {
    ini({
      tka <- log(1.57); label("Ka")
      tcl <- log(2.72); label("Cl")
      tv <- log(31.5); label("V")
      tv2 <- 3
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      v2 <- tv2
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl / v * center
      cp = center / v
      cp ~ add(add.sd)
    })
  }
  .f <- .f()
  .f <- rxUiDecompress(.f)
  .blessed <- ls(.f, all=TRUE)
  .blessed <- deparse(str2lang(paste0(".rxUiBlessed <- ",
                      paste(deparse(.blessed), collapse="\n"))))
  writeLines(c("## created by .createRxUiBlessedList() in function-body.R edit there",
               .blessed), devtools::package_file("R/rxUiBlessed.R"))
  message("saved!")
  invisible("")
}
## nocov end

#setOldClass("rxUi")
#' This gets the dropped items if a significant item changed
#'  
#' @param model uncompressed model to check
#' @return Character vector of items to be dropped
#' @noRd
#' @author Matthew L. Fidler
.getDropEnv <- function(model) {
  .lsModel <- ls(envir=model)
  setdiff(.lsModel, c(.rxUiBlessed, model$sticky))
}
#' This gets the additional items kept if a significant item changed
#'  
#' @param model uncompressed model to check
#' @return Character vector of additional items to be kept
#' @noRd
#' @author Matthew L. Fidler
.getKeepEnv <- function(model) {
  .lsModel <- ls(envir=model)
  .ret <- setdiff(.lsModel, .rxUiBlessed)
  .ret[.ret %in% model$sticky]
}

## .bodySetRxUi <- function(fun, envir = parent.frame(), value) {
##   if (is.function(value)) {
##     value <- body(value)
##   }
##   .model <- rxode2::rxUiDecompress(fun)
##   .clsModel <- class(.model)
##   .modelFun <- .model$fun # don't use as-function to avoid environ issues
##   body(.modelFun) <- value
##   .ret <- rxUiDecompress(rxode2(.modelFun))
##   .drop <- .getDropEnv(.model)
##   .keep <- .getKeepEnv(.model)
##   lapply(.keep, function(v) {
##     assign(v, get(v, envir=.model), envir=.ret)
##   })
##   if (length(.keep) > 0) {
##     cli::cli_alert(sprintf("kept in model: '%s'",
##                            paste(paste0("$", .keep), collapse="', '")))
##   }  
##   if (length(.drop) > 0) {
##     cli::cli_alert(sprintf("removed from model: '%s'",
##                            paste(paste0("$", .drop), collapse="', '")))
##   }
##   if (inherits(.model, "raw")) {
##     .ret <- rxode2::rxUiCompress(.ret)
##   }
##   class(.ret) <- .clsModel
##   .ret
## }

#' Set the function body of an rxUi object while retaining other object
#' information (like data)
#'
#' @param fun The rxUi object
#' @return The function body (see `base::body`)
#' @eval .createRxUiBlessedList()
#' @examples
#' 
#' one.compartment <- function() {
#'   ini({
#'     tka <- log(1.57); label("Ka")
#'     tcl <- log(2.72); label("Cl")
#'     tv <- log(31.5); label("V")
#'     eta.ka ~ 0.6
#'     eta.cl ~ 0.3
#'     eta.v ~ 0.1
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv + eta.v)
#'     d/dt(depot) = -ka * depot
#'     d/dt(center) = ka * depot - cl / v * center
#'     cp = center / v
#'     cp ~ add(add.sd)
#'   })
#' }
#' 
#' two.compartment <- function() {
#'   ini({
#'     lka <- 0.45 ; label("Absorption rate (Ka)")
#'     lcl <- 1 ; label("Clearance (CL)")
#'     lvc  <- 3 ; label("Central volume of distribution (V)")
#'     lvp  <- 5 ; label("Peripheral volume of distribution (Vp)")
#'     lq  <- 0.1 ; label("Intercompartmental clearance (Q)")
#'     propSd <- 0.5 ; label("Proportional residual error (fraction)")
#'   })
#'   model({
#'     ka <- exp(lka)
#'     cl <- exp(lcl)
#'     vc <- exp(lvc)
#'     vp <- exp(lvp)
#'     q  <- exp(lq)
#'     kel <- cl/vc
#'     k12 <- q/vc
#'     k21 <- q/vp
#'     d/dt(depot) <- -ka*depot
#'     d/dt(central) <-  ka*depot - kel*central - k12*central + k21*peripheral1
#'     d/dt(peripheral1) <- k12*central - k21*peripheral1
#'     cp <- central / vc
#'     cp ~ prop(propSd)
#'   })
#' }
#' 
#' ui <- rxode2(one.compartment)
#' 
#' body(ui) <- two.compartment
#' 

