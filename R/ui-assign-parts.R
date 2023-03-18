## nocov start
#' This creates the list of "blessed" rxode2 items
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.createRxUiBlessedList <- function() {
  message("querying default rxode2 object contents")
  tka <- log(1.57)
  tcl <- log(2.72)
  tv <- log(31.5)
  tv2 <- 3
  eta.ka <- 0.6
  eta.cl <- 0.3
  eta.v <- 0.1
  add.sd <- 0.7
  depot <- center <- NULL
  `/<-` <- function(...) {} # nolint
  dt <- function(...) {} #nolint
  .f <- function() {
    ini({
      tka <- log(1.57)
      tcl <- log(2.72)
      tv <- log(31.5)
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
  .blessed <- sort(unique(c("model", "modelName", ls(.f, all.names=TRUE))))
  .blessed <- deparse(str2lang(paste0(".rxUiBlessed <- ",
                      paste(deparse(.blessed), collapse="\n"))))
  writeLines(c("## created by .createRxUiBlessedList() in ui-assign-parts.R edit there",
               .blessed), devtools::package_file("R/rxUiBlessed.R"))
  message("saved!")
  invisible("")
}
## nocov end

#' Assign the model block in the rxode2 related object
#'
#' @param x rxode2 related object
#' @param envir Environment where assignment occurs
#' @param value Value of the object
#' @return rxode2 related object
#' @export
#' @author Matthew L. Fidler
`model<-` <- function(x, envir=environment(x), value) {
  UseMethod("model<-")
}
#' @export
`model<-.default` <- function(x, envir=environment(x), value) {
  .ret <- try(as.rxUi(x), silent = TRUE)
  if (inherits(.ret, "try-error")) {
    stop("cannot figure out what to do with model assignment", call.=FALSE)
  }
  .model <- as.model(force(value))
  .ini <- .ret$iniFun
  .fun <- function() {} # nolint
  body(.fun) <- as.call(list(quote(`{`), .ini, .model))
  rxode2(x) <- .fun
  x
}

#' Assign the ini block in the rxode2 related object
#'
#' @param x rxode2 related object
#' @param envir Environment where assignment occurs
#' @param value Value of the object
#' @return rxode2 related object
#' @export
#' @author Matthew L. Fidler
`ini<-` <- function(x, envir=environment(x), value) {
  UseMethod("ini<-")
}
#' @export
`ini<-.default` <- function(x, envir=environment(x), value) {
  .ret <- try(as.rxUi(x), silent = TRUE)
  if (inherits(.ret, "try-error")) {
    stop("cannot figure out what to do with ini assignment", call.=FALSE)
  }
  .ini <- as.ini(force(value))
  .model <- .ret$modelFun
  .fun <- function() {} # nolint
  body(.fun) <- as.call(list(quote(`{`), .ini, .model))
  rxode2(x) <- .fun
  x
}
#' This gets all the significant items in the model
#'
#' @param model uncompresseg model to check
#' @return all significant items to assign
#' @noRd
#' @author Matthew L. Fidler
.getAllSigEnv <- function(model) {
  .lsModel <- ls(envir=model)
  setdiff(.lsModel, .rxUiBlessed)
}

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
#' Checks to see if the models are nearly the same
#'
#' @param newModel New rxode2 model
#' @param oldModel old rxode2 model
#' @return boolean to say if the models are nearly the same
#' @noRd
#' @details
#'
#' Nearly the same models has the same model block, the same
#' estimates, but can change in fixed/unfixed, upper/lower limits and
#' labels.
#'
#' @author Matthew L. Fidler
.modelsNearlySame <- function(newModel, oldModel) {
  # first check to see if this is a "significant" change.
  if (identical(newModel$modelFun, oldModel$modelFun)) {
    .pre <- oldModel$iniDf[,c("name", "est")]
    names(.pre)[2] <- "estPre"
    .post <- newModel$iniDf[,c("name", "est")]
    .both <- merge(.pre, .post, all.x=TRUE, all.y=TRUE, by="name")
    if (any(is.na(.both$est))) return(FALSE)
    if (any(is.na(.both$estPre))) return(FALSE)
    if (all(abs(.both$est - .both$estPre) < 1e-10)) {
      return(TRUE)
    }
  }
  FALSE
}
#' Adjust new and old model
#'
#' @param newModel new model to adjust (uncompressed)
#' @param oldModel old model to get information from (uncompressed)
#' @param rename if this operation is a rename operation, it is
#'   considered an insignificant operation
#' @return new model adjusted to match oldModel as much as reasonable.
#'   The class needs to be adjusted and needs to be compressed
#' @noRd
#' @author Matthew L. Fidler
.newModelAdjust <- function(newModel, oldModel, rename=FALSE) {
  newModel <- rxUiDecompress(newModel)
  oldModel <- rxUiDecompress(oldModel)
  lapply(c("meta", "sticky", "model", "modelName"), function(x) {
    if (exists(x, envir=oldModel)) assign(x, get(x, envir=oldModel), envir=newModel)
  })
  if (rename || .modelsNearlySame(newModel, oldModel)) {
    lapply(.getAllSigEnv(oldModel),
           function(x) {
             assign(x, get(x, envir=oldModel), envir=newModel)
           })
    return(newModel)
  }
  .drop <- .getDropEnv(oldModel)
  .keep <- .getKeepEnv(oldModel)
  lapply(.keep, function(v) {
    assign(v, get(v, envir=oldModel), envir=newModel)
  })
  lapply(.drop, function(v) {
    if (exists(v, envir=newModel)) rm(list=v, envir=newModel)
  })
  if ( length(.drop) > 0 ) {
    cli::cli_alert("significant model change detected")
    if (length(.keep) > 0) {
      cli::cli_alert(sprintf("kept in model: '%s'",
                             paste(paste0("$", .keep), collapse="', '")))
    }
    cli::cli_alert(sprintf("removed from model: '%s'",
                           paste(paste0("$", .drop), collapse="', '")))
  }
  newModel
}

#' Set the body of the rxode2 related function
#'
#' @param fun function for setting the body
#'
#' @param envir environment where this is assigned
#'
#' @param value value that will be assigned
#'
#' @return new rxode2 function with meta information retained
#'
#' @noRd
#' @author Matthew L. Fidler & Bill Denney
.bodySetRxUi <- function(x, envir = parent.frame(), value) {
  if (is.function(value)) {
    value <- body(value)
  }
  .clsModel <- class(x)
  .model <- rxUiDecompress(x)
  .modelFun <- .model$fun # don't use as-function to avoid environment issues
  if (!inherits(.modelFun, "function")) stop("wrong input for 'x' in .bodySetRxUi", call.=FALSE)
  body(.modelFun) <- value
  .modelFun()
}

#' Set the function body of an rxUi object while retaining other object
#' information (like data)
#'
#' @param x The rxUi object
#' @param envir environment where the assignment ocurs
#' @param value the value that will be assigned
#' @return The rxode2 ui/function
#' @eval .createRxUiBlessedList()
#' @export
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
#' rxode2(ui) <- two.compartment
#'
`rxode2<-` <- function(x, envir=environment(x), value) {
  UseMethod("rxode2<-")
}
#' @rdname rxode2-set
#' @export
`rxode2<-.function` <- function(x, envir=environment(x), value) {
  .val <- force(value)
  if (inherits(.val, "{")) {
    .fun <- function() {} #nolint
    body(.fun) <- .val
    return(.fun)
  } else if (!inherits(value, "function")) {
    stop("cannot figure out how to assign this to the with rxode()<-",
         call.=FALSE)
  }
  return(force(value))
}

#' @rdname rxode2-set
#' @export
`rxode2<-.default` <- function(x, envir=environment(x), value) {
  force(value)
  .v <- value
  if (inherits(value, "function")) {
    value <- body(value)
  } else if (inherits(value, "rxUi")) {
    value <- body(as.function(value))
  } else if (!inherits(value, "{")) {
    stop("do not know how to assign this", call.=FALSE)
  }
  .ret <- .bodySetRxUi(x, envir = parent.frame(), value)
  if (inherits(x, "rxUi")) {
    .ret <- .newModelAdjust(.ret, x)
  }
  .ret <- rxUiCompress(.ret)
  if (inherits(x, "rxUi")) {
    .cls <- setdiff(class(x), class(.ret))
    if (length(.cls) > 0) {
      class(.ret) <- c(.cls, class(.ret))
    }
  }
  .ret
}

#'@rdname rxode2-set
#'@export
`rxode<-` <- function(x, envir=environment(x), value) {
  UseMethod("rxode2<-")
}

#'@rdname rxode2-set
#'@export
`RxODE<-` <- function(x, envir=environment(x), value) {
  UseMethod("rxode2<-")
}
