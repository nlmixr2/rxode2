.rbind2rxSove <- function(v, v2) {
  .tmp <- v$add.dosing # make sure info is calculated
  .tmp <- v2$add.dosing
  .cls <- class(v)
  .env1 <- attr(class(v), ".rxode2.env")
  .env2 <- attr(class(v2), ".rxode2.env")
  if (length(.env1$.check.names) != length(.env2$.check.names) &&
        !all(.env1$.check.names == .env2$.check.names)) {
    stop("cannot rbind these 2 rxSolve objects", call.=FALSE)
  }
  ## if (is.null(.env1$.params.single) || is.null(.env2$.params.single)) {
  ##   stop("cannot rbind single solve environments", call.=FALSE)
  ## }
  .cloneEnv <- new.env(parent=emptyenv())
  for (.v in ls(.env1, all=TRUE)) {
    assign(.v, get(.v, envir=.env1), envir=.cloneEnv)
  }
  .nStud1 <- .cloneEnv$.args$nStud
  .nStud2 <- .env2$.args$nStud
  if (.nStud1 > 1 && .nStud2 > 1) {
    .args <- .cloneEnv$.args
    .args$nStud <- .nStud1 + .nStud2
    .cloneEnv$.args <- .args
    .v <- v
    class(.v) <- "data.frame"
    .v2 <- v2
    .v2$sim.id <- .nStud1 + .v2$sim.id
    class(.v2) <- "data.frame"
    .v <- rbind(.v, .v2)
    .cloneEnv$.nsub <- .cloneEnv$.nsub + .env2$.nsub
    .cloneEnv$.et <- NULL
    .cloneEnv$.args.params <- NULL
    .cloneEnv$.args.inits <- NULL
    .cloneEnv$.args.object <- rxModelVars(.cloneEnv$.args.object)
    .cloneEnv$.args.par0 <- NULL
    .cloneEnv$.args.params <- NULL
    .cloneEnv$.check.nrow <- .cloneEnv$.check.nrow + .env2$.check.nrow
    .cloneEnv$nobs <- .cloneEnv$nobs + .env2$nobs
    .cloneEnv$.nsim <- .cloneEnv$.nsim + .env2$.nsim
    .cloneEnv$.nsub <- NULL
    .cloneEnv$.dadt.counter <- 0L
    .cloneEnv$.init.dat <- setNames(rep(NA_real_, length(.cloneEnv$.init.dat)),
                                    names(.cloneEnv$.init.dat))
    .cloneEnv$.jac.counter <- 0L
    .cloneEnv$.nsub <- NA_integer_
    .cloneEnv$.par.pos <- NULL
    .cloneEnv$.par.pos.ini <- NULL
    .pd <- .env2$.params.dat
    .pd$sim.id <- .nStud1 + .pd$sim.id

    .cloneEnv$.params.dat <- rbind(.cloneEnv$.params.dat, .pd)
    .cloneEnv$.params.single <- NULL
    .cloneEnv$.par.pos <- NULL
    .cloneEnv$.par.pos.ini <- NULL
    .cloneEnv$.et <- NULL
    if (inherits(.v$id, "factor")) {
      .cloneEnv$.idLevels <- levels(.v$id)
    }
    .cloneEnv$.jac.counter <- .cloneEnv$.slvr.counter <- 0L
    .cloneEnv$.nsim <- .cloneEnv$.nsim + .env2$.nsim
    .cloneEnv$.real.update <- FALSE
    .cloneEnv$.sigma <- NULL
    .fun <- function(...) {
      stop("functions don't work on rbound rxSolve", call.=FALSE)
    }
    .cloneEnv$.replace.sampling <- .fun
    .cloneEnv$add.dosing <- .fun
    .cloneEnv$add.sampling <- .fun
    .cloneEnv$clear.dosing <- .fun
    .cloneEnv$clear.sampling <- .fun
    .cloneEnv$get.dosing <- .fun
    .cloneEnv$get.EventTable <- .fun
    .cloneEnv$get.nobs <- .fun
    .cloneEnv$get.obs.rec <- .fun
    .cloneEnv$get.sampling <- .fun
    .cloneEnv$get.units <- .fun
    .cloneEnv$import <- .fun
    .cloneEnv$counts.EventTable <- NULL
    .cloneEnv$get.units <- NULL
    .cloneEnv$units <- c(dosing="NA", time="NA")
    .cloneEnv$dll <- NULL
    attr(.cls, ".rxode2.env") <- .cloneEnv
    class(.v) <- .cls
    return(.v)
  }
}

#' @export
rbind.rxSolve <- function(..., deparse.level = 1) {
  .lst <- list(...)
  if (length(.lst) >= 2) {
    .ret <- .rbind2rxSove(.lst[[1]], .lst[[2]])
    if (length(.lst) == 2) {
      return(.ret)
    }
    return(do.call(rbind.rxSolve,
                   c(list(.ret),
                     lapply(seq_along(.lst)[-(1:2)],
                            function(i){
                              .lst[[i]]
                            }))))
  }
  if (length(.lst) == 1) return(.lst[[1]])
  stop("called rbind.rxSolve() with no arguments",
       call.=FALSE)
}
