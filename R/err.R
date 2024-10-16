## This is a list of supported distributions with the number of arguments they currently support.
.errDist <- list(
  "dpois" = 1,
  "pois" = 1,
  "dbinom" = 1:2,
  "binom"=1:2,
  "dbern" = 1,
  "bern" = 1,
  "dbeta" = 2, # non-central isn't supported by stan so drop support
  "beta" = 2,
  "dt" = 1, # non-central isn't supported by stan, so drop
  "t" = 1,
  ##
  ## "dnbinom"=2:3,  ## dnbinom is in R; FIXME: how does ot compare to dneg_binomial
  ## "dneg_binomial", ## not in base R (but in glnmm2)
  ##
  ## Available as external package http://ugrad.stat.ubc.ca/R/library/rmutil/html/BetaBinom.html
  ## "dbetabinomial", ## not in base R (but in glnmm2)
  "add" = 1,
  "norm" = 0,
  "dnorm" = 0,
  "prop" = 1,
  "propT" = 1,
  "propF" = 2,
  "pow" = 2,
  "powT" = 2,
  "powF"=3,
  "tbs" = 1,
  "boxCox" = 1,
  "tbsYj" = 1,
  "yeoJohnson" = 1,
  "logn" = 1,
  "lnorm" = 1,
  "dlnorm" = 1,
  "dlogn" = 1,
  "logitNorm" = 1:3,
  "probitNorm" = 1:3,
  "combined1"=0,
  "combined2"=0,
  "comb1"=0,
  "comb2"=0,
  "dchisq"=1,
  "chisq"=1,
  "dexp"=0:1,
  "df"=2:3,
  "f"=2:3,
  "dgeom"=1,
  "geom"=1,
  #  "dhyper"=3,
  #  "hyper"=3,
  "dunif"=0:2,
  "unif"=0:2,
  "dweibull"=1:2,
  "weibull"=1:2,
  "cauchy"= 0,
  "dcauchy"= 0:2,
  "dgamma"=1:2,
  "nbinom"=2,
  "dnbinom"=2,
  "nbinomMu"=2,
  "dnbinomMu"=2
)

.errDistsPositive <- c("add", "prop", "propT", "pow", "powT", "logn", "dlogn", "lnorm", "dlnorm", "logitNorm", "probitNorm")

.errUnsupportedDists <- "nlmixrDist"

.errAddDists <- c("add", "prop", "propT", "propF", "norm", "pow", "powT", "powF", "dnorm", "logn", "lnorm", "dlnorm", "tbs", "tbsYj", "boxCox",
                  "yeoJohnson", "logitNorm", "probitNorm", "combined1", "combined2", "comb1", "comb2", "t", "cauchy", "norm")

.errIdenticalDists <- list(
  "lnorm"=c("logn", "dlogn", "dlnorm"),
  "dnorm"="norm",
  "boxCox"="tbs",
  "yeoJohnson"="tbsYj",
  "pois"="dpois",
  "binom"="dbinom",
  "bern"="dbern",
  "beta"="dbeta",
  "t"="dt",
  "combined1"="comb1",
  "combined2"="comb2",
  "chisq"="dchisq", #6
  "f"="df", #8
  "geom"="dgeom", #9
  "unif"="dunif", #10
  "weibull"="dweibull", #11
  "cauchy"="dcauchy", #12
  "nbinom"="dnbinom",
  "nbinomMu"="dnbinomMu")

.errDistArgRanges <- list(
  "add"=c(0, Inf),
  "prop"=c(0, Inf),
  "propT"=c(0, Inf),
  "propF"=c(0, Inf),
  "pow"=c(0, Inf),
  "powT"=c(0, Inf),
  "powF"=c(0, Inf),
  "pow2"=c(-Inf, Inf),
  "powT2"=c(-Inf, Inf),
  "powF2"=c(-Inf, Inf),
  "lnorm"=c(0, Inf),
  "boxCox"=c(-Inf, Inf),
  "yeoJohnson"=c(-Inf, Inf),
  "pois"=c(0, Inf),
  "binom"=c(0, Inf),
  "binom2"=c(0, 1),
  "nbinom"=c(0, Inf),
  "nbinom2"=c(0, 1),
  "nbinomMu"=c(0, Inf),
  "nbinomMu2"=c(0, Inf),
  "bern"=c(0, 1),
  "logitNorm"=c(0, Inf),
  "probitNorm"=c(0, Inf),
  "chisq"=c(0, Inf),
  "chisq2"=c(0, Inf),
  "dexp"=c(0, Inf),
  "f"=c(0, Inf),
  "f2"=c(0, Inf),
  "f3"=c(0, Inf),
  "geom"=c(0, 1),
  ## "hyper"=c(0, Inf),
  ## "hyper2"=c(0, Inf),
  ## "hyper3"=c(0, Inf),
  "dunif"=c(-Inf, Inf),
  "dunif2"=c(-Inf, Inf),
  "weibull"=c(0, Inf),
  "weibull2"=c(0, Inf),
  "cauchy"=c(-Inf, Inf),
  "cauchy2"=c(0, Inf),
  "dgamma"=c(0, Inf),
  "dgamma2"=c(0, Inf),
  "geom"=c(0, 1),
  "beta" = c(0, Inf),
  "beta2"=c(0, Inf),
  "t"=c(0, Inf),
  "t2"=c(0, Inf)
)

## the desired outcome for each expression is to capture the condition
## when the multiple endpoint occurs, the lower and upper for the
## transformation (if any) and the error type of the model.  The
## errors should be labeled "add" for an additive error and pow, pow2
## for the first and second argument of the power distribution.  The
## ini block should have already been processed by lotri, so we have
## some information about the model.  The predictions will be replaced
## by rxPred__# for easy replacement with the correct information for
## simulating or estimating in different nlmixr algorithms and
## different rxode2 simulation routines

## Currently you can support the following types of expressions:
##
## f ~ add(add.sd)
##
## This assumes a named DVID of f
##
## or
##
## g ~ add(add.sd) | namedDvid
##
## In this case the named DVID is namedDvid
##
## In the final rxode2 model expression these expressions will look like:
##
## rxPred__1 = f
## rxPred__2 = g
##
## With this change, there is sufficient information to send to the
## mu-referencing routine


#' Change distribution name to the preferred distribution name term
#'
#' This is determined by the internal preferred condition name list
#' `.errIdenticalDists`
#'
#' @param dist This is the input distribution
#' @return Preferred distribution term
#' @author Matthew Fidler
#'
#' @examples
#'
#' rxPreferredDistributionName("dt")
#'
#' rxPreferredDistributionName("add")
#'
#' # can be vectorized
#'
#' rxPreferredDistributionName(c("add","dt"))
#'
#' @export
rxPreferredDistributionName <- function(dist) {
  if (length(dist) == 1) {
    .names <- names(.errIdenticalDists)
    for(.n in .names) {
      if (dist == .n) return(.n)
      else if (dist %in% .errIdenticalDists[[.n]]) return(.n)
    }
    dist
  } else {
    vapply(dist, rxPreferredDistributionName, character(1))
  }
}

.incompatibleTransformations <-
  list(boxCox=c("yeoJohnson", "lnorm"),
       yeoJohnson=c("boxCox", "lnorm"),
       lnorm=c("yeoJohnson", "boxCox", "logit", "logitNorm", "probit", "probitNorm", "logit + yeoJohnson"),
       logit=c("probit", "probitNorm", "lnorm"),
       probit=c("logit", "lnorm"),
       "logit + yeoJohnson"=c("boxCox", "probit", "probitNorm", "lnorm"),
       "probit + yeoJohnson"=c("boxCox", "logit", "logitNorm", "lnorm"),
       "logit + boxCox"=c("yeoJohnson", "probit", "probitNorm", "lnorm"),
       "probit + boxCox"=c("yeoJohnson", "logit", "logitNorm", "lnorm"))

.rxTransformCombineLevels <- c(
  "boxCox", # 1
  "yeoJohnson", # 2
  "untransformed", # 3
  "lnorm", # 4
  "logit", # 5
  "logit + yeoJohnson", # 6
  "probit", # 7
  "probit + yeoJohnson", #8
  "logit + boxCox", # 9
  "probit + boxCox" # 10
)


.rxTransformHasALambdaParameter <- function(distribution) {
  (as.integer(distribution) %in% c(1L, 2L, 6L, 8L, 9L, 10L))
}

.rxTransformHasBounds <- function(distribution) {
  (as.integer(distribution) %in% 5:14)
}

.rxAddPropLevels <- c(
  "combined1", # 1
  "combined2", # 2
  "default" # 3
)

.incompatibleAddProp <- list(
  combined1="combined2",
  combined2="combined1"
)

.rxErrType <- c(
  "add", # 1
  "prop", # 2
  "pow", # 3
  "add + prop", # 4
  "add + pow", # 5
  "none" # 6
)

.rxDistributionType <- c(
  "norm",    #  1
  "pois",    #  2
  "binom",   #  3
  "beta",    #  4
  "t",       #  5
  "chisq",   #  6
  "dexp",    #  7
  "f",       #  8
  "geom",    #  9
  "hyper",   # 10; hyper will not be supported since all the inputs are integers
  "unif",    # 11
  "weibull", # 12
  "cauchy",  # 13
  "dgamma",  # 14
  "ordinal", # 15
  "LL",      # 16
  "dnorm",   # 17
  "nbinom",  # 18 size, prob
  "nbinomMu"  # 19 size, mu
)

#' Demote the error type
#'
#' @param errType Error type factor
#' @return Demoted Error Type
#' @author Matthew Fidler
#' @export
#' @examples
#' rxErrTypeCombine("add") %>%
#'   rxErrTypeCombine("prop")
#'
#' # This removes the internal additive error
#' rxErrTypeCombine("add") %>%
#'   rxErrTypeCombine("prop") %>%
#'   rxDemoteAddErr()
#'
#' # This is used for logitNorm(NA), the additive portion is stripped
#'
#' @keywords internal
rxDemoteAddErr <- function(errType) {
  if (inherits(errType, "factor")) {
    return(structure(switch(as.character(errType),
                            add=6L,
                            "add + prop"=2L,
                            "add + pow"=3L,
                            as.integer(errType)),
                     .Label =.rxErrType,
                     class="factor"))
  } else if (inherits(errType, "rxCombinedErrorList")) {
    return(.rxTransformCombineListOrChar(list(transform=errType$transform,
                                              errType=rxDemoteAddErr(errType$errType),
                                              errTypeF=errType$errTypeF,
                                              addProp=errType$addProp)))
  }
}

.incompatibleErrType <- list(prop=c("pow", "powT", "powF"),
                             propT=c("pow", "powT", "powF"),
                             propF=c("pow", "powT", "powF"),
                             pow=c("prop", "propT", "propF"),
                             powT=c("prop", "propT", "propF"),
                             powF=c("prop", "propT", "propF"))


.rxErrTypeF <- c(
  "untransformed", # 1
  "transformed", # 2
  "f", # 3
  "none")

.incompatibleErrTypeF <- list(prop=c("propT", "propF", "powT", "powF"),
                              propT=c("prop", "propF", "pow", "powF"),
                              propF=c("prop", "propT", "pow", "powT"),
                              pow=c("propF", "propT", "powF", "powT"),
                              powT=c("prop", "propF", "pow", "powF"),
                              powF=c("prop", "propT", "pow", "powT"))

.incompatibleErr <- function(err1, err2) {
  .errs <- sort(c(err1, err2))
  paste0("`", .errs[1], "` and `", .errs[2], "` are incompatible")
}
#' Combine error types to get the model F type
#'
#' @param newAddProp New error type
#' @param oldAddProp old error type
#' @return factor of the error type function OR a string indicating a syntax error
#' @author Matthew Fidler
#' @noRd
.rxCombineAddProp <- function(newAddProp, oldAddProp="default") {
  .tmp <- as.character(oldAddProp)
  .w <- which(names(.incompatibleAddProp) == .tmp)
  if (length(.w) == 1L) {
    if (newAddProp %in% .incompatibleAddProp[[.w]]) {
      return(.incompatibleErr(.tmp, newAddProp))
    }
  }
  structure(switch(newAddProp,
                   combined1 =1L,
                   combined2=2L,
                   default=3L,
                   ifelse(inherits(oldAddProp, "character"),
                          switch(oldAddProp,
                                 combined1 =1L,
                                 combined2=2L,
                                 default=3L,
                                 3L),
                          as.integer(oldAddProp))),
            .Label=.rxAddPropLevels,
            class="factor")
}

#' Combine error types to get the model F type
#'
#' @param newErrTypeF New error type
#' @param oldErrTypeF old error type
#' @return factor of the error type function OR a string indicating a syntax error
#' @author Matthew Fidler
#' @noRd
.rxCombineErrTypeF <- function(newErrTypeF, oldErrTypeF="none") {
  .tmp <- as.character(oldErrTypeF)
  .w <- which(names(.incompatibleErrTypeF) == .tmp)
  if (length(.w) == 1L) {
    if (newErrTypeF %in% .incompatibleErrTypeF[[.w]]) {
      return(.incompatibleErr(.tmp, newErrTypeF))
    }
  }
  structure(switch(newErrTypeF,
                   prop =1L,
                   propT=2L,
                   propF=3L,
                   pow=1L,
                   powT=2L,
                   powF=3L,
                   ifelse(inherits(oldErrTypeF, "character"),
                          switch(oldErrTypeF,
                                 prop =1L,
                                 propT=2L,
                                 propF=3L,
                                 pow=1L,
                                 powT=2L,
                                 powF=3L,
                                 4L),
                          as.integer(oldErrTypeF))),
            .Label=.rxErrTypeF,
            class="factor")
}


#' Combine error model
#'
#' @param newErrType New error portion combined with current proportion
#' @param oldErrType Old error information
#' @return A factor for error type OR a string with error information
#' @author Matthew Fidler
#' @noRd
.rxCombineErrType <- function(newErrType, oldErrType="none") {
  .tmp <- as.character(oldErrType)
  .w <- which(names(.incompatibleErrType) == .tmp)
  if (length(.w) == 1L) {
    if (newErrType %in% .incompatibleErrType[[.w]]) {
      return(.incompatibleErr(.tmp, newErrType))
    }
  }
  structure(switch(.tmp,
                   add=switch(newErrType,
                              prop=4L,
                              propT=4L,
                              propF=4L,
                              pow=5L,
                              powT=5L,
                              powF=5L,
                              1L),
                   prop=switch(newErrType,
                               add=4L,
                               probit=4L,
                               probitNorm=4L,
                               logit=4L,
                               logitNorm=4L,
                               lnorm=4L,
                               2L),
                   pow=switch(newErrType,
                              add=5L,
                              probit=5L,
                              probitNorm=5L,
                              logit=5L,
                              logitNorm=5L,
                              lnorm=5L,
                              3L),
                   none=structure(switch(newErrType,
                                         add=1L,
                                         lnorm=1L,
                                         logit=1L,
                                         logitNorm=1L,
                                         probit=1L,
                                         probitNorm=1L,
                                         prop=2L,
                                         propT=2L,
                                         propF=2L,
                                         pow=3L,
                                         powT=3L,
                                         powF=3L,
                                         6L)),
                   as.integer(oldErrType)),
            .Label=.rxErrType,
            class="factor")
}
#' Combine transformations
#'
#' @param newTransform New error structure added together
#' @param oldTransform Old transformation added together
#' @return A factor describing the transformation type
#' @author Matthew Fidler
#' @noRd
.rxCombineTransform <- function(newTransform, oldTransform="untransformed") {
  .tmp <- as.character(oldTransform)
  .w <- which(names(.incompatibleTransformations) == .tmp)
  if (length(.w) == 1L) {
    if (newTransform %in% .incompatibleTransformations[[.w]]) {
      return(.incompatibleErr(.tmp, newTransform))
    }
  }
  structure(switch(.tmp,
                   boxCox=switch(newTransform,
                                 logitNorm=9L,
                                 probitNorm=10L,
                                 1L),
                   yeoJohnson=switch(newTransform,
                                     logitNorm=6L,
                                     probitNorm=8L,
                                     2L),
                   logit=switch(newTransform,
                                boxCox=9L,
                                yeoJohnson=6L,
                                5L),
                   probit=switch(newTransform,
                                 boxCox=10L,
                                 yeoJohnson=8L,
                                 7L),
                   untransformed=structure(switch(newTransform,
                                                  boxCox=1L,
                                                  yeoJohnson=2L,
                                                  lnorm=4L,
                                                  logitNorm=5L,
                                                  probitNorm=7L,
                                                  3L),
                                           .Label=.rxTransformCombineLevels,
                                           class="factor"),
                   as.integer(oldTransform)),
            .Label=.rxTransformCombineLevels,
            class="factor")
}

#' This is a wrapper to make sure that the transformation combination returns the correct value
#'
#' @param inputList This is either an input list or character vector of length one
#' @return Either a complete list, or a character vector which represents the parsed error that was encountered
#' @author Matthew Fidler
#' @noRd
.rxTransformCombineListOrChar <- function(inputList) {
  if (inherits(inputList, "character")) return(inputList)
  .err  <- NULL
  for (i in names(inputList)) {
    if (inherits(inputList[[i]], "character")) {
      .err <- c(.err, inputList[[i]])
    }
  }
  if (is.null(.err)) {
    .ret <- inputList
    class(.ret) <- "rxCombinedErrorList"
    return(.ret)
  } else {
    return(paste(.err, collapse="\n"))
  }
}

#' Combine transformations and error structures
#'
#' Combine error information to figure out what transformation is
#' being applied for the current endpoint
#'
#' @param oldErrType This is the old transformation, by default is
#'   zero representing no prior transformation. This parameter is
#'   first to allow piping. When the parameter `addTransform` is
#'   missing and `oldErrType` is a character value, this functions
#'   swaps `oldErrType` and `addTransform` and assigns
#'   `oldErrType` to zero assuming that there is no prior
#'   distribution.
#'
#' @param newErrType This is the new distribution that is being
#'   "added" to the current transformation.  These assumes the inputs
#'   are in the preferred distribution name, as determined by
#'   `rxPreferredDistributionName()`
#'
#' @return The new transformation as a factor
#'
#' @author Matthew Fidler
#'
#' @examples
#'
#' rxErrTypeCombine("probitNorm")
#'
#' rxErrTypeCombine("probitNorm") %>%
#'   rxErrTypeCombine("boxCox")
#'
#'
#' @export
#' @keywords internal
rxErrTypeCombine <- function(oldErrType, newErrType) {
  if (missing(newErrType) && inherits(oldErrType, "character")) {
    return(.rxTransformCombineListOrChar(list(transform=.rxCombineTransform(oldErrType),
                                              errType=.rxCombineErrType(oldErrType),
                                              errTypeF=.rxCombineErrTypeF(oldErrType),
                                              addProp=.rxCombineAddProp(oldErrType))))
  } else if (inherits(oldErrType, "rxCombinedErrorList")) {
    return(.rxTransformCombineListOrChar(list(transform=.rxCombineTransform(newErrType, oldErrType$transform),
                                              errType=.rxCombineErrType(newErrType, oldErrType$errType),
                                              errTypeF=.rxCombineErrTypeF(newErrType, oldErrType$errTypeF),
                                              addProp=.rxCombineAddProp(newErrType, oldErrType$addProp))))
  } else {
    stop("old transform not in the proper format", call.=FALSE)
  }
}

#' Checks to see if an expression is numeric
#'
#' @param expression quoted expression
#' @param env Environment to store result in `env$.numeric`
#' @return TRUE if this is an expression containing a positive or negative expression or FALSE if it is an expression that doesn't contain an expression.
#' @author Matthew Fidler
#' @noRd
.is.numeric <- function(expression, env) {
  if (is.numeric(expression)) {
    env$.numeric <- expression
    return(TRUE)
  } else if (length(expression) == 2L) {
    if (identical(expression[[1]], quote(`-`)) &&
          is.numeric(expression[[2]])) {
      env$.numeric <- -(expression[[2]])
      return(TRUE)
    } else if (identical(expression[[1]], quote(`+`)) &&
                 is.numeric(expression[[2]])) {
      env$.numeric <- expression[[2]]
      return(TRUE)
    }
  }
  return(FALSE)
}

.allowDemoteAddDistributions <- c("lnorm", "probitNorm", "logitNorm")

.namedArgumentsToPredDf <- list(
  add="a",
  lnorm="a",
  boxCox="lambda",
  yeoJohnson="lambda",
  pow=c("b", "c"),
  powT=c("b", "c"),
  powF=c("b", "c", "f"),
  prop="b",
  propT="b",
  propF=c("b", "f"),
  t=c("d", "e"),
  pois=c("a"),
  binom=c("a", "b"),
  beta=c("a", "b",  "c"),
  chisq=c("a", "b"), #6
  dexp=c("a"), #7
  f=c("a", "b", "c"), #8
  geom=c("a"), #9
  #  hyper=c("a", "b", "c"), #10
  unif=c("a", "b"), #11
  weibull=c("a", "b"), #12
  cauchy=c("a", "b"),
  dgamma=c("a", "b"),
  nbinom=c("a", "b"),
  nbinomMu=c("a", "b")
)

.allowEstimatedParameters <- "ordinal"

#' This handles the error distribution for a single argument.
#'
#' @param argumentNumber The argument number of the distribution being processed
#' @param funName Function name string of the distribution name
#' @param expression Function expression (including the function name)
#' @param env Environment where the names and calculations are made for the user interface.
#' @return None, called for the side effects.
#' @author Matthew Fidler
#' @noRd
.errHandleSingleDistributionArgument <- function(argumentNumber, funName, expression, env) {
  .cur <- expression[[argumentNumber + 1]]
  .isLogitOrProbit <- (funName %in% c("logitNorm", "probitNorm"))
  if (is.name(.cur)) {
    .curName <- as.character(.cur)
    .w <- which(env$df$name == .curName)
    if (.isLogitOrProbit && argumentNumber > 2) {
      env$err <- c(env$err,
                   paste0("'", funName, "()' requires numeric bounds"))
    } else if (length(.w) == 1L) {
      .df  <- env$df
      .df$err[.w] <- ifelse(argumentNumber == 1, funName, paste0(funName, argumentNumber))
      if (!is.na(.df$condition[.w])) {
        assign("dupErr", c(env$dupErr, .df$name[.w]), envir=env)
      }
      .df$condition[.w] <- env$curCondition
      assign("df", .df, envir=env)
      assign("lastDistAssign", .curName, envir=env)
    } else {
      .w <- which(names(.namedArgumentsToPredDf) == funName)
      if (length(.w) == 1) {
        .curLst <- .namedArgumentsToPredDf[[.w]]
        if (argumentNumber <= length(.curLst)) {
          assign(.curLst[argumentNumber], .curName, envir=env)
          return(NULL)
        }
      }
      if (env$estNotAllowed) {
        if (!(funName %in% .allowEstimatedParameters)) {
          env$err <- c(env$err,
                       paste0("in the error expression, the variable '", .curName, "' must be estimated, not calculated"))
        }
      }
    }
  } else if (.is.numeric(.cur, env)) {
    if (.isLogitOrProbit) {
      if (argumentNumber == 2 || argumentNumber == 3) {
        env$trLimit[argumentNumber - 1] <- env$.numeric
      }
    }
  } else if (is.na(.cur)) {
    if (argumentNumber == 1 && funName %in% .allowDemoteAddDistributions) {
      env$needToDemoteAdditiveExpression <- TRUE
    } else {
      env$err <- c(env$err,
                   paste0("NA in '", funName, "()' cannot be used here"))
    }
  }
}

#' Checks and modifies the error term as needed in the user interface function.
#'
#' @param funName Name of the distributional error
#' @param expression The R expression (including the function name)
#' @param env The environment that holds information about the error structure
#' @return Nothing; Called for the side effects
#' @author Matthew Fidler
#' @noRd
.errHandleSingleDistributionTerm <- function(funName, expression, env) {
  .nargs <- length(expression) - 1
  .doIt <- FALSE
  if (funName == "ordinal") {
    if (.nargs == 0) {
      assign("err", c(env$err,
                      paste0("ordinal errors require at least 1 argument (i.e. err ~ c(err1))", envir=env)))
      return(invisible())
    }
    .doIt <- TRUE
  } else {
    .errDistArgs <- .errDist[[funName]]
    .doIt <- .nargs %in% .errDistArgs
  }
  if (.doIt) {
    if (.nargs > 0) {
      lapply(seq(1, .nargs), .errHandleSingleDistributionArgument, funName=funName, expression=expression, env=env)
    }
    env$errTypeInfo <- rxErrTypeCombine(env$errTypeInfo, funName)
    env$needsToBeAnErrorExpression <- TRUE
  } else {
    .min <- range(.errDistArgs)
    .max <- .min[2]
    .min <- .min[1]
    if (.min == .max) {
      assign("err", c(env$err,
                      paste0("`", funName, "` requires ",
                             .max, " argument(s), you specified ", .nargs)), envir=env)
    } else {
      assign("err", c(env$err,
                      paste0("`", funName, "` requires ",
                             .min, " to ", .max, " argument(s), you specified ", .nargs)),
             envir=env)
    }
    if (.nargs > 0) {
      lapply(seq(1, .nargs), .errHandleSingleDistributionArgument, funName=funName, expression=expression, env=env)
    }
  }
}
#' This handles a function that is not an error term.
#'
#' @param funName Function that is being called, as a character
#' @param expression Expression (including function)
#' @param env Environment that stores the information about errors
#' @return Nothing, called for the side effects
#' @author Matthew Fidler
#' @noRd
.errHandleSingleTerm <- function(funName, expression, env) {
  env$hasNonErrorTerm <- TRUE
}

#' Handle the error structure term
#'
#' @param expression The error structure term
#'
#' @param env The environment that holds information about the error
#'   structure
#'
#' @return Nothing, called for the side efects
#' @author Matthew Fidler
#' @noRd
.errHandleErrorStructure <- function(expression, env) {
  .isAdd <- FALSE
  if (is.name(expression) || is.atomic(expression)) {
    .isAdd <- FALSE
  } else if (identical(expression[[1]], quote(`+`))) {
    .isAdd <- TRUE
  }
  if (.isAdd) {
    env$isAnAdditiveExpression <- TRUE
    .errHandleErrorStructure(expression[[2]], env)
    .errHandleErrorStructure(expression[[3]], env)
  } else if (env$isAnAdditiveExpression) {
    .currErr <- rxPreferredDistributionName(deparse1(expression[[1]]))
    if (.currErr %in% .errAddDists) {
      if (.currErr == "t") {
        if (env$distribution == "cauchy") {
          stop("you cannot combine 't' and 'cauchy' distributions")
        }
        if (env$distribution == "dnorm") {
          stop("you cannot combine 't' and 'dnorm' distributions")
        }
        env$distribution <- "t"
      } else if (.currErr == "cauchy") {
        if (env$distribution == "t") {
          stop("you cannot combine 't' and 'cauchy' distributions")
        }
        if (env$distribution == "dnorm") {
          stop("you cannot combine 'dnorm' and 'cauchy' distributions")
        }
        env$distribution <- "cauchy"
      } else if (.currErr == "dnorm") {
        if (env$distribution == "t") {
          stop("you cannot combine 't' and 'dnorm' distributions")
        }
        if (env$distribution == "cauchy") {
          stop("you cannot combine 'cauchy' and 'dnorm' distributions")
        }
        env$distribution <- "dnorm"
      }
      .errHandleSingleDistributionTerm(.currErr, expression, env)
    } else if (.currErr %in% names(.errDist)) {
      assign("err", c(env$err,
                      paste0("`", .currErr, "` is incorrectly added to an error expression")), envir=env)
    } else {
      .errHandleSingleTerm(.currErr, expression, env)
    }
  } else {
    if (is.name(expression) || is.atomic(expression)) {
      .currErr <- deparse1(expression)
      .isC <- FALSE
      .isErrDist <- FALSE
    } else {
      .currErr <- deparse1(expression[[1]])
      .isC <- .currErr == "c"
      .isErrDist <- .currErr %in% names(.errDist)
    }
    if (.isC) {
      env$distribution <- "ordinal"
      .errHandleSingleDistributionTerm("ordinal", expression, env)
    } else if (.isErrDist) {
      .currErr <- rxPreferredDistributionName(.currErr)
      env$distribution <- .currErr
      .errHandleSingleDistributionTerm(.currErr, expression, env)
    } else {
      .errHandleSingleTerm(.currErr, expression, env)
    }
  }
}

#' Handle the right hand side conditional expression (if it exists)
#'
#' @param expression A right hand side of the tilde equation
#' @param env Environment for storing information about the expression
#' @return An expression without a conditional statement.
#'
#' @details
#'
#' In addition to stripping the conditional statement out of the
#' expression, the environment is modified when a conditional
#' expression is present.  First, the environment variable
#' `needsToBeAnErrorExpression` is changed to `TRUE`.  Second, the
#' expression `curCondition` is modified to match the information
#' within the conditional statement.
#'
#' @author Matthew Fidler
#' @noRd
.errHandleCondition <- function(expression, env) {
  if (is.name(expression) || is.atomic(expression)) return(expression)
  if (identical(expression[[1]], quote(`|`))) {
    env$needsToBeAnErrorExpression  <- TRUE
    env$curCondition <- deparse1(expression[[3]])
    return(expression[[2]])
  }
  expression
}

#' Handle LL equivalent for ll or `linCmt()` statements for lhs
#'
#' @param expression Left handed side of the equation
#'
#' @param env Environment for storing information about the error
#'   structure
#'
#' @return Nothing called for side effects
#'
#' @details
#'
#' The takes the expression
#'
#' ll(var) ~  log(...)
#'
#' And strips the `ll` and sets the flag `env$ll` to `TRUE`
#'
#' Otherwise it leaves the `expression` alone and returns the value
#'
#' This takes the expression:
#'
#' linCmt() ~ add() ...
#'
#' And returns a `rxLinCmt` as the lhs value and sets the linCmt flag to TRUE
#'
#'
#' @author Matthew Fidler
#' @noRd
.errHandleLlOrLinCmt <- function(expression, env) {
  if (is.call(expression)) {
    if (identical(expression[[1]], quote(`ll`)) &&
          length(expression) == 2L) {
      env$ll <- TRUE
      return(expression[[2]])
    } else if (identical(expression[[1]], quote(`linCmt`))) {
      env$linCmt <- TRUE
      return(quote(`rxLinCmt`))
    } else {
      stop("the left handed side of the error expression (function: '", as.character(expression[[1]]), "') can only be functions with 'linCmt' or 'll'",
           call.=FALSE)
    }
  }
  expression
}


#' Handle the error expressions
#'
#' @param expression Single tilde error expression
#' @param env Environment with initial estimate data.frame
#' @return nothing, called for side effects
#' @author Matthew Fidler
#' @noRd
.errHandleTilde <- function(expression, env) {
  env$ll <- FALSE
  env$estNotAllowed <- TRUE
  env$linCmt <- FALSE
  .left <- .errHandleLlOrLinCmt(expression[[2]], env)
  env$trLimit <- c(-Inf, Inf)
  env$a <- env$b <- env$c <- env$d <- env$e <- env$f <- env$lambda <- NA_character_
  env$curCondition <- env$curVar <- deparse1(.left)
  env$hasNonErrorTerm <- FALSE
  env$needsToBeAnErrorExpression <- FALSE
  env$needToDemoteAdditiveExpression <- FALSE
  .right <- .errHandleCondition(expression[[3]], env)
  env$isAnAdditiveExpression <- FALSE
  env$errTypeInfo <- rxErrTypeCombine("")
  env$distribution <- "norm"
  .errHandleErrorStructure(.right, env)
  if (inherits(env$errTypeInfo, "character")) {
    env$err <- c(env$err, env$errTypeInfo)
    env$errTypeInfo <- rxErrTypeCombine("")
  } else if (env$needToDemoteAdditiveExpression) {
    env$errTypeInfo <- rxDemoteAddErr(env$errTypeInfo)
  }
  if (env$hasNonErrorTerm && env$needsToBeAnErrorExpression) {
    assign("errGlobal", c(env$errGlobal, "cannot mix error expression with algebraic expressions"),
           envir=env)
  } else if (env$hasNonErrorTerm) {
    if (env$ll) {
      env$distribution <- "LL"
      env$predDf <- rbind(env$predDf,
                          data.frame(cond=env$curCondition, var=env$curVar, dvid=env$curDvid,
                                     trLow=env$trLimit[1], trHi=env$trLimit[2],
                                     transform=env$errTypeInfo$transform,
                                     errType=env$errTypeInfo$errType,
                                     errTypeF=env$errTypeInfo$errTypeF,
                                     addProp=env$errTypeInfo$addProp,
                                     distribution=factor(env$distribution, levels=.rxDistributionType),
                                     line=env$line,
                                     a=env$a,
                                     b=env$b,
                                     c=env$c,
                                     d=env$d,
                                     e=env$e,
                                     f=env$f,
                                     lambda=env$lambda,
                                     linCmt=env$linCmt))
      env$curDvid <- env$curDvid + 1L

    }
  } else if (!env$hasNonErrorTerm) {
    if (env$ll) {
      assign("errGlobal", c(env$errGlobal, "a -2 log-likelihood expression cannot use abbreviated error codes like add() + prop() "),
             envir=env)
    } else {
      if (!(env$distribution %in% .rxDistributionType)) {
        env$distribution <- "norm"
      }
      .tmp <- data.frame(cond=env$curCondition, var=env$curVar, dvid=env$curDvid,
                         trLow=env$trLimit[1], trHi=env$trLimit[2],
                         transform=env$errTypeInfo$transform,
                         errType=env$errTypeInfo$errType,
                         errTypeF=env$errTypeInfo$errTypeF,
                         addProp=env$errTypeInfo$addProp,
                         distribution=factor(env$distribution, levels=.rxDistributionType),
                         line=env$line,
                         a=env$a,
                         b=env$b,
                         c=env$c,
                         d=env$d,
                         e=env$e,
                         f=env$f,
                         lambda=env$lambda,
                         linCmt=env$linCmt)
      env$predDf <- rbind(env$predDf, .tmp)
      env$curDvid <- env$curDvid + 1L
    }
  }
}
#' Add the Error Names
#'
#' @param errs Input endpoints
#' @return The endpoints that the user enters
#' @author Matthew L. Fidler
#' @noRd
.userEndpointNames <- function(endpoints) {
  vapply(endpoints, function(e) {
    if (e == "rxLinCmt") {
      return("linCmt()")
    }
    e
  }, character(1), USE.NAMES=FALSE)
}

#' Check for error exceptions
#'
#' @param env Environment to check for error exceptions
#' @return Nothing called for side effects
#' @author Matthew L. Fidler
#' @noRd
.checkForMissingOrDupliacteInitials <- function(env) {
  .predDf <- env$predDf
  .iniDf <- env$iniDf
  .err <- env$dupErr
  for (.i in seq_along(.predDf$cond)) {
    if (!any(!is.na(.predDf[.i, c("a", "b", "c", "d", "e", "f", "lambda")]))) {
      if (!(.predDf[.i, "distribution"] %in% c("LL", "ordinal"))) {
        .cnd <- .predDf$cond[.i]
        .w <- which(.iniDf$condition == .cnd)
        if (length(.w) == 0L) {
          if (!any(.errDist[[paste(.predDf$distribution[.i])]] == 0)) {
            .err <- c(.err, .cnd)
          }
        }
      }
    }
  }
  if (length(.err) > 0) {
    stop("endpoint parameter(s) missing, duplicated, or defined with '~': ", paste(.userEndpointNames(.err), collapse=", "),
         call.=FALSE)
  }
}

#' Get a blank, theta1, or eta1 initialization block for iniDf
#'
#' @param type type of initialization block to return
#' @return A data.frame with the appropriate number/type of columns.
#'
#' For type="empty", the data.frame will have 0 rows but all the correct types.
#'
#' For type="theta", the data.frame will have 1 row with the correct
#' types and default values.  The "name" and "est" will likely need to
#' be updated.
#'
#' For type="eta", the data.frame will have 1 row with the correct
#' types and default values for the a single eta being added.  The
#' "name" and "est" will likely need to be updated.
#'
#'
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#'
#' .rxBlankIni("empty")
#'
#' .rxBlankIni("theta")
#'
#' .rxBlankIni("eta")
#'
.rxBlankIni <- function(type=c("empty", "theta", "eta")) {
  type <- match.arg(type)
  if (type == "empty") {
    data.frame(ntheta=integer(0),
               neta1=integer(0),
               neta2=integer(0),
               name=character(0),
               lower=numeric(0),
               est=numeric(0),
               upper=numeric(0),
               fix=logical(0),
               err=character(0),
               label=character(0),
               stringsAsFactors=FALSE)
  } else if (type == "theta") {
    data.frame(ntheta=1L,
               neta1=NA_integer_,
               neta2=NA_integer_,
               name=NA_character_,
               lower=-Inf,
               est=0,
               upper=Inf,
               fix=FALSE,
               err=NA_character_,
               label=NA_character_,
               stringsAsFactors=FALSE)
  } else {
    data.frame(ntheta=NA_integer_,
               neta1=1L,
               neta2=1L,
               name=NA_character_,
               lower=0,
               est=0.1,
               upper=Inf,
               fix=FALSE,
               err=NA_character_,
               label=NA_character_,
               stringsAsFactors=FALSE)
  }
}

#' Process the errors in the quoted expression
#'
#' @param x Quoted expression for parsing
#' @param ini Initialization block
#' @param linCmtSens Type of linCmt sensitivity
#' @param verbose verbose
#' @param checkMissing Check for missing arguments in the ini({}) block
#' @param df lotri data.frame of estimates
#' @return Environment with error information setup.
#' @author Matthew Fidler
#' @examples
#' lmat <- lotri({
#'  ## You may label each parameter with a comment
#'  tka <- 0.45 # Log Ka
#'  tcl <- log(c(0, 2.7, 100)) # Log Cl
#'  ## This works with interactive models
#'  ## You may also label the preceding line with label("label text")
#'  tv <- 3.45; label("log V")
#'  tvp <- 3.45; label("log V")
#'  cl.wt <- 0.1
#'  v.wt <- 0.1
#'  cl.sex <- 0.1
#'  v.sex <- 0.1
#'  cl.age <- 0.1
#'  v.age <- 0.1
#'  vp.wt <- 1
#'  vp.sex <- 1
#'  vp.age <- 1
#'  ## the label("Label name") works with all models
#'  eta.ka ~ 0.6
#'  eta.cl ~ 0.3
#'  eta.v ~ 0.1
#'  add.sd <- 0.7
#'})
#'
#'
#' iniDf <- as.data.frame(lmat)
#'
#' .errProcessExpression()
#' @noRd
.errProcessExpression <- function(x, ini,
                                  linCmtSens = c("linCmtA", "linCmtB", "linCmtC"),
                                  verbose=FALSE, checkMissing=TRUE, mv=rxUdfUiMv()) {
  on.exit({
    .udfUiEnv$num <- 1L
    .udfUiEnv$iniDf <- NULL
    .udfUiEnv$lhs <- NULL
    .udfUiEnv$parsing <- FALSE
  })
  .udfUiEnv$parsing <- TRUE
  # ntheta neta1 neta2   name lower       est   upper   fix  err  label
  # backTransform condition trLow trHi
  .env <- new.env(parent=emptyenv())
  .env$uiUseData <- FALSE
  .env$uiUseMv <- FALSE
  rxUdfUiData(NULL)
  rxUdfUiEst(NULL)
  rxUdfUiMv(mv)
  .env$rxUdfUiCount <- new.env(parent=emptyenv())
  .env$before <- list()
  .env$after <- list()
  .env$eta <- dimnames(ini)[[1]]
  .env$top <- TRUE
  if (!inherits(ini, "lotriFix") && inherits(ini, "matrix")) {
    class(ini) <- c("lotriFix", class(ini))
  }
  .env$df <- as.data.frame(ini)
  .env$dupErr <- NULL
  .env$err <- NULL
  .env$errGlobal <- NULL
  # Add error structure like nlmixr ui had before transitioning to rxode2
  if (length(.env$df$ntheta) > 0) {
    .env$df$err <- NA_character_
  } else {
    .env$df$err <- character(0)
  }
  #.env$df$trLow <- .env$df$trHi <- NA_real_
  .env$curDvid <- 1L
  # Pred df needs to be finalized with compartment information from parsing the raw rxode2 model
  .env$predDf  <- NULL
  .env$lastDistAssign <- ""
  if (is.call(x)) {
    if (.env$top && identical(x[[1]], quote(`{`))) {
      .env$top <- FALSE
      .y <- x[-1]
      .env$lstChr <- character(length(.y))
      .env$lstErr <- vector(length(.y), mode="list")
      .env$lstExpr <- vector(length(.y), mode="list")
      .env$hasErrors <- FALSE
      .i <- 1L
      .y <- lapply(seq_along(.y), function(i) {
        .y[[i]]
      })
      while(.i <= length(.y)) {
        .env$line <- .i
        if (identical(.y[[.i]][[1]], quote(`~`))) {
          .errHandleTilde(.y[[.i]], .env)
        } else {
          .env$redo <- FALSE
          .cur <- .y[[.i]]
          if (length(.cur) >= 3 && identical(.cur[[1]], quote(`<-`))) {
            .env$lhs <- .cur[[2]]
          } else if (length(.cur)>= 3 && identical(.cur[[1]], quote(`=`))) {
            .env$lhs <- .cur[[2]]
          } else {
            .env$lhs <- NULL
          }
          .cur <- .handleUdfUi(.cur, .env)
          .len <- length(.y)
          .y <- c(lapply(seq_len(.i - 1),
                         function(i) {
                           .y[[i]]
                         }), .env$before, .cur, .env$after,
                  lapply(seq_len(.len - .i),
                         function(i) {
                           .y[[i + .i]]
                         }))
          if (length(.y) != .len) {
            # Update the lengths of lstChr, lstErr, lstExpr
            .len <- length(.env$before) + length(.env$after)
            .env$lstChr <- c(.env$lstChr, character(.len))
            .env$lstErr <- c(.env$lstErr, vector(.len, mode="list"))
            .env$lstExpr <- c(.env$lstExpr, vector(.len, mode="list"))
            .env$before <- list()
            .env$after <- list()
            # redo the parsing since the length of the expression has changed
            next
          } else if (.env$redo) {
            next
          }
        }
        .env$lstChr[[.i]] <- deparse1(.y[[.i]])
        .env$lstExpr[[.i]] <- .y[[.i]]
        if (!is.null(.env$err)) {
          .env$lstErr[[.i]] <- paste(paste(" ", .env$err), collapse="\n")
          .env$err <- NULL
          .env$hasErrors <- TRUE
        }
        .i <- .i + 1L
      }
      .env$iniDf <- .env$df
      if (is.null(.env$predDf)) {
        ## .env$errGlobal <- c(.env$errGlobal,
        ##                     "there must be at least one prediction in the model({}) block.  Use `~` for predictions")
      } else if (length(.env$predDf[, 1]) == 0L) {
        .env$predDf <- NULL
        ## .env$errGlobal <- c(.env$errGlobal,
        ##                     "there must be at least one prediction in the model({}) block.  Use `~` for predictions")
      }
      if (!is.null(.env$errGlobal)) {
        stop(paste(.env$errGlobal, collapse="\n"), call.=FALSE)
      }
      if (!is.null(.env$predDf)) {
        .lstChr <- .env$lstChr
        .lines <- .env$predDf$line
        .w <- which(.env$predDf$distribution == "LL")
        if (length(.w) > 0) {
          .lstExpr <- .env$lstExpr
          .w2 <- .env$predDf$line[.w]
          .lstChr <- vapply(seq_along(.lstExpr),
                            function(i) {
                              .cur <- .lstExpr[[i]]
                              if (i %in% .w2) {
                                paste0("rxLL ~ ", deparse1(.cur[[3]]))
                              } else {
                                deparse1(.cur)
                              }
                            }, character(1), USE.NAMES=FALSE)
          .lines <- .lines[-.w]
          if (length(.lines) > 0) {
            .lstChr <- .lstChr[-.lines]
          }
        } else {
          .lstChr <- .lstChr[-.lines]
        }
        if (length(.lstChr) == 0) {
          stop("a rxode2 ui model must have more than error definition(s) in the `model({})` block",
               call.=FALSE)
        }
        if (any(.env$predDf$linCmt)) {
          .env$mv0 <- rxModelVars(paste(c(.lstChr, "rxLinCmt ~ linCmt()"), collapse="\n"))
        } else {
          .env$mv0 <- rxModelVars(paste(.lstChr, collapse="\n"))
        }
      } else {
        .env$mv0 <- rxModelVars(paste(.env$lstChr, collapse="\n"))
      }
      if (isTRUE(.env$uiUseMv) && is.null(mv)) {
        # ui function requests model variables, so re-process
        on.exit({
          rxUdfUiData(NULL)
          rxUdfUiMv(NULL)
        })
        return(.errProcessExpression(x=x, ini=ini,
                                     linCmtSens = linCmtSens,
                                     verbose=verbose, checkMissing=checkMissing,
                                     mv=.env$mv0))
      }
      .env$errParams0 <- rxUiGet.errParams(list(.env, TRUE))
      if (.Call(`_rxode2_isLinCmt`) == 1L) {
        .env$.linCmtM <- rxNorm(.env$mv0)
        .ini <- .env$mv0$ini
        .ini <- which(!is.na(.ini))
        .vars <- c(.env$mv0$params, .env$mv0$lhs, .env$mv0$slhs, names(.ini))
        .env$mvL <- rxGetModel(.Call(
          `_rxode2_linCmtGen`,
          length(.env$mv0$state),
          .vars,
          setNames(
            c("linCmtA" = 1L, "linCmtB" = 2L,
              "linCmtC" = 3L
              )[match.arg(linCmtSens)],
            NULL
          ), verbose
        ))
      } else {
        .env$mvL <- NULL
      }
      .env$curCmt <- length(.env$mv0$state)
      .env$extraCmt <- NULL
      if (!is.null(.env$predDf)) {
        .env$predDf$cmt <- vapply(seq_along(.env$predDf$line),
                                  function(i) {
                                    .cmtName <- .env$predDf$cond[i]
                                    .w <- which(.cmtName == .env$mv0$state)
                                    if (length(.w) == 1L) return(.w)
                                    assign("curCmt", .env$curCmt + 1L, envir=.env)
                                    assign("extraCmt", c(.env$extraCmt,
                                                         paste0("cmt(", .cmtName, ")")),
                                           envir=.env)
                                    .env$curCmt
                                  }, integer(1))
      }
      .env$extraDvid <- paste0("dvid(", paste(.env$predDf$cmt, collapse = ","), ")")
      # Cleanup the environment
      .rm <- intersect(c("curCondition", "curDvid", "curVar", "df",
                         "errTypeInfo", "err", "hasNonErrorTerm", "isAnAdditiveExpression",
                         "lastDistAssign", "line", "needsToBeAnErrorExpression",
                         "needToDemoteAdditiveExpression",
                         "top", "trLimit", ".numeric", "a", "b", "c", "d", "e", "f",  "lambda",
                         "curCmt", "errGlobal", "linCmt", "ll", "distribution", "rxUdfUiCount", "before", "after",
                         "lhs"),
                       ls(envir=.env, all.names=TRUE))
      if (length(.rm) > 0) rm(list=.rm, envir=.env)
      if (checkMissing) .checkForMissingOrDupliacteInitials(.env)
      return(.env)
    }
  }
  return(invisible(NULL))
}

#' Determine if expression is a rxode2 error expression
#'
#' Currently only handles simple error expressions, not ll(var) ~ ...
#'
#' @param expr Expression to test
#' @param uiEnv Return the uiEnv for the single expression
#' @return Boolean that says if the expression is an error expression or not
#' @author Matthew L. Fidler
#' @examples
#'
#' .isErrorExpression(quote(ipre~add(add.sd)))
#' .isErrorExpression(quote(ipre~add(add.sd)+3))
#'
#' @noRd
.isErrorExpression <- function(expr, uiEnv=FALSE) {
  if (!identical(expr[[1]], quote(`~`))) return(FALSE)
  if (identical(expr[[2]], quote(linCmt()))) {
    .pre <- "linCmt()"
    .var <- "cl=abc\nv=d\nrxDummyVarNotUsedInModel"
    .lin <- TRUE
  } else {
    .expr2 <- expr[[2]]
    if (length(.expr2) == 2L) {
      if (identical(.expr2[[1]], quote(`-`)) &&
            is.name(.expr2[[2]])) {
        return(TRUE)
      }
    }
    .pre <- allNames(.expr2)
    if (length(.pre) != 1L) return(FALSE)
    .var <- .pre
    .lin <- FALSE
  }
  .mod <- eval(parse(text=paste0("quote({", .var, "=1+2\n", deparse1(expr), "})")))
  .ini <- as.data.frame(eval(parse(text=paste0("lotri({\n",
                                               paste(paste(allNames(expr[[3]]), "<- 1"), collapse="\n"),
                                               "\n})"))))
  .env <- try(.errProcessExpression(.mod, .ini, checkMissing=FALSE), silent=TRUE)
  if (inherits(.env, "try-error")) return(FALSE)
  if (uiEnv) return(.env)
  ifelse(.lin, "rxLinCmt" == .env$predDf$var, .pre == .env$predDf$var)
}
#'  Is Normal, Cauchy or t distribution model specification?
#'
#' @param expr Expression
#'
#' @return TRUE if this is a normal/t/cauchy model, FALSE otherwise
#' @author Matthew L. Fidler
#' @noRd
.isNormOrTErrorExpression <- function(expr) {
  .env <- .isErrorExpression(expr, TRUE)
  if (inherits(.env, "logical")) return(FALSE)
  any(.env$predDf$distribution == c("norm", "t", "cauchy"))
}
#' Throw an error if the error expression  is invalid
#'
#' @param expr Error expression from a model({}) or related pipe
#' @return Nothing, called for side effect of an error for a bad pipe
#' @author Matthew L. Fidler
#' @noRd
.throwIfInvalidTilde <- function(expr) {
  .env <- .isErrorExpression(expr, TRUE)
  .env$isAnAdditiveExpression <- FALSE
  .env$ll <- FALSE
  .env$linCmt <- FALSE
  .left <- .errHandleLlOrLinCmt(expr[[2]], .env)
  .env$trLimit <- c(-Inf, Inf)
  .env$a <- .env$b <- .env$c <- .env$d <- .env$e <- .env$f <- .env$lambda <- NA_character_
  .env$curCondition <- .env$curVar <- deparse1(.left)
  .env$hasNonErrorTerm <- FALSE
  .env$needsToBeAnErrorExpression <- FALSE
  .env$needToDemoteAdditiveExpression <- FALSE
  .right <- .errHandleCondition(expr[[3]], .env)
  .env$isAnAdditiveExpression <- FALSE
  .env$errTypeInfo <- rxErrTypeCombine("")
  .env$distribution <- "norm"
  .env$estNotAllowed <- FALSE
  .errHandleErrorStructure(.right, .env)
  if (!is.null(.env$err)) {
    stop(paste(.env$err, collapse="\n"), call.=FALSE)
  }
}
