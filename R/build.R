## nocov start

genDefine <- function() {

  mod1 <- rxode2parse("
    C2 = centr/V2
    C3 = peri/V3
    d/dt(depot) =-KA*depot
    alag(depot) = 3
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3
    d/dt(peri)  =                    Q*C2 - Q*C3
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff
  ")

  mod <- rxode2parse("
a = 6
b = 0.6
d/dt(intestine) = -a*intestine
d/dt(blood)     = a*intestine - b*blood
")

  mv <- mod1

  .ctl <- rxode2::rxControl()

  .n <- gsub("[.]","_",names(.ctl))
  sink(devtools::package_file("inst/include/rxode2parse_control.h")) # nolint
  cat("#pragma once\n")
  cat("#ifndef __rxode2parse_control_H__\n#define __rxode2parse_control_H__\n")
  cat(paste(paste0("#define ", "Rxc_", .n, " ", seq_along(.n)-1),collapse="\n"))

  .mv <- mod1

  .nmv <- gsub("[.]", "_", names(.mv))
  cat("\n")
  cat(paste(paste0("#define RxMv_", .nmv, " ", seq_along(.nmv)-1),collapse="\n"))
  .nmvf <- names(.mv$flag)
  cat("\n")
  cat(paste(paste0("#define RxMvFlag_", .nmvf, " ", seq_along(.nmvf)-1),collapse="\n"))
  cat("\n")

  .nmvt <- gsub("[.]", "_", names(.mv$trans))

  cat("\n")
  cat(paste(paste0("#define RxMvTrans_", .nmvt, " ",
                   seq_along(.nmvt)-1),collapse="\n"))
  cat("\n")

  et <- structure(list(time = c(0, 0.05, 0.1, 0.2, 0.3, 0.5), cmt = c("(default)", "(obs)", "intestine", "-intestine", "intestine", "out"), amt = c(0.0833333333333333, NA, 3, NA, 3, 3), rate = c(2, 0, 0, 0, 0, 0), ii = c(1, 0, 3, 0, 3, 0), addl = c(9L, 0L, 0L, 0L, 0L, 0L), evid = c(1L, 2L, 1L, 2L, 1L, 1L), ss = c(0L, 0L, 1L, 0L, 2L, 0L)), class = "data.frame", row.names = c(NA, -6L))

  ett1 <- etTrans(et, mod, keepDosingOnly=TRUE)
  .n <- gsub("[.]", "_", names(attr(class(ett1), ".rxode2")))

  cat(paste(paste0("#define RxTrans_", .n, " ", seq_along(.n)-1),collapse="\n"))
  cat(paste0("\n#define RxTransNames CharacterVector _en(", length(.n), ");",
             paste(paste0("_en[",seq_along(.n)-1,']="', .n, '";'), collapse=""),"e.names() = _en;"))
  cat("\n")
  cat("\n#endif // __rxode2parse_control_H__\n")
  sink() # nolint
}


.rxodeBuildCode <- function() {
  # This builds the code needed for rxode2
  # generate control
  try({
    message("generate defines")
    sink(devtools::package_file("inst/include/rxode2_control.h"))
    cat("#pragma once\n")
    cat("#ifndef __rxode2_control_H__\n#define __rxode2_control_H__\n")
    cat('#include "rxode2parse_control.h"\n')
    cat("\n#endif // __rxode2_control_H__\n")
    sink()
    message("Copy header to inst directory")
    file.copy(devtools::package_file("src/rxode2_types.h"),
              devtools::package_file("inst/include/rxode2_types.h"),
              overwrite=TRUE)
    .createRxUiBlessedList()
  })
  message("generate rxResidualError and update documentation")
  rxResidualError <- utils::read.csv(devtools::package_file("inst/residualErrors.csv"),
                              check.names=FALSE)
  usethis::use_data(rxResidualError, overwrite = TRUE)
  .l <- readLines(devtools::package_file("R/rxResidualError.R"))
  .l <- sub("[#][']\\s*@format\\s*.*",
            sprintf("#' @format A data frame with %d columns and %d rows",
                    dim(rxResidualError)[2], dim(rxResidualError)[1]), .l)
  .R <- file(devtools::package_file("R/rxResidualError.R"), "wb")
  writeLines(.l, .R)
  close(.R)
  message("done")
  message("generate rxReservedKeywords and update documentation")
  rxReservedKeywords <- utils::read.csv(devtools::package_file("inst/reserved-keywords.csv"))
  names(rxReservedKeywords)[1] <- "Reserved Name"
  usethis::use_data(rxReservedKeywords, overwrite=TRUE)
  .l <- readLines(devtools::package_file("R/rxReservedKeywords.R"))
  .l <- sub("[#][']\\s*@format\\s*.*",
            sprintf("#' @format A data frame with %d columns and %d rows",
                    dim(rxReservedKeywords)[2], dim(rxReservedKeywords)[1]), .l)
  .R <- file(devtools::package_file("R/rxReservedKeywords.R"), "wb")
  writeLines(.l, .R)
  close(.R)
  message("generate rxSyntaxFunctions and update documentation")
  rxSyntaxFunctions <- utils::read.csv(devtools::package_file("inst/syntax-functions.csv"))
  usethis::use_data(rxSyntaxFunctions, overwrite=TRUE)
  .l <- readLines(devtools::package_file("R/rxSyntaxFunctions.R"))
  .l <- sub("[#][']\\s*@format\\s*.*",
            sprintf("#' @format A data frame with %d columns and %d rows",
                    dim(rxSyntaxFunctions)[2], dim(rxSyntaxFunctions)[1]), .l)
  .R <- file(devtools::package_file("R/rxSyntaxFunctions.R"), "wb")
  writeLines(.l, .R)
  close(.R)
  message("done")
  message("generate rxode2_control.h")
  .n <- gsub("[.]","_",names(rxControl()))
  sink(devtools::package_file("inst/include/rxode2_control.h"))
  cat("#pragma once\n")
  cat("#ifndef __rxode2_control_H__\n#define __rxode2_control_H__\n")
  cat('#include "rxode2parse_control.h"\n')
  cat(paste(paste0("#define ", "Rxc_", .n, " ", seq_along(.n)-1),collapse="\n"))
  cat("\n#endif // __rxode2_control_H__\n")
  sink()
  message("done")
  return(invisible(""))
}
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
  message("Generating parseFuns.R")
  .var <- deparse(rxode2::rxSupportedFuns())
  .num <- deparse(rxode2:::.rxSEeq)
  .var[1] <- paste0(".parseEnv$.parseFuns <- ", .var[1])
  .num[1] <- paste0(".parseEnv$.parseNum <- ", .num[1])
  .pf <- devtools::package_file("R/parseFuns.R")
  unlink(.pf)
  parseFuns.R <- file(.pf, "wb")
  writeLines(.var, parseFuns.R)
  writeLines(.num, parseFuns.R)
  close(parseFuns.R)

  message("rebuild rxode2parse_control.h")
  genDefine()
  message("done")
  invisible("")
}


## nocov end
