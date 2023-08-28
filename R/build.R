## nocov start

.rxodeBuildCode <- function() {
  # This builds the code needed for rxode2
  # generate control
  try({
    message("generate defines")
    sink(devtools::package_file("inst/include/rxode2_control.h"))
    cat("#pragma once\n")
    cat("#ifndef __rxode2_control_H__\n#define __rxode2_control_H__\n")
    cat('#include <rxode2parse_control.h>\n')
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
  cat('#include <rxode2parse_control.h>\n')
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
  invisible("")
}


## nocov end

