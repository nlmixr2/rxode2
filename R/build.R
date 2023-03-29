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
    sink();
    message("Copy header to inst directory")
    file.copy(devtools::package_file("src/rxode2_types.h"),
              devtools::package_file("inst/include/rxode2_types.h"),
              overwrite=TRUE)
    .createRxUiBlessedList()
  })
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

