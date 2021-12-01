rxode2.logo <- "  _____         ____  _____  ______
 |  __ \\       / __ \\|  __ \\|  ____| %s
 | |__) |__  _| |  | | |  | | |__
 |  _  / \\ \\/ / |  | | |  | |  __|
 | | \\ \\  >  <| |__| | |__| | |____
 |_|  \\_\\/_/\\_\\\\____/|_____/|______|
"

#' Version and repository for this dparser package.
#'
#' @param version Version to display/return
#' @param echo Boolean to echo the text logo, by default FALSE
#' @param extra extra text to display in the logo
#' @return A character vector with the version and repository.
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
rxVersion <- function(extra = "", echo = FALSE, version = sessionInfo()$otherPkgs$rxode2$Version) {
  if (echo) {
    message(sprintf(rxode2.logo, sprintf("%s%s", version, extra)), appendLF = FALSE)
  }
  return(invisible(c(version = version, repo = "https://github.com/nlmixrdevelopment/rxode2", md5 = rxode2.md5)))
}
