.codeLoaded <- function() {
  .Call(`_rxode2_codeLoaded`)
}

.codegen <- function(c_file, prefix, libname, pMd5, timeId, lastMv, goodFun,
                     eventSensCode = rep("", 13L)) {
  .Call(`_rxode2_codegen`, c_file, prefix, libname, pMd5, timeId, lastMv, goodFun,
        eventSensCode[1], eventSensCode[2], eventSensCode[3], eventSensCode[4],
        eventSensCode[5], eventSensCode[6], eventSensCode[7], eventSensCode[8],
        eventSensCode[9], eventSensCode[10], eventSensCode[11], eventSensCode[12],
        eventSensCode[13])
}

.parseModel <- function(type) {
  .Call(`_rxode2_parseModel`, type)
}

.isLinCmt <- function() {
  .Call(`_rxode2_isLinCmt`)
}

.trans <- function(parse_file, prefix, model_md5, parseStr, isEscIn, inME, goodFuns, fullPrintIn) {
  .Call(`_rxode2_trans`,
        parse_file, prefix, model_md5, parseStr, isEscIn, inME, goodFuns, fullPrintIn)
}

.linCmtParse <- function(vars, inStr, verbose) {
  .Call(`_linCmtParse`, vars, inStr, verbose)
}

.linCmtGen <- function(linCmt, vars, linCmtSens, verbose) {
  .Call(`_rxode2_linCmtGen`, linCmt, vars, linCmtSens, verbose)
}

.parseFreeSexp  <- function(last) {
  .Call(`_rxode2_parseFreeSexp`, last)
}

## .calcDerived <- function(ncmtSXP, transSXP, inp, sigdigSXP) {
##   .Call(`_calcDerived`, ncmtSXP, transSXP, inp, sigdigSXP)
## }

#' Test-only driver for the internal `_getDur()` infusion-duration lookup
#'
#' Builds a minimal solving structure from the supplied dose records and calls
#' `_getDur()` directly.  `backward` is 1 (scan back for the infusion start), 2
#' (scan forward, return `NA` when the end is missing) or anything else (scan
#' forward, error when the end is missing).
#'
#' @param time numeric vector of record times
#' @param dose numeric vector of record amounts
#' @param evid integer vector of record event ids
#' @param idose integer vector of 0-based indices into `time`/`dose`/`evid` that
#'   hold the dose records
#' @param l 0-based dose index to look up
#' @param backward scan direction, see above
#' @return numeric of length 2: the duration and the paired dose index found
#' @author Matthew L. Fidler
#' @keywords internal
#' @noRd
.getDurTest <- function(time, dose, evid, idose, l, backward) {
  .Call(`_rxode2_getDurTest`, as.double(time), as.double(dose),
        as.integer(evid), as.integer(idose), as.integer(l),
        as.integer(backward))
}
