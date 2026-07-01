.codeLoaded <- function() {
  .Call(`_rxode2_codeLoaded`)
}

.codegen <- function(c_file, prefix, libname, pMd5, timeId, lastMv, goodFun,
                     eventSensCode = rep("", 8L)) {
  .Call(`_rxode2_codegen`, c_file, prefix, libname, pMd5, timeId, lastMv, goodFun,
        eventSensCode[1], eventSensCode[2], eventSensCode[3], eventSensCode[4],
        eventSensCode[5], eventSensCode[6], eventSensCode[7], eventSensCode[8])
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
