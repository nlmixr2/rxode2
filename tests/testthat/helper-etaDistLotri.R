## `dist()` needs a lotri that describes declared distributions.  The version on
## CRAN does not, and it cannot be detected by version -- the lotri carrying the
## feature reports the same 1.0.5 as the one that does not, so a `DESCRIPTION`
## requirement cannot express it.  `.rxEtaDistLotriOk()` tests for the FEATURE.
##
## Without this guard these files do not merely fail, they fail in two different
## ways and neither names the cause:
##
##   * a test that calls the catalogue directly dies on lotri's own namespace
##     error, "'lotriEtaDists' is not an exported object from 'namespace:lotri'";
##   * a model whose `ini({})` carries a `dist()` declaration dies earlier still,
##     inside lotri's ini parser, as "lotri syntax errors above" -- before any
##     rxode2 code runs, so rxode2 cannot turn it into a better message.
##
## Skipping is the honest outcome: the feature genuinely is not present, and a
## CRAN check run against the CRAN lotri must report that as a skip rather than
## 23 failures.
skipIfNoEtaDist <- function() {
  testthat::skip_if_not(
    .rxEtaDistLotriOk(),
    "lotri does not provide lotriEtaDists(); dist() needs the development lotri"
  )
}
