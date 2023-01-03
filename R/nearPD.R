.nearPDlast <- NULL

.nearPD <- function(mat) {
  .mat <- try(Matrix::nearPD(mat), silent=TRUE)
  if (inherits(.mat, "try-catch")) return(NULL)
  .mat <- as.matrix(.mat$mat)
  dimnames(.mat) <- dimnames(mat)
  assignInMyNamespace(".nearPDlast", .mat)
  .mat
}

.nearPDl <- function() {
  .nearPDlast
}
