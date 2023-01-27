.nearPD <- function(mat) {
  .ret <- try(Matrix::nearPD(mat), silent=TRUE)
  if (inherits(.ret, "try-error")) return(NULL)
  as.matrix(.ret$mat)
}
