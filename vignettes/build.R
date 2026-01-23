library(rxode2)

.files <- list.files(pattern=".*[.]Rmd")
.files <- .files[is.na(match(.files, c("rxode2-intro.Rmd", "rxode2-syntax.Rmd")))]
for (.f in .files){rmarkdown::render(.f)}
