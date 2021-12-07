library(rxode2)

.files <- list.files(pattern=".*[.]Rmd")
.files <- .files[!(.files %in% c("rxode2-intro.Rmd", "rxode2-syntax.Rmd"))]
for (.f in .files){rmarkdown::render(.f)}
