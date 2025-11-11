# Interactive Tutorials

## Interactive tutorials

``` r
library(rxode2)
#> rxode2 4.1.1.9000 using 2 threads (see ?getRxThreads)
#>   no cache: create with `rxCreateCache()`
```

`rxode2` comes with a few interactive tutorials that you can run with
your own R session; They are built into Rstudio 1.3 and can be run from
any R session by:

    ## rxode2 Syntax
    learnr::run_tutorial("rxode2-00-syntax", "rxode2")

    ## rxode2 event tables 

    learnr::run_tutorial("rxode2-00-events", "rxode2")
