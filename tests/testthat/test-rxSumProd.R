test_that("sumProd test", {
  skip_if_not(file.exists("rxSumProd-1.qs"))
  
  f <- qs::qread("test-rxSumProd-1.qs")

  rxSumProdModel(f)
  
  
})
