if (FALSE) {
  # Setup the test data
  lognCv <- function(x){log((x/100)^2+1)}
  
  withr::with_seed(32, {
    nSub <- 3
    nStud <- 2
    
    theta <- c(lka=log(0.5), # log ka
               lCl=log(5), # log Cl
               lV=log(300) # log V
    )
    
    thetaMat <- lotri(lCl ~ lognCv(5),
                      lV  ~ lognCv(5),
                      lka ~ lognCv(5))
    
    nev <- nSub*nStud
    
    ev1 <- data.frame(COV1=rnorm(nev,50,30),COV2=rnorm(nev,75,10),
                      COV3=sample(c(1.0,2.0),nev,replace=TRUE))
    
    tmat <-rxRmvn(nStud, theta[dimnames(thetaMat)[[1]]], thetaMat)
    
    x1 <- rxCbindStudyIndividual(tmat, ev1)
    
    nev <- nStud
    
    ev2 <- data.frame(COV1=rnorm(nev,50,30),COV2=rnorm(nev,75,10),
                      COV3=sample(c(1.0,2.0),nev,replace=TRUE))
    
    
    x2 <- rxCbindStudyIndividual(tmat, ev2)
  })
  qs::qsave(list(tmat=tmat, ev1=ev1, ev2=ev2, x1=x1, x2=x2), "data-cbind-study-individual.qs")
}

test_that("cbind study and individual", {
  qs <- test_path("data-cbind-study-individual.qs")
  skip_if_not(file.exists(qs))
  
  l <- qs::qload(qs)
  
  tmat <- l$tmat
  ev1 <- l$ev1
  ev2 <- l$ev2
  
  expect_equal(rxCbindStudyIndividual(tmat, ev1), l$x1)
  
  expect_equal(rxCbindStudyIndividual(as.data.frame(tmat), ev1), l$x1)
  
  expect_equal(rxCbindStudyIndividual(tmat, ev2), l$x2)
  
  expect_equal(rxCbindStudyIndividual(as.data.frame(tmat), ev2), l$x2)
  
  expect_error(rxCbindStudyIndividual(c(a = 1), ev1))
  
  expect_error(rxCbindStudyIndividual(tmat, c(a = 1)))
  
  tmat2 <- tmat
  
  dimnames(tmat2) <- list(c("c", "cq"), NULL)
  
  expect_error(rxCbindStudyIndividual(tmat2, ev1))
})
