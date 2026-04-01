rxTest({

  for (i in seq(0.05, 0.95, by=0.05)) {
    test_that(paste0("mlogit", i), {
      expect_equal(mlogit(i), logit(i))
      expect_equal(mexpit(mlogit(i)), i)
    })
  }

  df <- expand.grid(p1=seq(0.05, 0.95, by=0.05), p2=seq(0.05, 0.95, by=0.05))
  df <- df[df$p1+df$p2 < 1, ]
  for (i in seq_along(df$p1)) {
    test_that(paste0("mlogit ", df$p1[i], ", ", df$p2[i]), {
      expect_equal(mexpit(mlogit(df$p1[i], df$p2[i])), c(df$p1[i], df$p2[i]))
    })
  }

  df <- expand.grid(p1=seq(0.05, 0.95, by=0.05), p2=seq(0.05, 0.95, by=0.05),
                    p3=seq(0.05, 0.95, by=0.05))
  df <- df[df$p1+df$p2 + df$p3 < 1, ]
  for (i in seq_along(df$p1)) {
    test_that(paste0("mlogit ", df$p1[i], ", ", df$p2[i], ", ", df$p3[i]), {
      expect_equal(mexpit(mlogit(df$p1[i], df$p2[i], df$p3[i])),
                   c(df$p1[i], df$p2[i], df$p3[i]))
    })
  }

  df <- expand.grid(p1=seq(0.05, 0.95, by=0.05), p2=seq(0.05, 0.95, by=0.05),
                    p3=seq(0.05, 0.95, by=0.05))
  df <- df[df$p1+df$p2 + df$p3 < 1, ]
  for (i in seq_along(df$p1)) {
    ml <- mlogit(df$p1[i], df$p2[i], df$p3[i])
    dml <- dmexpit(ml)
    dml2 <- vapply(seq_along(ml), function(j) {
      exp(ml[j])/(1+sum(exp(ml))) - exp(2*ml[j])/(1+sum(exp(ml)))^2
    }, numeric(1), USE.NAMES=FALSE)
    test_that(paste0("dmexpit ", df$p1[i], ", ", df$p2[i], ", ", df$p3[i]), {
      expect_equal(dml, dml2)
    })
  }
})
