rxode2Test(
  {
    # Make sure solved rxSolve work with dplyr objects
    ## rxode2 instance 1
    m1 <-
      rxode2(
        model = "
         C2 = centr/V2;
         C3 = peri/V3;
         d/dt(depot) =-KA*depot;
         d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
         d/dt(peri)  =                    Q*C2 - Q*C3;
         d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;"
      )

    test_that("rxode2 instance 1 is created", {
      expect_equal(class(m1), "rxode2")
    })

    ## et1 <- eventTable(amount.units="ug", time.units = "hours")
    et1 <- eventTable()
    et1$add.dosing(dose = 10000, nbr.doses = 5, dosing.interval = 24)
    et1$add.sampling(0:24)
    et1$add.sampling(24)
    et1$add.sampling(seq(from = 24 + 8, to = 5 * 24, by = 8))


    test_that("rxode2 event table 1 was created", {
      expect_true(inherits(et1, "rxEt"))
      expect_equal(et1$get.nobs(), 38)
      expect_equal(length(et1$get.dosing()[, 1]), 1)
    })

    o1.first <- suppressWarnings(rxSolve(m1,
      params = c(
        KA = .291, CL = 18.6, V2 = 40.2, Q = 10.5, V3 = 297.0,
        Kin = 1.0, Kout = 1.0, EC50 = 200.0
      ),
      events = et1,
      inits = c(0, 0, 0, 1)
    ))

    test_that("filter works", {
      expect_equal((o1.first %>% dplyr::filter(time <= 5))$time, 0:5)
    })


    test_that("distinct works", {
      expect_equal(sum((o1.first %>% dplyr::distinct())$time == 24), 1)
    })


    test_that("slice works", {
      expect_equal((o1.first %>% dplyr::slice(2:4))$time, 1:3)
    })

    test_that("top n works", {
      expect_equal((o1.first %>% dplyr::top_n(3, depot))$time, c(48, 72, 96))
    })

    sn <- dplyr::sample_n(o1.first, 3)
    sf <- dplyr::sample_frac(o1.first, 0.5)

    test_that("sample n works", {
      expect_equal(length(sn$time), 3)
    })

    test_that("sample frac works", {
      expect_equal(length(sf$time), 19)
    })

    test_that("top n works", {
      expect_equal(
        round(as.data.frame(dplyr::top_n(o1.first, 3, depot)), 4),
        structure(list(time = c(48, 72, 96), C2 = c(4.762, 5.7649, 6.3537), C3 = c(12.3885, 15.0829, 16.6647), depot = c(10009.2745, 10009.2745, 10009.2745), centr = c(191.4312, 231.7507, 255.4203), peri = c(3679.3816, 4479.633, 4949.4198), eff = c(1.0247, 1.0298, 1.0328)), row.names = c(NA, -3L), class = "data.frame")
      )
    })

    test_that("select works", {
      expect_equal(names(o1.first %>% dplyr::select(time, depot)), c("time", "depot"))
    })

    test_that("summarize works", {
      expect_equal((o1.first %>% dplyr::summarize(avg = mean(depot)))$avg, mean(o1.first$depot))
    })

    test_that("summarize count works", {
      expect_equal((o1.first %>% dplyr::filter(time == 24) %>% dplyr::count())$n, 2)
    })

    test_that("mutate works", {
      expect_equal((o1.first %>% dplyr::mutate(time = time + 1))$time, et1$get.sampling()$time + 1)
    })

    test_that("transmute works", {
      expect_equal(names(o1.first %>% dplyr::transmute(C1 = C2 + C3, time = time)), c("C1", "time"))
    })

    test_that("rename works", {
      expect_equal(names(o1.first %>% dplyr::rename(Cdepot = C2)), c("time", "Cdepot", "C3", "depot", "centr", "peri", "eff"))
    })

    tmp <- round(o1.first %>% dplyr::arrange(C2), 4)

    attr(tmp, ".env") <- NULL
    test_that("arrange works", {
      expect_equal(
        round(o1.first %>% dplyr::arrange(C2), 4),
        structure(list(time = c(0, 24, 24, 23, 22, 21, 20, 19, 18, 17, 48, 16, 15, 72, 14, 96, 120, 40, 13, 64, 12, 88, 112, 11, 10, 9, 8, 32, 56, 7, 80, 104, 6, 5, 1, 4, 3, 2), C2 = c(0, 3.0533, 3.0533, 3.1669, 3.2983, 3.4531, 3.6385, 3.8645, 4.1443, 4.4955, 4.762, 4.9415, 5.5137, 5.7649, 6.2535, 6.3537, 6.6994, 6.9833, 7.2161, 8.1812, 8.4737, 8.8843, 9.2972, 10.1208, 12.2786, 15.0984, 18.7602, 21.2111, 22.6416, 23.4585, 23.4814, 23.9744, 29.3553, 36.4638, 43.9933, 44.3751, 51.6516, 54.5087), C3 = c(0, 7.7987, 7.7987, 7.9675, 8.1379, 8.3094, 8.4809, 8.6513, 8.8187, 8.9809, 12.3885, 9.1347, 9.2758, 15.0829, 9.3985, 16.6647, 17.5933, 14.6161, 9.4952, 17.834, 9.5558, 19.7231, 20.8321, 9.5666, 9.5098, 9.3619, 9.0928, 15.6374, 19.4806, 8.6643, 21.7367, 23.0612, 8.0302, 7.139, 0.9114, 5.9433, 4.4244, 2.6511), depot = c(10000, 10009.2659, 10009.2659, 12.3956, 16.5824, 22.1833, 29.676, 39.6996, 53.1087, 71.0469, 10009.2745, 95.0441, 127.1466, 10009.2745, 170.0922, 10009.2745, 9.2745, 95.1322, 227.5433, 95.1322, 304.3994, 95.1323, 95.1323, 407.2147, 544.7573, 728.757, 974.9053, 975.8087, 975.8095, 1304.1938, 975.8095, 975.8095, 1744.7043, 2334.0036, 7475.1568, 3122.3474, 4176.9658, 5587.7969), centr = c(0, 122.744, 122.744, 127.3114, 132.5933, 138.8126, 146.2666, 155.3525, 166.6003, 180.7179, 191.4312, 198.6489, 221.6504, 231.7507, 251.3926, 255.4203, 269.3155, 280.7294, 290.0867, 328.8825, 340.6435, 357.1508, 373.7456, 406.8576, 493.5995, 606.9538, 754.1615, 852.6842, 910.1934, 943.0311, 943.9538, 963.7728, 1180.0844, 1465.8455, 1768.5322, 1783.8804, 2076.3956, 2191.248), peri = c(0, 2316.2102, 2316.2102, 2366.3378, 2416.9649, 2467.8913, 2518.8413, 2569.4374, 2619.1665, 2667.3335, 3679.3816, 2713.0005, 2754.9056, 4479.633, 2791.3549, 4949.4198, 5225.2075, 4340.9686, 2820.0816, 5296.6982, 2838.0603, 5857.7582, 6187.1279, 2841.2708, 2824.4011, 2780.4942, 2700.5673, 4644.3219, 5785.7365, 2573.2913, 6455.803, 6849.1648, 2384.9597, 2120.2772, 270.6751, 1765.1486, 1314.0348, 787.3677), eff = c(1, 1.0159, 1.0159, 1.0166, 1.0174, 1.0183, 1.0195, 1.021, 1.0228, 1.0252, 1.0247, 1.0282, 1.0322, 1.0298, 1.0373, 1.0328, 1.0346, 1.0387, 1.044, 1.0449, 1.0529, 1.0485, 1.0506, 1.0644, 1.0795, 1.0988, 1.1229, 1.1359, 1.1434, 1.1518, 1.1478, 1.1504, 1.1838, 1.2141, 1.084, 1.2335, 1.2275, 1.1795)), row.names = c(NA, -38L), class = "data.frame")
      )
    })
  },
  silent = TRUE,
  test = "cran"
)
