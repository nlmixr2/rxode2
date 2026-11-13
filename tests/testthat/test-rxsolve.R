test_that(".rxEvCol() reads an event column whatever its case", {
  .df <- data.frame(ID = 1, RATE = 2, dur = 3, Ss = 4)
  expect_identical(.rxEvCol(.df, "rate"), 2)
  expect_identical(.rxEvCol(.df, "dur"), 3)
  expect_identical(.rxEvCol(.df, "ss"), 4)
  expect_null(.rxEvCol(.df, "ii"))
  expect_null(.rxEvCol(NULL, "rate"))
  # a missing column can default to one value per row
  expect_identical(.rxEvCol(data.frame(ID = 1:3), "rate", 0), c(0, 0, 0))
  expect_identical(.rxEvCol(.df, "rate", 0), 2)
})
