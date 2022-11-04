test_that("matchesTemplate", {
  expect_true(matchesTemplate(str2lang("d/dt(foo)"), str2lang("d/dt(name)")))
  expect_false(matchesTemplate(str2lang("d/dt(foo)"), str2lang("d/foo(name)")))
})
