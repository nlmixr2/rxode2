if (rxode2parse::.linCmtSens()) {
  test_that(".matchesLangTemplate", {
    expect_false(.matchesLangTemplate(str2lang("d/dt(foo)"), str2lang("d/foo(.name)")))
    # . matches anything
    expect_true(.matchesLangTemplate(str2lang("'foo'"), str2lang(".")))
    # .name matches any name
    expect_true(.matchesLangTemplate(str2lang("d/dt(foo)"), str2lang("d/dt(.name)")))
    # .call() matches any call but it must be given as a call (and not a name)
    expect_true(.matchesLangTemplate(str2lang("d/(foo)"), str2lang("d/.call()")))
    expect_false(.matchesLangTemplate(str2lang("d/(foo)"), str2lang("d/.call")))
  })
}
