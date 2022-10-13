rxTest({ 
  test_that("param() is not dropped in rxNorm model; Issue RxODE#431", {
    mod <- "param(parent_0,log_k_parent,log_k_m1,f_parent_qlogis,CMT);\ncmt(parent);\ncmt(m1);\nparent_0_model~parent_0;\nparent(0)=parent_0_model;\nk_parent~exp(log_k_parent);\nk_m1~exp(log_k_m1);\nf_parent_to_m1~expit(f_parent_qlogis);\nd/dt(parent)=-k_parent*parent;\nd/dt(m1)=+f_parent_to_m1*k_parent*parent-k_m1*m1;\nif (CMT==1){\nnlmixr_pred=parent;\n}\nif (CMT==2){\nnlmixr_pred=m1;\n}\n"
    expect_equal(rxNorm(rxode2(mod)), mod)
  })
})
