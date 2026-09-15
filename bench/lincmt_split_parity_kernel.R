# Kernel-level parity cases, sourced by bench/lincmt_split_parity.R
# (needs .keep() from the driver).

# --- per-row kernel, decompositions, derived parameters -----------------------
.linCmtModelDouble <- utils::getFromNamespace("linCmtModelDouble", "rxode2")
for (.ncmt in 1:3) {
  for (.oral0 in 0:1) {
    for (.deriv in c(FALSE, TRUE)) {
      .nstate <- .ncmt + .oral0
      .n <- if (.deriv) .nstate + .ncmt * (2L * .ncmt + .oral0) + .oral0 else .nstate
      for (.st in c(3L, 30L, 31L, 32L)) {
        for (.dt in c(0.5, 3)) {
          .keep(sprintf("modelDouble_c%d_o%d_d%d_st%d_dt%g", .ncmt, .oral0, .deriv, .st, .dt),
                .linCmtModelDouble(.dt, 2, 20, 3, 40, 1, 80, 1.1,
                                   c(100, numeric(.n - 1L)), c(5, numeric(.nstate - 1L)),
                                   .ncmt, .oral0, 1L, .deriv, 0L, 0, 0, 0, 0L, 0L, .st, 0.001))
        }
      }
    }
  }
}
.keep("solComp2", .solComp2(k10 = 0.1, k12 = 3, k21 = 1))
.keep("solComp3", .solComp3(k10 = 0.1, k12 = 3, k21 = 1, k13 = 2, k31 = 0.5))
.keep("derived_1", rxDerived(v1 = 8, k = 0.5 * 1:3, digits = 3))
.keep("derived_2", rxDerived(v1 = 5, v2 = 50, cl = 3.5, q = 2.5, digits = 3))
.keep("derived_3", rxDerived(v1 = 10, v2 = 100, v3 = 1000, cl = 3, q = 2, q2 = 1))
