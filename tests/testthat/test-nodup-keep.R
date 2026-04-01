rxTest({
  test_that("nodup-keep", {

    #rxode2 issue #169
    mod3 <- rxode2({
      KA=2.94E-01
      ## Clearance with individuals
      CL=1.86E+01 * (WT / 70) ^ 0.75
      V2=4.02E+01
      Q=1.05E+01
      V3=2.97E+02
      Kin=1
      Kout=1
      EC50=200
      ## The linCmt() picks up the variables from above
      C2   = linCmt()
      Tz= 8
      amp=0.1
      eff(0) = 1  ## This specifies that the effect compartment starts at 1.
      d/dt(eff) =  Kin - Kout*(1-C2/(EC50+C2))*eff
    })

    ev <- qs2::qs_deserialize( qs2::base91_decode("unjXVBZLQAAAAAAAAA_)$(K(D.2[vuBAC\"><F{XEsBahBtBJbM_9Pl<L`9FA+v4y1)~Xy&!D*99^H`0im5XJJYQe3>eS{0lRPJ3BrwM!fBuZjT4DQ9nD}P3t7_<]!0`,TJ~0zwEYBVi*7qez?MUUHeG{KyUEkct?hwdx9BgMuFk\"VqFd7lzp,i75mzyn[B|Lw|.#P_Cevz[dE[=7t2rH?vm030isNj+]g#LS||Mdsb4w7C:gWJT=A+QKk>eX<*r!)se~p~v>~9mupn=|+=u&M*!$zIu8dJj0_:k68ivK[9`Riz3mJAN,H[.IF6xB<\")XiNR*67$@v*z/a4t%x)E=sT6jkmH8R~3i<jB@79HZ@&k8|jOa7dA%`[;3S,ot1SM|>0TP|~9^F4gZ>2kD@v+oIR5b.;L|0/&iaz2uMe}mm?oY*45hbh^%gbTL_?V%zqcU1=UtB}sArB:(RdEyDAUc>@]L,RX(W/Dtcb[C1_2LW;epkXbXf]GT8cbd6Ct<Ke"))

    rxWithSeed(10, {

      r1 <- solve(mod3, ev,
                  # Create individual covariate data-frame
                  keep="WT", returnType="data.frame")

      expect_length(which(names(r1)=="WT"), 1)
    })


  })

})
