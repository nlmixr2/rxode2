test_that("nodup-keep", {

  #rxode2 issue #169
  mod3 <- rxode2({
    KA=2.94E-01;
    ## Clearance with individuals
    CL=1.86E+01 * (WT / 70) ^ 0.75;
    V2=4.02E+01;
    Q=1.05E+01;
    V3=2.97E+02;
    Kin=1;
    Kout=1;
    EC50=200;
    ## The linCmt() picks up the variables from above
    C2   = linCmt();
    Tz= 8
    amp=0.1
    eff(0) = 1  ## This specifies that the effect compartment starts at 1.
    d/dt(eff) =  Kin - Kout*(1-C2/(EC50+C2))*eff;
  })

  ev <- qs::qdeserialize( qs::base91_decode("un]\"BAAA@QRtHACAAAAAAAY4RAAAv7#aT)ZPjXEAt%^ng/zWFBEA{2KAa\"1BbLk08.Gl&2^$bX)tT/]$x88j[G9w1]weL%9/kbZPPPU=|<H[1zn!Dm2oOp5g\"<y{G$/xSxQ\"AA:CuW$Asbxc;v7d!C#TM)6FPWeq<)4F)s3}wZ(.4@!TkNo|&2Yq[i>llNv\"_Y/fal_QniDa~7.G]akz7Rf/[88j;I4J`YZ;qUW0pQcjH%^da@;pPIxARF<s4}n_KP^D(g4gtNN!60zCUz#H~O$4VT9(P2t*~F}okGz?FXe0r8C:0Svu06HecVriBy}TLp~BaqmF7:n+V]T{s6%S$b}V(MkU@:5u)c4Gyq%>~uLB5)*DCz%e5(3!uHA"))

  rxWithSeed(10, {

    r1 <- solve(mod3, ev,
                # Create individual covariate data-frame
                keep="WT", returnType="data.frame")

    expect_length(which(names(r1)=="WT"), 1)
  })


 })

