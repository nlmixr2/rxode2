library(tidyverse)

itdepends::dep_usage_pkg("RxODE") |>
    count(pkg, sort = TRUE) |>
    as.data.frame


if (FALSE){
  ## Creating optimized models for linCmt Advan
  ## rate 1-3 cmt
  m <- RxODE({
    A1=r1/ka-((r1+(-b1-A1last)*ka)*exp(-ka*t))/ka
    A2=(((ka-k32)*r1+(-b1-A1last)*ka^2+(b1+A1last)*k32*ka)*exp(-ka*t))/(ka^2+(-beta-alpha)*ka+alpha*beta)+((((k32-beta)*ka-beta*k32+beta^2)*r2+(k32-beta)*ka*r1+((-b2-b1-A3last-A2last-A1last)*beta*k32+(b2+b1+A2last+A1last)*beta^2)*ka+(b2+A3last+A2last)*beta^2*k32+(-b2-A2last)*beta^3)*exp(-beta*t))/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-((((k32-alpha)*ka-alpha*k32+alpha^2)*r2+(k32-alpha)*ka*r1+((-alpha*b2-alpha*b1+(-A3last-A2last-A1last)*alpha)*k32+alpha^2*b2+alpha^2*b1+(A2last+A1last)*alpha^2)*ka+(alpha^2*b2+(A3last+A2last)*alpha^2)*k32-alpha^3*b2-A2last*alpha^3)*exp(-alpha*t))/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k32*r2+k32*r1)/(alpha*beta)
    A3=-((k23*r1+(-b1-A1last)*k23*ka)*exp(-ka*t))/(ka^2+(-beta-alpha)*ka+alpha*beta)+(((k23*ka-beta*k23)*r2+k23*ka*r1+((-b2-b1-A2last-A1last)*beta*k23+A3last*beta^2-A3last*E2*beta)*ka+(b2+A2last)*beta^2*k23-A3last*beta^3+A3last*E2*beta^2)*exp(-beta*t))/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-(((k23*ka-alpha*k23)*r2+k23*ka*r1+((-alpha*b2-alpha*b1+(-A2last-A1last)*alpha)*k23+A3last*alpha^2-A3last*E2*alpha)*ka+(alpha^2*b2+A2last*alpha^2)*k23-A3last*alpha^3+A3last*E2*alpha^2)*exp(-alpha*t))/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k23*r2+k23*r1)/(alpha*beta)
  })

  message(rxOptExpr(rxNorm(m)))

  m <- RxODE({
    A1=r1/ka-((r1+(-b1-A1last)*ka)*exp(-ka*t))/ka
    A2=-(((lam3^3+(-ka-k42-k32)*lam3^2+((k42+k32)*ka+k32*k42)*lam3-k32*k42*ka)*r2+(-ka*lam3^2+(k42+k32)*ka*lam3-k32*k42*ka)*r1+(-b2-A2last)*lam3^4+((b2+b1+A2last+A1last)*ka+(b2+A4last+A2last)*k42+(b2+A3last+A2last)*k32)*lam3^3+(((-b2-b1-A4last-A2last-A1last)*k42+(-b2-b1-A3last-A2last-A1last)*k32)*ka+(-b2-A4last-A3last-A2last)*k32*k42)*lam3^2+(b2+b1+A4last+A3last+A2last+A1last)*k32*k42*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)+(((lam2^3+(-ka-k42-k32)*lam2^2+((k42+k32)*ka+k32*k42)*lam2-k32*k42*ka)*r2+(-ka*lam2^2+(k42+k32)*ka*lam2-k32*k42*ka)*r1+(-b2-A2last)*lam2^4+((b2+b1+A2last+A1last)*ka+(b2+A4last+A2last)*k42+(b2+A3last+A2last)*k32)*lam2^3+(((-b2-b1-A4last-A2last-A1last)*k42+(-b2-b1-A3last-A2last-A1last)*k32)*ka+(-b2-A4last-A3last-A2last)*k32*k42)*lam2^2+(b2+b1+A4last+A3last+A2last+A1last)*k32*k42*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)-(((lam1^3+(-ka-k42-k32)*lam1^2+((k42+k32)*ka+k32*k42)*lam1-k32*k42*ka)*r2+(-ka*lam1^2+(k42+k32)*ka*lam1-k32*k42*ka)*r1+(-b2-A2last)*lam1^4+((b2+b1+A2last+A1last)*ka+(b2+A4last+A2last)*k42+(b2+A3last+A2last)*k32)*lam1^3+(((-b2-b1-A4last-A2last-A1last)*k42+(-b2-b1-A3last-A2last-A1last)*k32)*ka+(-b2-A4last-A3last-A2last)*k32*k42)*lam1^2+(b2+b1+A4last+A3last+A2last+A1last)*k32*k42*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)-(((ka^2+(-k42-k32)*ka+k32*k42)*r1+(-b1-A1last)*ka^3+((b1+A1last)*k42+(b1+A1last)*k32)*ka^2+(-b1-A1last)*k32*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k32*k42*r2+k32*k42*r1)/(lam1*lam2*lam3)
    A3=(((k23*lam3^2+(-k23*ka-k23*k42)*lam3+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam3)*r1+A3last*lam3^4+(-A3last*ka-A3last*k42+(-b2-A2last)*k23-A3last*E2)*lam3^3+((A3last*k42+(b2+b1+A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(b2+A4last+A2last)*k23+A3last*E2)*k42)*lam3^2+(A3last*k24+(-b2-b1-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k23*lam2^2+(-k23*ka-k23*k42)*lam2+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam2)*r1+A3last*lam2^4+(-A3last*ka-A3last*k42+(-b2-A2last)*k23-A3last*E2)*lam2^3+((A3last*k42+(b2+b1+A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(b2+A4last+A2last)*k23+A3last*E2)*k42)*lam2^2+(A3last*k24+(-b2-b1-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k23*lam1^2+(-k23*ka-k23*k42)*lam1+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam1)*r1+A3last*lam1^4+(-A3last*ka-A3last*k42+(-b2-A2last)*k23-A3last*E2)*lam1^3+((A3last*k42+(b2+b1+A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(b2+A4last+A2last)*k23+A3last*E2)*k42)*lam1^2+(A3last*k24+(-b2-b1-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k23*ka-k23*k42)*r1+(-b1-A1last)*k23*ka^2+(b1+A1last)*k23*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k23*k42*r2+k23*k42*r1)/(lam1*lam2*lam3)
    A4=(((k24*lam3^2+(-k24*ka-k24*k32)*lam3+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam3)*r1+A4last*lam3^4+(-A4last*ka-A4last*k32+(-b2-A2last)*k24-A4last*E2)*lam3^3+((A4last*k32+(b2+b1+A2last+A1last)*k24+A4last*E2)*ka+((b2+A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam3^2+((-b2-b1-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k24*lam2^2+(-k24*ka-k24*k32)*lam2+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam2)*r1+A4last*lam2^4+(-A4last*ka-A4last*k32+(-b2-A2last)*k24-A4last*E2)*lam2^3+((A4last*k32+(b2+b1+A2last+A1last)*k24+A4last*E2)*ka+((b2+A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam2^2+((-b2-b1-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k24*lam1^2+(-k24*ka-k24*k32)*lam1+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam1)*r1+A4last*lam1^4+(-A4last*ka-A4last*k32+(-b2-A2last)*k24-A4last*E2)*lam1^3+((A4last*k32+(b2+b1+A2last+A1last)*k24+A4last*E2)*ka+((b2+A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam1^2+((-b2-b1-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k24*ka-k24*k32)*r1+(-b1-A1last)*k24*ka^2+(b1+A1last)*k24*k32*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k24*k32*r2+k24*k32*r1)/(lam1*lam2*lam3)
  })

  message(rxOptExpr(rxNorm(m)))

  ## Now 1 compartment oral
  m <-RxODE({
    A1 <- A1last*exp(-t*ka)+b1
    A2 <- A1last*ka/(ka-k20)*(exp(-t*k20)-exp(-t*ka))+A2last*exp(-t*k20)+b2
  })

  message(rxOptExpr(rxNorm(m)))

  ## 2 compartment oral

  m <- RxODE({
    E2 <- kel+k12
    E3 <- k21
    #calculate hybrid rate constants
    lambda1 = 0.5*((E2+E3)+sqrt((E2+E3)^2-4*(E2*E3-k12*k21)))
    lambda2 = 0.5*((E2+E3)-sqrt((E2+E3)^2-4*(E2*E3-k12*k21)))
    A2term1 = (((A2last*E3+A3last*k32)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E3+A3last*k32)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A2term2 = A1last*ka*(exp(-t*ka)*(E3-ka)/((lambda1-ka)*(lambda2-ka))+exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(ka-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(ka-lambda2)))
    A2 = A2term1+A2term2  #Amount in the central compartment

    A3term1 = (((A3last*E2+A2last*k23)-A3last*lambda1)*exp(-t*lambda1)-((A3last*E2+A2last*k23)-A3last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A3term2 = A1last*ka*k23*(exp(-t*ka)/((lambda1-ka)*(lambda2-ka))+exp(-t*lambda1)/((lambda2-lambda1)*(ka-lambda1))+exp(-t*lambda2)/((lambda1-lambda2)*(ka-lambda2)))
    A3 = A3term1+A3term2  #Amount in the peripheral compartment

    A1last = A1last*exp(-t*ka)
    A1 = A1last + b1
    A2 = A2+b2
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n")

  message(rxOptExpr(mod))

  message(rxOptExpr(rxNorm(m)))

  ## 3 compartment oral
  m <- RxODE({
    A1=(b1+A1last)*exp(-ka*t);
    A2=(((b2+A2last)*lam3^3+((-b2-b1-A2last-A1last)*ka+(-b2-A4last-A2last)*k42+(-b2-A3last-A2last)*k32)*lam3^2+(((b2+b1+A4last+A2last+A1last)*k42+(b2+b1+A3last+A2last+A1last)*k32)*ka+(b2+A4last+A3last+A2last)*k32*k42)*lam3+(-b2-b1-A4last-A3last-A2last-A1last)*k32*k42*ka)*exp(-lam3*t))/(lam3^3+(-lam2-lam1-ka)*lam3^2+((lam1+ka)*lam2+ka*lam1)*lam3-ka*lam1*lam2)-(((b2+A2last)*lam2^3+((-b2-b1-A2last-A1last)*ka+(-b2-A4last-A2last)*k42+(-b2-A3last-A2last)*k32)*lam2^2+(((b2+b1+A4last+A2last+A1last)*k42+(b2+b1+A3last+A2last+A1last)*k32)*ka+(b2+A4last+A3last+A2last)*k32*k42)*lam2+(-b2-b1-A4last-A3last-A2last-A1last)*k32*k42*ka)*exp(-lam2*t))/((lam2^2+(-lam1-ka)*lam2+ka*lam1)*lam3-lam2^3+(lam1+ka)*lam2^2-ka*lam1*lam2)+(((b2+A2last)*lam1^3+((-b2-b1-A2last-A1last)*ka+(-b2-A4last-A2last)*k42+(-b2-A3last-A2last)*k32)*lam1^2+(((b2+b1+A4last+A2last+A1last)*k42+(b2+b1+A3last+A2last+A1last)*k32)*ka+(b2+A4last+A3last+A2last)*k32*k42)*lam1+(-b2-b1-A4last-A3last-A2last-A1last)*k32*k42*ka)*exp(-lam1*t))/(((lam1-ka)*lam2-lam1^2+ka*lam1)*lam3+(ka*lam1-lam1^2)*lam2+lam1^3-ka*lam1^2)+(((b1+A1last)*ka^3+((-b1-A1last)*k42+(-b1-A1last)*k32)*ka^2+(b1+A1last)*k32*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)
    A3=((A3last*lam3^3+(-A3last*ka-A3last*k42+(-b2-A2last)*k23-A3last*E2)*lam3^2+((A3last*k42+(b2+b1+A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(b2+A4last+A2last)*k23+A3last*E2)*k42)*lam3+(A3last*k24+(-b2-b1-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka)*exp(-lam3*t))/(lam3^3+(-lam2-lam1-ka)*lam3^2+((lam1+ka)*lam2+ka*lam1)*lam3-ka*lam1*lam2)-((A3last*lam2^3+(-A3last*ka-A3last*k42+(-b2-A2last)*k23-A3last*E2)*lam2^2+((A3last*k42+(b2+b1+A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(b2+A4last+A2last)*k23+A3last*E2)*k42)*lam2+(A3last*k24+(-b2-b1-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka)*exp(-lam2*t))/((lam2^2+(-lam1-ka)*lam2+ka*lam1)*lam3-lam2^3+(lam1+ka)*lam2^2-ka*lam1*lam2)+((A3last*lam1^3+(-A3last*ka-A3last*k42+(-b2-A2last)*k23-A3last*E2)*lam1^2+((A3last*k42+(b2+b1+A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(b2+A4last+A2last)*k23+A3last*E2)*k42)*lam1+(A3last*k24+(-b2-b1-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka)*exp(-lam1*t))/(((lam1-ka)*lam2-lam1^2+ka*lam1)*lam3+(ka*lam1-lam1^2)*lam2+lam1^3-ka*lam1^2)-(((b1+A1last)*k23*ka^2+(-b1-A1last)*k23*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)
    A4=((A4last*lam3^3+(-A4last*ka-A4last*k32+(-b2-A2last)*k24-A4last*E2)*lam3^2+((A4last*k32+(b2+b1+A2last+A1last)*k24+A4last*E2)*ka+((b2+A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam3+((-b2-b1-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka)*exp(-lam3*t))/(lam3^3+(-lam2-lam1-ka)*lam3^2+((lam1+ka)*lam2+ka*lam1)*lam3-ka*lam1*lam2)-((A4last*lam2^3+(-A4last*ka-A4last*k32+(-b2-A2last)*k24-A4last*E2)*lam2^2+((A4last*k32+(b2+b1+A2last+A1last)*k24+A4last*E2)*ka+((b2+A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam2+((-b2-b1-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka)*exp(-lam2*t))/((lam2^2+(-lam1-ka)*lam2+ka*lam1)*lam3-lam2^3+(lam1+ka)*lam2^2-ka*lam1*lam2)+((A4last*lam1^3+(-A4last*ka-A4last*k32+(-b2-A2last)*k24-A4last*E2)*lam1^2+((A4last*k32+(b2+b1+A2last+A1last)*k24+A4last*E2)*ka+((b2+A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam1+((-b2-b1-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka)*exp(-lam1*t))/(((lam1-ka)*lam2-lam1^2+ka*lam1)*lam3+(ka*lam1-lam1^2)*lam2+lam1^3-ka*lam1^2)-(((b1+A1last)*k24*ka^2+(-b1-A1last)*k24*k32*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)
  })

  message(rxOptExpr(rxNorm(m)))

  ## Now 1-3 compartment bolus

  ## 3 compartment IV bolus
  m <- RxODE({
    k20 <- 0
    k30 <- 0
    E1 <- k10+k12+k13
    E2 <- k21+k20
    E3 <- k31+k30

    ##calculate hybrid rate constants
    a <- E1+E2+E3
    b <- E1*E2+E3*(E1+E2)-k12*k21-k13*k31
    c <- E1*E2*E3-E3*k12*k21-E2*k13*k31

    m <- (3*b - a^2)/3
    n <- (2*a^3 - 9*a*b + 27*c)/27
    Q <- (n^2)/4 + (m^3)/27

    alpha <- sqrt(-1*Q)
    beta <- -1*n/2
    gamma <- sqrt(beta^2+alpha^2)
    theta <- atan2(alpha,beta)


    lambda1 <- a/3 + gamma^(1/3)*(cos(theta/3) + sqrt(3)*sin(theta/3))
    lambda2 <- a/3 + gamma^(1/3)*(cos(theta/3) - sqrt(3)*sin(theta/3))
    lambda3 <- a/3 -(2*gamma^(1/3)*cos(theta/3))

    B = A2last*k21+A3last*k31
    C = E3*A2last*k21+E2*A3last*k31
    I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31
    J = A1last*k13*E2+A2last*k13*k21-A3last*k12*k21

    A1term1 = A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A1term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))

    A1 <- b1+(A1term1+A1term2)  #Amount in the central compartment

    A2term1 = A2last*(exp(-t*lambda1)*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))

    A2 <- A2term1+A2term2             #Amount in the first-peripheral compartment

    A3term1 = A3last*(exp(-t*lambda1)*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))

    A3 <- A3term1+A3term2            #Amount in the second-peripheral compartment

  })

  ## 2 compartment infusion
  m <- RxODE({
    A1=(((A1last*E2+r1+A2last*k21)-A1last*lambda1)*exp(-t*lambda1)-((A1last*E2+r1+A2last*k21)-A1last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1) + r1*E2*(1/(lambda1*lambda2)+exp(-t*lambda1)/(lambda1*(lambda1-lambda2))-exp(-t*lambda2)/(lambda2*(lambda1-lambda2)))
    A2=(((A2last*E1+A1last*k12)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E1+A1last*k12)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)+r1*k12*(1/(lambda1*lambda2)+exp(-t*lambda1)/(lambda1*(lambda1-lambda2))-exp(-t*lambda2)/(lambda2*(lambda1-lambda2)))
  })

  env <- rxS(m)

  message(rxOptExpr(rxNorm(m)))

  m <- RxODE({
    A1=A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3))) + exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2)) + r1*((E2*E3)/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))
    A2 = A2last*(exp(-t*lambda1)*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3))) + exp(-t*lambda1)*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2)) + r1*k12*(E3/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E3-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E3-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E3-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))
    A3=A3last*(exp(-t*lambda1)*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))+exp(-t*lambda1)*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))+r1*k13*(E2/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E2-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E2-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E2-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))
  })

  message(rxOptExpr(rxNorm(m)))

  m <- RxODE({
    A1 = (((A1last*E2+A2last*k21)-A1last*lambda1)*exp(-t*lambda1)-((A1last*E2+A2last*k21)-A1last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1);
    A2 = (((A2last*E1+A1last*k12)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E1+A1last*k12)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
  })

  message(rxOptExpr(rxNorm(m)))

  m <- RxODE({
    A1= A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3))) + exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))
    A2=A2last*(exp(-t*lambda1)*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))+exp(-t*lambda1)*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))
    A3=A3last*(exp(-t*lambda1)*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))+exp(-t*lambda1)*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))
  })

  message(rxOptExpr(rxNorm(m)))


  ## Now paramterizations

  m <- RxODE({
    E2 <- kel + k12
    ##Calculate roots
    beta <- 0.5*(k12+k21+kel - sqrt((k12+k21+kel)^2 - 4*k21*kel))
    alpha <- k21*kel/beta
  })

  message(rxOptExpr(rxNorm(m)))

  m <- RxODE({
    E2 <- kel + k12 + k13
                                        #Calculate roots - see Upton, 2004
    j <- k12+kel+k21+k31+k13
    k <- k12*k31+kel*k21+kel*k31+k21*k31+k13*k21
    l <- kel*k21*k31

    m <- (3*k - j^2)/3
    n <- (2*j^3 - 9*j*k + 27*l)/27
    Q <- (n^2)/4 + (m^3)/27

    alpha <- sqrt(-1*Q)
    beta <- -1*n/2
    rho <- sqrt(beta^2+alpha^2)
    theta <- atan2(alpha,beta)

    lambda1 <- j/3 + rho^(1/3)*(cos(theta/3) + sqrt(3)*sin(theta/3))
    lambda2 <- j/3 + rho^(1/3)*(cos(theta/3) - sqrt(3)*sin(theta/3))
    lambda3 <- j/3 -(2*rho^(1/3)*cos(theta/3))
  })

  message(rxOptExpr(rxNorm(m)))

  m <- RxODE({
    E1 <- kel+k12
    E2 <- k21
    #calculate hybrid rate constants
    lambda1 = 0.5*((E1+E2)+sqrt((E1+E2)^2-4*(E1*E2-k12*k21)))
    lambda2 = 0.5*((E1+E2)-sqrt((E1+E2)^2-4*(E1*E2-k12*k21)))
  })

  message(rxOptExpr(rxNorm(m)))

  m <- RxODE({
    E1 <- kel+k12+k13
    E2 <- k21
    E3 <- k31

    #calculate hybrid rate constants
    a <- E1+E2+E3
    b <- E1*E2+E3*(E1+E2)-k12*k21-k13*k31
    c <- E1*E2*E3-E3*k12*k21-E2*k13*k31

    m <- (3*b - a^2)/3
    n <- (2*a^3 - 9*a*b + 27*c)/27
    Q <- (n^2)/4 + (m^3)/27

    alpha <- sqrt(-1*Q)
    beta <- -1*n/2
    gamma <- sqrt(beta^2+alpha^2)
    theta <- atan2(alpha,beta)

    lambda1 <- a/3 + gamma^(1/3)*(cos(theta/3) + sqrt(3)*sin(theta/3))
    lambda2 <- a/3 + gamma^(1/3)*(cos(theta/3) - sqrt(3)*sin(theta/3))
    lambda3 <- a/3 -(2*gamma^(1/3)*cos(theta/3))

    B = A2last*k21+A3last*k31
    C = E3*A2last*k21+E2*A3last*k31
    I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31
    J = A1last*k13*E2+A2last*k13*k21-A3last*k12*k21
  })

  message(rxOptExpr(rxNorm(m)))

  m <- RxODE({
    E2 <- kel+k23
    E3 <- k21
    #calculate hybrid rate constants
    lambda1 = 0.5*((E2+E3)+sqrt((E2+E3)^2-4*(E2*E3-k12*k21)))
    lambda2 = 0.5*((E2+E3)-sqrt((E2+E3)^2-4*(E2*E3-k12*k21)))
  })

  message(rxOptExpr(rxNorm(m)))

  m <- RxODE({
    E2  = k20+k23;
    bet  = k23+k32;
    one  = bet+k20;
    beta=0.5*(one-sqrt(one*one-4.0*k32*k20));
    alpha=k32*k20/beta;
    A1 = r1/ka-((r1-A1last*ka)*exp(-ka*t))/ka+b1;
    A2=(((ka-k32)*r1-A1last*ka^2+A1last*k32*ka)*exp(-ka*t))/(ka^2+(-beta-alpha)*ka+alpha*beta)+((((k32-beta)*ka-beta*k32+beta^2)*r2+(k32-beta)*ka*r1+((-A3last-A2last-A1last)*beta*k32+(A2last+A1last)*beta^2)*ka+(A3last+A2last)*beta^2*k32-A2last*beta^3)*exp(-beta*t))/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-((((k32-alpha)*ka-alpha*k32+alpha^2)*r2+(k32-alpha)*ka*r1+((-A3last-A2last-A1last)*alpha*k32+(A2last+A1last)*alpha^2)*ka+(A3last+A2last)*alpha^2*k32-A2last*alpha^3)*exp(-alpha*t))/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k32*r2+k32*r1)/(alpha*beta) + b2;
    A3= -((k23*r1-A1last*k23*ka)*exp(-ka*t))/(ka^2+(-beta-alpha)*ka+alpha*beta)+(((k23*ka-beta*k23)*r2+k23*ka*r1+((-A2last-A1last)*beta*k23+A3last*beta^2-A3last*E2*beta)*ka+A2last*beta^2*k23-A3last*beta^3+A3last*E2*beta^2)*exp(-beta*t))/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-(((k23*ka-alpha*k23)*r2+k23*ka*r1+((-A2last-A1last)*alpha*k23+A3last*alpha^2-A3last*E2*alpha)*ka+A2last*alpha^2*k23-A3last*alpha^3+A3last*E2*alpha^2)*exp(-alpha*t))/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k23*r2+k23*r1)/(alpha*beta);
  })


  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n")


  tmp2 <- RxODE(rxOptExpr(mod))

  summary(rxC(tmp2))

  m0 <- RxODE({
    #Calculate sum of exit rates constants
    E2 =  k20+ k23 + k24
    ##Calculate roots - see Upton, 2004
    j <- k23+k20+k32+k42+k24
    k <- k23*k42+k20*k32+k20*k42+k32*k42+k24*k32
    l <- k20*k32*dk42

    m <- (3*k - j^2)/3
    n <- (2*j^3 - 9*j*k + 27*l)/27
    Q <- (n^2)/4 + (m^3)/27

    alpha <- sqrt(-1*Q)
    beta <- -1*n/2
    rho <- sqrt(beta^2+alpha^2)
    theta <- atan2(alpha,beta)
    lam1 <- j/3 + rho^(1/3)*(cos(theta/3) + sqrt(3)*sin(theta/3))
    lam2 <- j/3 + rho^(1/3)*(cos(theta/3) - sqrt(3)*sin(theta/3))
    lam3 <- j/3 -(2*rho^(1/3)*cos(theta/3))
  })

  tmp <- RxODE(rxOptExpr(rxNorm(m0)))

  summary(rxC(tmp))

  m <- RxODE({
    ##Check half-lives
    ## thalfa <- log(2)/d[,"l1"]
    ## thalfb <- log(2)/d[,"l2"]
    ## thalfg <- log(2)/d[,"l3"]
    A1 = b1+ r1/ka-((r1-A1last*ka)*exp(-ka*t))/ka
    A2 = b2-(((lam3^3+(-ka-k42-k32)*lam3^2+((k42+k32)*ka+k32*k42)*lam3-k32*k42*ka)*r2+(-ka*lam3^2+(k42+k32)*ka*lam3-k32*k42*ka)*r1-A2last*lam3^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam3^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam3^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)+(((lam2^3+(-ka-k42-k32)*lam2^2+((k42+k32)*ka+k32*k42)*lam2-k32*k42*ka)*r2+(-ka*lam2^2+(k42+k32)*ka*lam2-k32*k42*ka)*r1-A2last*lam2^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam2^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam2^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)-(((lam1^3+(-ka-k42-k32)*lam1^2+((k42+k32)*ka+k32*k42)*lam1-k32*k42*ka)*r2+(-ka*lam1^2+(k42+k32)*ka*lam1-k32*k42*ka)*r1-A2last*lam1^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam1^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam1^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)-(((ka^2+(-k42-k32)*ka+k32*k42)*r1-A1last*ka^3+(A1last*k42+A1last*k32)*ka^2-A1last*k32*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k32*k42*r2+k32*k42*r1)/(lam1*lam2*lam3);
    A3=(((k23*lam3^2+(-k23*ka-k23*k42)*lam3+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam3)*r1+A3last*lam3^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam3^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam3^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k23*lam2^2+(-k23*ka-k23*k42)*lam2+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam2)*r1+A3last*lam2^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam2^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam2^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k23*lam1^2+(-k23*ka-k23*k42)*lam1+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam1)*r1+A3last*lam1^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam1^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam1^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k23*ka-k23*k42)*r1-A1last*k23*ka^2+A1last*k23*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k23*k42*r2+k23*k42*r1)/(lam1*lam2*lam3);
    A4=(((k24*lam3^2+(-k24*ka-k24*k32)*lam3+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam3)*r1+A4last*lam3^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam3^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam3^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k24*lam2^2+(-k24*ka-k24*k32)*lam2+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam2)*r1+A4last*lam2^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam2^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam2^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k24*lam1^2+(-k24*ka-k24*k32)*lam1+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam1)*r1+A4last*lam1^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam1^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam1^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k24*ka-k24*k32)*r1-A1last*k24*ka^2+A1last*k24*k32*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k24*k32*r2+k24*k32*r1)/(lam1*lam2*lam3);
  })

  env <- rxS(m)

  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n",
                "A4=", paste0(env$A4),"\n")


  tmp2 <- RxODE(rxOptExpr(mod))

  summary(rxC(tmp2))

  ###########
  ## steady state infusion solutions

  m <- RxODE({
    r2=0
    eAt = 0;##exp(-ka*t)
    eK20 = 0;##exp(-k20*t);
    A1=r1/ka-((r1-A1last*ka)*eAt)/ka
    A2=((r1-A1last*ka)*eAt)/(ka-k20)-(((ka-k20)*r2+ka*r1+(-A2last-A1last)*k20*ka+A2last*k20^2)*eK20)/(k20*ka-k20^2)+(r2+r1)/k20
  })

  env <- rxS(m)

  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n")

  m <- RxODE({
    r1=0
    eAt = 0;##exp(-ka*t)
    eK20 = 0;##exp(-k20*t);
    A1=r1/ka-((r1-A1last*ka)*eAt)/ka
    A2=((r1-A1last*ka)*eAt)/(ka-k20)-(((ka-k20)*r2+ka*r1+(-A2last-A1last)*k20*ka+A2last*k20^2)*eK20)/(k20*ka-k20^2)+(r2+r1)/k20
  })

  env <- rxS(m)

  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n")


  m <- RxODE({
    r2=0
    eKa = 0;##exp(-ka*t)
    eA = 0;##exp(-alpha*t)
    eB = 0;##exp(-beta*t)
    A1=r1/ka-((r1-A1last*ka)*eKa)/ka
    A2= (((ka-k32)*r1-A1last*ka^2+A1last*k32*ka)*eKa)/(ka^2+(-beta-alpha)*ka+alpha*beta)+((((k32-beta)*ka-beta*k32+beta^2)*r2+(k32-beta)*ka*r1+((-A3last-A2last-A1last)*beta*k32+(A2last+A1last)*beta^2)*ka+(A3last+A2last)*beta^2*k32-A2last*beta^3)*eB)/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-((((k32-alpha)*ka-alpha*k32+alpha^2)*r2+(k32-alpha)*ka*r1+((-A3last-A2last-A1last)*alpha*k32+(A2last+A1last)*alpha^2)*ka+(A3last+A2last)*alpha^2*k32-A2last*alpha^3)*eA)/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k32*r2+k32*r1)/(alpha*beta)
    A3=-((k23*r1-A1last*k23*ka)*eKa)/(ka^2+(-beta-alpha)*ka+alpha*beta)+(((k23*ka-beta*k23)*r2+k23*ka*r1+((-A2last-A1last)*beta*k23+A3last*beta^2-A3last*E2*beta)*ka+A2last*beta^2*k23-A3last*beta^3+A3last*E2*beta^2)*eB)/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-(((k23*ka-alpha*k23)*r2+k23*ka*r1+((-A2last-A1last)*alpha*k23+A3last*alpha^2-A3last*E2*alpha)*ka+A2last*alpha^2*k23-A3last*alpha^3+A3last*E2*alpha^2)*eA)/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k23*r2+k23*r1)/(alpha*beta)
  })

  env <- rxS(m)

  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3))

  m <- RxODE({
    r1=0
    eKa = 0;##exp(-ka*t)
    eA = 0;##exp(-alpha*t)
    eB = 0;##exp(-beta*t)
    A1=r1/ka-((r1-A1last*ka)*eKa)/ka
    A2= (((ka-k32)*r1-A1last*ka^2+A1last*k32*ka)*eKa)/(ka^2+(-beta-alpha)*ka+alpha*beta)+((((k32-beta)*ka-beta*k32+beta^2)*r2+(k32-beta)*ka*r1+((-A3last-A2last-A1last)*beta*k32+(A2last+A1last)*beta^2)*ka+(A3last+A2last)*beta^2*k32-A2last*beta^3)*eB)/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-((((k32-alpha)*ka-alpha*k32+alpha^2)*r2+(k32-alpha)*ka*r1+((-A3last-A2last-A1last)*alpha*k32+(A2last+A1last)*alpha^2)*ka+(A3last+A2last)*alpha^2*k32-A2last*alpha^3)*eA)/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k32*r2+k32*r1)/(alpha*beta)
    A3=-((k23*r1-A1last*k23*ka)*eKa)/(ka^2+(-beta-alpha)*ka+alpha*beta)+(((k23*ka-beta*k23)*r2+k23*ka*r1+((-A2last-A1last)*beta*k23+A3last*beta^2-A3last*E2*beta)*ka+A2last*beta^2*k23-A3last*beta^3+A3last*E2*beta^2)*eB)/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-(((k23*ka-alpha*k23)*r2+k23*ka*r1+((-A2last-A1last)*alpha*k23+A3last*alpha^2-A3last*E2*alpha)*ka+A2last*alpha^2*k23-A3last*alpha^3+A3last*E2*alpha^2)*eA)/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k23*r2+k23*r1)/(alpha*beta)
  })

  env <- rxS(m)

  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3))

  m <- RxODE({
    r2=0
    eKa = 0#exp(-ka*t)
    el3 = 0#exp(-lam3*t)
    el2 = 0#exp(-lam2*t)
    el1 = 0#exp(-lam1*t)
    A1 = r1/ka-((r1-A1last*ka)*eKa)/ka
    A2 = -(((lam3^3+(-ka-k42-k32)*lam3^2+((k42+k32)*ka+k32*k42)*lam3-k32*k42*ka)*r2+(-ka*lam3^2+(k42+k32)*ka*lam3-k32*k42*ka)*r1-A2last*lam3^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam3^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam3^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam3)*el3)/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)+(((lam2^3+(-ka-k42-k32)*lam2^2+((k42+k32)*ka+k32*k42)*lam2-k32*k42*ka)*r2+(-ka*lam2^2+(k42+k32)*ka*lam2-k32*k42*ka)*r1-A2last*lam2^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam2^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam2^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam2)*el2)/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)-(((lam1^3+(-ka-k42-k32)*lam1^2+((k42+k32)*ka+k32*k42)*lam1-k32*k42*ka)*r2+(-ka*lam1^2+(k42+k32)*ka*lam1-k32*k42*ka)*r1-A2last*lam1^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam1^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam1^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam1)*el1)/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)-(((ka^2+(-k42-k32)*ka+k32*k42)*r1-A1last*ka^3+(A1last*k42+A1last*k32)*ka^2-A1last*k32*k42*ka)*eKa)/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k32*k42*r2+k32*k42*r1)/(lam1*lam2*lam3)
    A3=(((k23*lam3^2+(-k23*ka-k23*k42)*lam3+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam3)*r1+A3last*lam3^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam3^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam3^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam3)*el3)/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k23*lam2^2+(-k23*ka-k23*k42)*lam2+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam2)*r1+A3last*lam2^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam2^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam2^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam2)*el2)/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k23*lam1^2+(-k23*ka-k23*k42)*lam1+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam1)*r1+A3last*lam1^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam1^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam1^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam1)*el1)/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k23*ka-k23*k42)*r1-A1last*k23*ka^2+A1last*k23*k42*ka)*eKa)/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k23*k42*r2+k23*k42*r1)/(lam1*lam2*lam3)
    A4=(((k24*lam3^2+(-k24*ka-k24*k32)*lam3+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam3)*r1+A4last*lam3^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam3^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam3^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam3)*el3)/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k24*lam2^2+(-k24*ka-k24*k32)*lam2+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam2)*r1+A4last*lam2^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam2^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam2^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam2)*el2)/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k24*lam1^2+(-k24*ka-k24*k32)*lam1+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam1)*r1+A4last*lam1^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam1^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam1^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam1)*el1)/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k24*ka-k24*k32)*r1-A1last*k24*ka^2+A1last*k24*k32*ka)*eKa)/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k24*k32*r2+k24*k32*r1)/(lam1*lam2*lam3)
  })

  env <- rxS(m)

  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n",
                "A4=", paste0(env$A4),"\n")

  m <- RxODE({
    r1=0
    eKa = 0#exp(-ka*t)
    el3 = 0#exp(-lam3*t)
    el2 = 0#exp(-lam2*t)
    el1 = 0#exp(-lam1*t)
    A1 = r1/ka-((r1-A1last*ka)*eKa)/ka
    A2 = -(((lam3^3+(-ka-k42-k32)*lam3^2+((k42+k32)*ka+k32*k42)*lam3-k32*k42*ka)*r2+(-ka*lam3^2+(k42+k32)*ka*lam3-k32*k42*ka)*r1-A2last*lam3^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam3^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam3^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam3)*el3)/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)+(((lam2^3+(-ka-k42-k32)*lam2^2+((k42+k32)*ka+k32*k42)*lam2-k32*k42*ka)*r2+(-ka*lam2^2+(k42+k32)*ka*lam2-k32*k42*ka)*r1-A2last*lam2^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam2^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam2^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam2)*el2)/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)-(((lam1^3+(-ka-k42-k32)*lam1^2+((k42+k32)*ka+k32*k42)*lam1-k32*k42*ka)*r2+(-ka*lam1^2+(k42+k32)*ka*lam1-k32*k42*ka)*r1-A2last*lam1^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam1^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam1^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam1)*el1)/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)-(((ka^2+(-k42-k32)*ka+k32*k42)*r1-A1last*ka^3+(A1last*k42+A1last*k32)*ka^2-A1last*k32*k42*ka)*eKa)/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k32*k42*r2+k32*k42*r1)/(lam1*lam2*lam3)
    A3=(((k23*lam3^2+(-k23*ka-k23*k42)*lam3+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam3)*r1+A3last*lam3^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam3^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam3^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam3)*el3)/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k23*lam2^2+(-k23*ka-k23*k42)*lam2+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam2)*r1+A3last*lam2^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam2^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam2^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam2)*el2)/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k23*lam1^2+(-k23*ka-k23*k42)*lam1+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam1)*r1+A3last*lam1^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam1^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam1^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam1)*el1)/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k23*ka-k23*k42)*r1-A1last*k23*ka^2+A1last*k23*k42*ka)*eKa)/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k23*k42*r2+k23*k42*r1)/(lam1*lam2*lam3)
    A4=(((k24*lam3^2+(-k24*ka-k24*k32)*lam3+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam3)*r1+A4last*lam3^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam3^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam3^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam3)*el3)/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k24*lam2^2+(-k24*ka-k24*k32)*lam2+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam2)*r1+A4last*lam2^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam2^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam2^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam2)*el2)/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k24*lam1^2+(-k24*ka-k24*k32)*lam1+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam1)*r1+A4last*lam1^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam1^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam1^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam1)*el1)/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k24*ka-k24*k32)*r1-A1last*k24*ka^2+A1last*k24*k32*ka)*eKa)/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k24*k32*r2+k24*k32*r1)/(lam1*lam2*lam3)
  })

  env <- rxS(m)

  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n",
                "A4=", paste0(env$A4),"\n")

  ## 2 compartment infusion
  m <- RxODE({
    eL1 = 0#exp(-t*lambda1)
    eL2= 0#exp(-t*lambda2)
    A1=(((A1last*E2+r1+A2last*k21)-A1last*lambda1)*eL1-((A1last*E2+r1+A2last*k21)-A1last*lambda2)*eL2)/(lambda2-lambda1) + r1*E2*(1/(lambda1*lambda2)+eL1/(lambda1*(lambda1-lambda2))-eL2/(lambda2*(lambda1-lambda2)))
    A2=(((A2last*E1+A1last*k12)-A2last*lambda1)*eL1-((A2last*E1+A1last*k12)-A2last*lambda2)*eL2)/(lambda2-lambda1)+r1*k12*(1/(lambda1*lambda2)+eL1/(lambda1*(lambda1-lambda2))-eL2/(lambda2*(lambda1-lambda2)))
  })

  env <- rxS(m)

  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n")

  ## 3 compartment infusion
  m <- RxODE({
    eL1 = 0#exp(-t*lambda1)
    eL2 = 0#exp(-t*lambda2)
    eL3 = 0#exp(-t*lambda3)
    A1=A1last*(eL1*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+eL2*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+eL3*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3))) + eL1*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+eL2*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+eL3*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2)) + r1*((E2*E3)/(lambda1*lambda2*lambda3)-eL1*(E2-lambda1)*(E3-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-eL2*(E2-lambda2)*(E3-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-eL3*(E2-lambda3)*(E3-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))
    A2 = A2last*(eL1*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+eL2*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+eL3*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3))) + eL1*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+eL2*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+eL3*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2)) + r1*k12*(E3/(lambda1*lambda2*lambda3)-eL1*(E3-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-eL2*(E3-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-eL3*(E3-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))
    A3=A3last*(eL1*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+eL2*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+eL3*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))+eL1*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+eL2*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+eL3*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))+r1*k13*(E2/(lambda1*lambda2*lambda3)-eL1*(E2-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-eL2*(E2-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-eL3*(E2-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))
  })


  env <- rxS(m)

  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n")

  ## Two compartment steady state

  m <- RxODE({
    A1last <- b1
    A2last <- 0

  A1term = (((A1last*E2+A2last*k21)-A1last*lambda1)*exp(-t*lambda1)-((A1last*E2+A2last*k21)-A1last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
  A1 = A1term

  A2term = (((A2last*E1+A1last*k12)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E1+A1last*k12)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
  A2 = A2term            #Amount in the peripheral compartment
  })

  env <- rxS(m)

  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n")

  ## Three compartment tau steady state

  m <- RxODE({
    A1last <- b1
    A2last <- 0
    A3last <- 0
    B = A2last*k21+A3last*k31;
    C = E3*A2last*k21+E2*A3last*k31;
    I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31;

    A1term1 = A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A1term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))

    A1 <- A1term1+A1term2  #Amount in the central compartment

    A2term1 = A2last*(exp(-t*lambda1)*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))

    A2 <- A2term1+A2term2             #Amount in the first-peripheral compartment

    A3term1 = A3last*(exp(-t*lambda1)*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))

    A3 <- A3term1+A3term2            #Amount in the second-peripheral compartment
  })


  env <- rxS(m)

  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A2),"\n")

  ## IV bolus steady state
  ## 2 compartment
  ##  First calculate tinf
  m <- RxODE({
    A1last=0;
    A2last=0
    A1=(((A1last*E2+r1+A2last*k21)-A1last*lambda1)*exp(-t*lambda1)-((A1last*E2+r1+A2last*k21)-A1last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1) + r1*E2*(1/(lambda1*lambda2)+exp(-t*lambda1)/(lambda1*(lambda1-lambda2))-exp(-t*lambda2)/(lambda2*(lambda1-lambda2)))
    A2=(((A2last*E1+A1last*k12)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E1+A1last*k12)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)+r1*k12*(1/(lambda1*lambda2)+exp(-t*lambda1)/(lambda1*(lambda1-lambda2))-exp(-t*lambda2)/(lambda2*(lambda1-lambda2)))
  })
  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n")

  ##  Then calculate at t after tinf
  m <- RxODE({
    A1last=(exp(-tinf*lambda1)*r1 - exp(-tinf*lambda2)*r1)/(-lambda1 + lambda2) + r1*E2*(1.0*1/(lambda1*lambda2) + exp(-tinf*lambda1)/((lambda1 - lambda2)*lambda1) - exp(-tinf*lambda2)/((lambda1 - lambda2)*lambda2))
    A2last=r1*k12*(1.0*1/(lambda1*lambda2) + exp(-tinf*lambda1)/((lambda1 - lambda2)*lambda1) - exp(-tinf*lambda2)/((lambda1 - lambda2)*lambda2))
    A1term = (((A1last*E2+A2last*k21)-A1last*lambda1)*exp(-t*lambda1)-((A1last*E2+A2last*k21)-A1last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
  A1 = A1term
    A2term = (((A2last*E1+A1last*k12)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E1+A1last*k12)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A2 = A2term            #Amount in the peripheral compartment
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n")

  ## Three compartment model
  m <- RxODE({
    A1last=0;
    A2last=0
    A3last=0
    B = A2last*k21+A3last*k31
    C = E3*A2last*k21+E2*A3last*k31
    I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31
    J = A1last*k13*E2+A2last*k13*k21-A3last*k12*k21
    A1=A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3))) + exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2)) + r1*((E2*E3)/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))
    A2 = A2last*(exp(-t*lambda1)*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3))) + exp(-t*lambda1)*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2)) + r1*k12*(E3/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E3-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E3-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E3-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))
    A3=A3last*(exp(-t*lambda1)*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))+exp(-t*lambda1)*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))+r1*k13*(E2/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E2-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E2-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E2-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n")
  ## Now use A1last to A3 last from above with t=tinf
  m <- RxODE({
    A1last=r1*(E2*E3/(lambda1*lambda2*lambda3) - exp(-tinf*lambda1)*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - exp(-tinf*lambda2)*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - exp(-tinf*lambda3)*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))
    A2last=r1*k12*(E3/(lambda1*lambda2*lambda3) - exp(-tinf*lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - exp(-tinf*lambda2)*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - exp(-tinf*lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))
    A3last=r1*k13*(E2/(lambda1*lambda2*lambda3) - exp(-tinf*lambda1)*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - exp(-tinf*lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - exp(-tinf*lambda3)*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))

    B = A2last*k21+A3last*k31
    C = E3*A2last*k21+E2*A3last*k31
    I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31
    J = A1last*k13*E2+A2last*k13*k21-A3last*k12*k21

    A1term1 = A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A1term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))

    A1 <- (A1term1+A1term2)  #Amount in the central compartment

    A2term1 = A2last*(exp(-t*lambda1)*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))

    A2 <- A2term1+A2term2             #Amount in the first-peripheral compartment

    A3term1 = A3last*(exp(-t*lambda1)*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))

    A3 <- A3term1+A3term2            #Amount in the second-peripheral compartment
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n")

  ## Oral steady state tau
  ## 1 compartment
  m <-RxODE({
    A1last = b1
    A2last = 0
    A1 <- A1last*exp(-t*ka)
    A2 <- A1last*ka/(ka-k20)*(exp(-t*k20)-exp(-t*ka))+A2last*exp(-t*k20)
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n")

  m <-RxODE({
    A1last = 0
    A2last = b2
    A1 <- A1last*exp(-t*ka)
    A2 <- A1last*ka/(ka-k20)*(exp(-t*k20)-exp(-t*ka))+A2last*exp(-t*k20)
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n")

  ## 2 compartment oral SS deriv
  m <- RxODE({
    A1last = b1
    A2last = 0
    A3last = 0
    A2term1 = (((A2last*E3+A3last*k32)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E3+A3last*k32)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A2term2 = A1last*ka*(exp(-t*ka)*(E3-ka)/((lambda1-ka)*(lambda2-ka))+exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(ka-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(ka-lambda2)))
    A2 = A2term1+A2term2  #Amount in the central compartment

    A3term1 = (((A3last*E2+A2last*k23)-A3last*lambda1)*exp(-t*lambda1)-((A3last*E2+A2last*k23)-A3last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A3term2 = A1last*ka*k23*(exp(-t*ka)/((lambda1-ka)*(lambda2-ka))+exp(-t*lambda1)/((lambda2-lambda1)*(ka-lambda1))+exp(-t*lambda2)/((lambda1-lambda2)*(ka-lambda2)))
    A3 = A3term1+A3term2  #Amount in the peripheral compartment

    A1last = A1last*exp(-t*ka)
    A1 = A1last
    A2 = A2
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n")

  m <- RxODE({
    A1last = 0
    A2last = b2
    A3last = 0
    A2term1 = (((A2last*E3+A3last*k32)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E3+A3last*k32)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A2term2 = A1last*ka*(exp(-t*ka)*(E3-ka)/((lambda1-ka)*(lambda2-ka))+exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(ka-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(ka-lambda2)))
    A2 = A2term1+A2term2  #Amount in the central compartment

    A3term1 = (((A3last*E2+A2last*k23)-A3last*lambda1)*exp(-t*lambda1)-((A3last*E2+A2last*k23)-A3last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A3term2 = A1last*ka*k23*(exp(-t*ka)/((lambda1-ka)*(lambda2-ka))+exp(-t*lambda1)/((lambda2-lambda1)*(ka-lambda1))+exp(-t*lambda2)/((lambda1-lambda2)*(ka-lambda2)))
    A3 = A3term1+A3term2  #Amount in the peripheral compartment

    A1last = A1last*exp(-t*ka)
    A1 = A1last
    A2 = A2
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n")

  ## 3 compartment oral SS deriv
  m <- RxODE({
    A1last = b1
    A2last = 0
    A3last = 0
    A4last = 0

    B = A3last*k32+A4last*k42
    C = E4*A3last*k32+E3*A4last*k42
    I = A2last*k23*E4-A3last*k24*k42+A4last*k23*k42
    J = A2last*k24*E3+A3last*k24*k32-A4last*k23*k32

    A2term1 = A2last*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))
    A2term3 = A1last*KA*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E3-KA)*(E4-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    A2 = A2term1+A2term2+A2term3   #Amount in the central compartment

    A3term1 = A3last*(exp(-t*lambda1)*(E2-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(I-A2last*k23*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k23*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k23*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))
    A3term3 = A1last*KA*k23*(exp(-t*lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E4-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    A3 = A3term1+A3term2+A3term3  #Amount in the first-peripheral compartment

    A4term1 = A4last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A4term2 = exp(-t*lambda1)*(J-A2last*k24*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k24*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k24*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))
    A4term3 = A1last*KA*k24*(exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E3-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    A4 = A4term1+A4term2+A4term3  #Amount in the second-peripheral compartment

    A1 = A1last*exp(-t*KA)
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n",
                "A4=", paste0(env$A4),"\n")


  m <- RxODE({
    A1last = 0
    A2last = b2
    A3last = 0
    A4last = 0

    B = A3last*k32+A4last*k42
    C = E4*A3last*k32+E3*A4last*k42
    I = A2last*k23*E4-A3last*k24*k42+A4last*k23*k42
    J = A2last*k24*E3+A3last*k24*k32-A4last*k23*k32

    A2term1 = A2last*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))
    A2term3 = A1last*KA*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E3-KA)*(E4-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    A2 = A2term1+A2term2+A2term3   #Amount in the central compartment

    A3term1 = A3last*(exp(-t*lambda1)*(E2-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(I-A2last*k23*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k23*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k23*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))
    A3term3 = A1last*KA*k23*(exp(-t*lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E4-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    A3 = A3term1+A3term2+A3term3  #Amount in the first-peripheral compartment

    A4term1 = A4last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A4term2 = exp(-t*lambda1)*(J-A2last*k24*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k24*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k24*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))
    A4term3 = A1last*KA*k24*(exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E3-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    A4 = A4term1+A4term2+A4term3  #Amount in the second-peripheral compartment

    A1 = A1last*exp(-t*KA)
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n",
                "A4=", paste0(env$A4),"\n")

  ## Ka rate equations for steady state

  ## First solve tinf
  m <- RxODE({
    A1last = 0
    A2last = 0
    r2 = 0
    A1=r1/ka-((r1-A1last*ka)*exp(-ka*t))/ka
    A2=((r1-A1last*ka)*exp(-ka*t))/(ka-k20)-(((ka-k20)*r2+ka*r1+(-A2last-A1last)*k20*ka+A2last*k20^2)*exp(-k20*t))/(k20*ka-k20^2)+(r2+r1)/k20
  })

  env <- rxS(m)
  mod <- gsub("[-]t[*]", "-tinf*", paste0("A1last=", paste0(env$A1),"\n",
                                          "A2last=", paste0(env$A2),"\n"))

  ## With amounts at tinf, solve to steady state
  m <- RxODE({
    A1last=r1/ka - exp(-tinf*ka)*r1/ka
    A2last=r1/k20 + exp(-tinf*ka)*r1/(-k20 + ka) - exp(-tinf*k20)*r1*ka/(ka*k20 - k20^2.0)
    A1 <- A1last*exp(-t*ka)
    A2 <- A1last*ka/(ka-k20)*(exp(-t*k20)-exp(-t*ka))+A2last*exp(-t*k20)
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n")



  ## First solve tinf
  m <- RxODE({
    A1last = 0
    A2last = 0
    r1 = 0
    A1=r1/ka-((r1-A1last*ka)*exp(-ka*t))/ka
    A2=((r1-A1last*ka)*exp(-ka*t))/(ka-k20)-(((ka-k20)*r2+ka*r1+(-A2last-A1last)*k20*ka+A2last*k20^2)*exp(-k20*t))/(k20*ka-k20^2)+(r2+r1)/k20
  })

  env <- rxS(m)
  mod <- gsub("[-]t[*]", "-tinf*", paste0("A1last=", paste0(env$A1),"\n",
                                          "A2last=", paste0(env$A2),"\n"))

  ## With amounts at tinf, solve to steady state
  m <- RxODE({
    A1last=0
    A2last=r2/k20 - exp(-tinf*k20)*r2*(-k20 + ka)/(ka*k20 - k20^2.0)
    A1 <- A1last*exp(-t*ka)
    A2 <- A1last*ka/(ka-k20)*(exp(-t*k20)-exp(-t*ka))+A2last*exp(-t*k20)
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n")

  ## 2 compartment oral steady state rate tau
  ## First solve to tinf
  m <- RxODE({
    A1last=0
    A2last=0
    A3last=0
    r2 = 0
    A1 = r1/ka-((r1-A1last*ka)*exp(-ka*t))/ka
    A2 = (((ka-k32)*r1-A1last*ka^2+A1last*k32*ka)*exp(-ka*t))/(ka^2+(-beta-alpha)*ka+alpha*beta)+((((k32-beta)*ka-beta*k32+beta^2)*r2+(k32-beta)*ka*r1+((-A3last-A2last-A1last)*beta*k32+(A2last+A1last)*beta^2)*ka+(A3last+A2last)*beta^2*k32-A2last*beta^3)*exp(-beta*t))/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-((((k32-alpha)*ka-alpha*k32+alpha^2)*r2+(k32-alpha)*ka*r1+((-A3last-A2last-A1last)*alpha*k32+(A2last+A1last)*alpha^2)*ka+(A3last+A2last)*alpha^2*k32-A2last*alpha^3)*exp(-alpha*t))/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k32*r2+k32*r1)/(alpha*beta)
    A3 = -((k23*r1-A1last*k23*ka)*exp(-ka*t))/(ka^2+(-beta-alpha)*ka+alpha*beta)+(((k23*ka-beta*k23)*r2+k23*ka*r1+((-A2last-A1last)*beta*k23+A3last*beta^2-A3last*E2*beta)*ka+A2last*beta^2*k23-A3last*beta^3+A3last*E2*beta^2)*exp(-beta*t))/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-(((k23*ka-alpha*k23)*r2+k23*ka*r1+((-A2last-A1last)*alpha*k23+A3last*alpha^2-A3last*E2*alpha)*ka+A2last*alpha^2*k23-A3last*alpha^3+A3last*E2*alpha^2)*exp(-alpha*t))/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k23*r2+k23*r1)/(alpha*beta)
  })

  env <- rxS(m)
  mod <- gsub("[-]t[*]", "-tinf*", paste0("A1last=", paste0(env$A1),"\n",
                                          "A2last=", paste0(env$A2),"\n",
                                          "A3last=", paste0(env$A3),"\n"))

    m <-RxODE({
        A1last=r1/ka - exp(-tinf*ka)*r1/ka
A2last=r1*k32/(beta*alpha) + exp(-tinf*ka)*r1*(-k32 + ka)/(beta*alpha + ka*(-alpha - beta) + ka^2.0) - exp(-tinf*alpha)*r1*ka*(-alpha + k32)/(-beta*alpha^2.0 + ka*(beta*alpha - alpha^2.0) + alpha^3.0) + exp(-tinf*beta)*r1*ka*(-beta + k32)/(beta^2.0*alpha + ka*(-beta*alpha + beta^2.0) - beta^3.0)
        A3last=r1*k23/(beta*alpha) - exp(-tinf*ka)*r1*k23/(beta*alpha + ka*(-alpha - beta) + ka^2.0) - exp(-tinf*alpha)*r1*ka*k23/(-beta*alpha^2.0 + ka*(beta*alpha - alpha^2.0) + alpha^3.0) + exp(-tinf*beta)*r1*ka*k23/(beta^2.0*alpha + ka*(-beta*alpha + beta^2.0) - beta^3.0)

        A2term1 = (((A2last*E3+A3last*k32)-A2last*alpha)*exp(-t*alpha)-((A2last*E3+A3last*k32)-A2last*beta)*exp(-t*beta))/(beta-alpha)
        A2term2 = A1last*ka*(exp(-t*ka)*(E3-ka)/((alpha-ka)*(beta-ka))+exp(-t*alpha)*(E3-alpha)/((beta-alpha)*(ka-alpha))+exp(-t*beta)*(E3-beta)/((alpha-beta)*(ka-beta)))
        A2 = A2term1+A2term2  #Amount in the central compartment

        A3term1 = (((A3last*E2+A2last*k23)-A3last*alpha)*exp(-t*alpha)-((A3last*E2+A2last*k23)-A3last*beta)*exp(-t*beta))/(beta-alpha)
        A3term2 = A1last*ka*k23*(exp(-t*ka)/((alpha-ka)*(beta-ka))+exp(-t*alpha)/((beta-alpha)*(ka-alpha))+exp(-t*beta)/((alpha-beta)*(ka-beta)))
        A3 = A3term1+A3term2  #Amount in the peripheral compartment

        A1 = A1last*exp(-t*ka)
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n")

    message(rxOptExpr(mod))

#####
  ## r2 w/ steady state
  m <- RxODE({
    A1last=0
    A2last=0
    A3last=0
    r1=0
    A1=r1/ka-((r1-A1last*ka)*exp(-ka*t))/ka
    A2=(((ka-k32)*r1-A1last*ka^2+A1last*k32*ka)*exp(-ka*t))/(ka^2+(-beta-alpha)*ka+alpha*beta)+((((k32-beta)*ka-beta*k32+beta^2)*r2+(k32-beta)*ka*r1+((-A3last-A2last-A1last)*beta*k32+(A2last+A1last)*beta^2)*ka+(A3last+A2last)*beta^2*k32-A2last*beta^3)*exp(-beta*t))/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-((((k32-alpha)*ka-alpha*k32+alpha^2)*r2+(k32-alpha)*ka*r1+((-A3last-A2last-A1last)*alpha*k32+(A2last+A1last)*alpha^2)*ka+(A3last+A2last)*alpha^2*k32-A2last*alpha^3)*exp(-alpha*t))/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k32*r2+k32*r1)/(alpha*beta)
    A3=-((k23*r1-A1last*k23*ka)*exp(-ka*t))/(ka^2+(-beta-alpha)*ka+alpha*beta)+(((k23*ka-beta*k23)*r2+k23*ka*r1+((-A2last-A1last)*beta*k23+A3last*beta^2-A3last*E2*beta)*ka+A2last*beta^2*k23-A3last*beta^3+A3last*E2*beta^2)*exp(-beta*t))/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-(((k23*ka-alpha*k23)*r2+k23*ka*r1+((-A2last-A1last)*alpha*k23+A3last*alpha^2-A3last*E2*alpha)*ka+A2last*alpha^2*k23-A3last*alpha^3+A3last*E2*alpha^2)*exp(-alpha*t))/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k23*r2+k23*r1)/(alpha*beta)
  })

  env <- rxS(m)
  mod <- gsub("[-]t[*]", "-tinf*", paste0("A1last=", paste0(env$A1),"\n",
                                          "A2last=", paste0(env$A2),"\n",
                                          "A3last=", paste0(env$A3),"\n"))

  m <-RxODE({
    A1last=0
A2last=r2*k32/(beta*alpha) - exp(-tinf*alpha)*r2*(-k32*alpha + ka*(-alpha + k32) + alpha^2.0)/(-beta*alpha^2.0 + ka*(beta*alpha - alpha^2.0) + alpha^3.0) + exp(-tinf*beta)*r2*(-k32*beta + ka*(-beta + k32) + beta^2.0)/(beta^2.0*alpha + ka*(-beta*alpha + beta^2.0) - beta^3.0)
A3last=r2*k23/(beta*alpha) - exp(-tinf*alpha)*r2*(-k23*alpha + ka*k23)/(-beta*alpha^2.0 + ka*(beta*alpha - alpha^2.0) + alpha^3.0) + exp(-tinf*beta)*r2*(-k23*beta + ka*k23)/(beta^2.0*alpha + ka*(-beta*alpha + beta^2.0) - beta^3.0)
    A2term1 = (((A2last*E3+A3last*k32)-A2last*alpha)*exp(-t*alpha)-((A2last*E3+A3last*k32)-A2last*beta)*exp(-t*beta))/(beta-alpha)
    A2term2 = A1last*ka*(exp(-t*ka)*(E3-ka)/((alpha-ka)*(beta-ka))+exp(-t*alpha)*(E3-alpha)/((beta-alpha)*(ka-alpha))+exp(-t*beta)*(E3-beta)/((alpha-beta)*(ka-beta)))
    A2 = A2term1+A2term2  #Amount in the central compartment

    A3term1 = (((A3last*E2+A2last*k23)-A3last*alpha)*exp(-t*alpha)-((A3last*E2+A2last*k23)-A3last*beta)*exp(-t*beta))/(beta-alpha)
    A3term2 = A1last*ka*k23*(exp(-t*ka)/((alpha-ka)*(beta-ka))+exp(-t*alpha)/((beta-alpha)*(ka-alpha))+exp(-t*beta)/((alpha-beta)*(ka-beta)))
    A3 = A3term1+A3term2  #Amount in the peripheral compartment

    A1last = A1last*exp(-t*ka)
    A1 = A1last
    A2 = A2
  })

  env <- rxS(m)

  mod <- gsub("\\b(ka|k[1-4][1-4]|r[1-2])\\b", "(*\\1)",
              gsub("exp[(]-tinf[*]beta[)]", "eiB",
             gsub("exp[(]-tinf[*]alpha[)]", "eiA",
                  gsub("exp[(]-t[*]alpha[)]", "eA",
                       gsub("exp[(]-t[*]beta[)]", "eB",
                       gsub("exp(-t*ka)", "eKa",
             gsub("exp(-ka*tinf)", "eiKa",
                  gsub("(alpha|beta)\\^([1-4])[.][0]", "\\1\\2",
                       paste0("A1=", paste0(env$A1),"\n",
                              "A2=", paste0(env$A2),"\n",
                              "A3=", paste0(env$A3),"\n")), fixed=TRUE), fixed=TRUE))))), perl=TRUE)

  ## 3 compartment oral steady state infusion with tau
  m <- RxODE({
    A1last=0
    A2last=0
    A3last=0
    A4last=0
    r2=0
    A1 = r1/ka-((r1-A1last*ka)*exp(-ka*t))/ka
    A2 = -(((lam3^3+(-ka-k42-k32)*lam3^2+((k42+k32)*ka+k32*k42)*lam3-k32*k42*ka)*r2+(-ka*lam3^2+(k42+k32)*ka*lam3-k32*k42*ka)*r1-A2last*lam3^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam3^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam3^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)+(((lam2^3+(-ka-k42-k32)*lam2^2+((k42+k32)*ka+k32*k42)*lam2-k32*k42*ka)*r2+(-ka*lam2^2+(k42+k32)*ka*lam2-k32*k42*ka)*r1-A2last*lam2^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam2^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam2^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)-(((lam1^3+(-ka-k42-k32)*lam1^2+((k42+k32)*ka+k32*k42)*lam1-k32*k42*ka)*r2+(-ka*lam1^2+(k42+k32)*ka*lam1-k32*k42*ka)*r1-A2last*lam1^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam1^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam1^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)-(((ka^2+(-k42-k32)*ka+k32*k42)*r1-A1last*ka^3+(A1last*k42+A1last*k32)*ka^2-A1last*k32*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k32*k42*r2+k32*k42*r1)/(lam1*lam2*lam3)
    A3 = (((k23*lam3^2+(-k23*ka-k23*k42)*lam3+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam3)*r1+A3last*lam3^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam3^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam3^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k23*lam2^2+(-k23*ka-k23*k42)*lam2+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam2)*r1+A3last*lam2^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam2^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam2^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k23*lam1^2+(-k23*ka-k23*k42)*lam1+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam1)*r1+A3last*lam1^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam1^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam1^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k23*ka-k23*k42)*r1-A1last*k23*ka^2+A1last*k23*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k23*k42*r2+k23*k42*r1)/(lam1*lam2*lam3)
    A4 = (((k24*lam3^2+(-k24*ka-k24*k32)*lam3+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam3)*r1+A4last*lam3^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam3^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam3^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k24*lam2^2+(-k24*ka-k24*k32)*lam2+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam2)*r1+A4last*lam2^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam2^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam2^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k24*lam1^2+(-k24*ka-k24*k32)*lam1+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam1)*r1+A4last*lam1^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam1^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam1^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k24*ka-k24*k32)*r1-A1last*k24*ka^2+A1last*k24*k32*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k24*k32*r2+k24*k32*r1)/(lam1*lam2*lam3)
  })

  env <- rxS(m)
  mod <- gsub("[-]t[*]", "-tinf*", paste0("A1last=", paste0(env$A1),"\n",
                                          "A2last=", paste0(env$A2),"\n",
                                          "A3last=", paste0(env$A3),"\n",
                                          "A4last=", paste0(env$A4),"\n"))

  m2 <- RxODE(paste0(mod,"\n",
                    gsub("KA", "ka", gsub("\\blambda([1-3])\\b", "lam\\1", "\nB = A3last*k32+A4last*k42
    C = E4*A3last*k32+E3*A4last*k42
    I = A2last*k23*E4-A3last*k24*k42+A4last*k23*k42
    J = A2last*k24*E3+A3last*k24*k32-A4last*k23*k32

    A2term1 = A2last*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))
    A2term3 = A1last*KA*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E3-KA)*(E4-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    A2 = A2term1+A2term2+A2term3   #Amount in the central compartment

    A3term1 = A3last*(exp(-t*lambda1)*(E2-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(I-A2last*k23*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k23*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k23*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))
    A3term3 = A1last*KA*k23*(exp(-t*lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E4-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    A3 = A3term1+A3term2+A3term3  #Amount in the first-peripheral compartment

    A4term1 = A4last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A4term2 = exp(-t*lambda1)*(J-A2last*k24*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k24*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k24*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))
    A4term3 = A1last*KA*k24*(exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E3-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    A4 = A4term1+A4term2+A4term3  #Amount in the second-peripheral compartment

    A1 = A1last*exp(-t*KA)
"))))

  env <- rxS(m2)

  mod2 <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n",
                "A4=", paste0(env$A4),"\n")

  m2 <- gsub("\\b(ka|k[1-4][1-4]|r[1])\\b", "(*\\1)",
             gsub("exp[(]-lam([1-3])[*]tinf[)]", "eiL\\1",
             gsub("exp[(]-t[*]lam([1-3])[)]", "eL\\1", gsub("exp(-t*ka)", "eKa",
             gsub("exp(-ka*tinf)", "eiKa",
                  gsub("(lam[1-4]|ka)\\^([1-4])[.][0]", "\\1\\2",
                       mod2), fixed=TRUE), fixed=TRUE))), perl=TRUE)

    message(rxOptExpr(mod))


    m <- RxODE({
    A1last=0
    A2last=0
    A3last=0
    A4last=0
    r1=0
    A1=r1/ka-((r1-A1last*ka)*exp(-ka*t))/ka
    A2=-(((lam3^3+(-ka-k42-k32)*lam3^2+((k42+k32)*ka+k32*k42)*lam3-k32*k42*ka)*r2+(-ka*lam3^2+(k42+k32)*ka*lam3-k32*k42*ka)*r1-A2last*lam3^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam3^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam3^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)+(((lam2^3+(-ka-k42-k32)*lam2^2+((k42+k32)*ka+k32*k42)*lam2-k32*k42*ka)*r2+(-ka*lam2^2+(k42+k32)*ka*lam2-k32*k42*ka)*r1-A2last*lam2^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam2^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam2^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)-(((lam1^3+(-ka-k42-k32)*lam1^2+((k42+k32)*ka+k32*k42)*lam1-k32*k42*ka)*r2+(-ka*lam1^2+(k42+k32)*ka*lam1-k32*k42*ka)*r1-A2last*lam1^4+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam1^3+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam1^2+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)-(((ka^2+(-k42-k32)*ka+k32*k42)*r1-A1last*ka^3+(A1last*k42+A1last*k32)*ka^2-A1last*k32*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k32*k42*r2+k32*k42*r1)/(lam1*lam2*lam3)
    A3=(((k23*lam3^2+(-k23*ka-k23*k42)*lam3+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam3)*r1+A3last*lam3^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam3^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam3^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k23*lam2^2+(-k23*ka-k23*k42)*lam2+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam2)*r1+A3last*lam2^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam2^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam2^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k23*lam1^2+(-k23*ka-k23*k42)*lam1+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam1)*r1+A3last*lam1^4+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam1^3+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam1^2+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k23*ka-k23*k42)*r1-A1last*k23*ka^2+A1last*k23*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k23*k42*r2+k23*k42*r1)/(lam1*lam2*lam3)
    A4=(((k24*lam3^2+(-k24*ka-k24*k32)*lam3+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam3)*r1+A4last*lam3^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam3^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam3^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam3)*exp(-lam3*t))/(lam3^4+(-lam2-lam1-ka)*lam3^3+((lam1+ka)*lam2+ka*lam1)*lam3^2-ka*lam1*lam2*lam3)-(((k24*lam2^2+(-k24*ka-k24*k32)*lam2+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam2)*r1+A4last*lam2^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam2^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam2^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam2)*exp(-lam2*t))/((lam2^3+(-lam1-ka)*lam2^2+ka*lam1*lam2)*lam3-lam2^4+(lam1+ka)*lam2^3-ka*lam1*lam2^2)+(((k24*lam1^2+(-k24*ka-k24*k32)*lam1+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam1)*r1+A4last*lam1^4+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam1^3+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam1^2+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam1)*exp(-lam1*t))/(((lam1^2-ka*lam1)*lam2-lam1^3+ka*lam1^2)*lam3+(ka*lam1^2-lam1^3)*lam2+lam1^4-ka*lam1^3)+(((k24*ka-k24*k32)*r1-A1last*k24*ka^2+A1last*k24*k32*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka^2)*lam3+(ka^2-ka*lam1)*lam2+ka^2*lam1-ka^3)+(k24*k32*r2+k24*k32*r1)/(lam1*lam2*lam3)
  })

  env <- rxS(m)
  mod <- gsub("[-]t[*]", "-tinf*", paste0("A1last=", paste0(env$A1),"\n",
                                          "A2last=", paste0(env$A2),"\n",
                                          "A3last=", paste0(env$A3),"\n",
                                          "A4last=", paste0(env$A4),"\n"))

  m <- RxODE({
    A1last=0
    A2last=-exp(-tinf*lam1)*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam1^2.0 - ka*k42*k32 + lam1^3.0)/(-ka*lam1^3.0 + lam2*(ka*lam1^2.0 - lam1^3.0) + lam3*(ka*lam1^2.0 + lam2*(-ka*lam1 + lam1^2.0) - lam1^3.0) + lam1^4.0) + exp(-tinf*lam2)*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam2^2.0 - ka*k42*k32 + lam2^3.0)/(lam2^3.0*(ka + lam1) + lam3*(lam2^2.0*(-ka - lam1) + ka*lam2*lam1 + lam2^3.0) - ka*lam2^2.0*lam1 - lam2^4.0) - exp(-tinf*lam3)*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam3^2.0 - ka*k42*k32 + lam3^3.0)/(lam3^2.0*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam3^3.0 - ka*lam2*lam1*lam3 + lam3^4.0) + r2*k42*k32/(lam2*lam1*lam3)
    A3last=exp(-tinf*lam1)*r2*(k23*lam1^2.0 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam1^3.0 + lam2*(ka*lam1^2.0 - lam1^3.0) + lam3*(ka*lam1^2.0 + lam2*(-ka*lam1 + lam1^2.0) - lam1^3.0) + lam1^4.0) - exp(-tinf*lam2)*r2*(k23*lam2^2.0 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam2^3.0*(ka + lam1) + lam3*(lam2^2.0*(-ka - lam1) + ka*lam2*lam1 + lam2^3.0) - ka*lam2^2.0*lam1 - lam2^4.0) + exp(-tinf*lam3)*r2*(k23*lam3^2.0 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam3^2.0*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam3^3.0 - ka*lam2*lam1*lam3 + lam3^4.0) + r2*k42*k23/(lam2*lam1*lam3)
    A4last=exp(-tinf*lam1)*r2*(k24*lam1^2.0 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam1^3.0 + lam2*(ka*lam1^2.0 - lam1^3.0) + lam3*(ka*lam1^2.0 + lam2*(-ka*lam1 + lam1^2.0) - lam1^3.0) + lam1^4.0) - exp(-tinf*lam2)*r2*(k24*lam2^2.0 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam2^3.0*(ka + lam1) + lam3*(lam2^2.0*(-ka - lam1) + ka*lam2*lam1 + lam2^3.0) - ka*lam2^2.0*lam1 - lam2^4.0) + exp(-tinf*lam3)*r2*(k24*lam3^2.0 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam3^2.0*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam3^3.0 - ka*lam2*lam1*lam3 + lam3^4.0) + r2*k24*k32/(lam2*lam1*lam3)
    B = A3last*k32+A4last*k42
    C = E4*A3last*k32+E3*A4last*k42
    I = A2last*k23*E4-A3last*k24*k42+A4last*k23*k42
    J = A2last*k24*E3+A3last*k24*k32-A4last*k23*k32

    A2term1 = A2last*(exp(-t*lam1)*(E3-lam1)*(E4-lam1)/((lam2-lam1)*(lam3-lam1))+exp(-t*lam2)*(E3-lam2)*(E4-lam2)/((lam1-lam2)*(lam3-lam2))+exp(-t*lam3)*(E3-lam3)*(E4-lam3)/((lam1-lam3)*(lam2-lam3)))
    A2term2 = exp(-t*lam1)*(C-B*lam1)/((lam1-lam2)*(lam1-lam3))+exp(-t*lam2)*(B*lam2-C)/((lam1-lam2)*(lam2-lam3))+exp(-t*lam3)*(B*lam3-C)/((lam1-lam3)*(lam3-lam2))
    A2term3 = A1last*ka*(exp(-t*lam1)*(E3-lam1)*(E4-lam1)/((lam2-lam1)*(lam3-lam1)*(ka-lam1))+exp(-t*lam2)*(E3-lam2)*(E4-lam2)/((lam1-lam2)*(lam3-lam2)*(ka-lam2))+exp(-t*lam3)*(E3-lam3)*(E4-lam3)/((lam1-lam3)*(lam2-lam3)*(ka-lam3))+exp(-t*ka)*(E3-ka)*(E4-ka)/((lam1-ka)*(lam2-ka)*(lam3-ka)))
    A2 = A2term1+A2term2+A2term3   #Amount in the central compartment

    A3term1 = A3last*(exp(-t*lam1)*(E2-lam1)*(E4-lam1)/((lam2-lam1)*(lam3-lam1))+exp(-t*lam2)*(E2-lam2)*(E4-lam2)/((lam1-lam2)*(lam3-lam2))+exp(-t*lam3)*(E2-lam3)*(E4-lam3)/((lam1-lam3)*(lam2-lam3)))
    A3term2 = exp(-t*lam1)*(I-A2last*k23*lam1)/((lam1-lam2)*(lam1-lam3))+exp(-t*lam2)*(A2last*k23*lam2-I)/((lam1-lam2)*(lam2-lam3))+exp(-t*lam3)*(A2last*k23*lam3-I)/((lam1-lam3)*(lam3-lam2))
    A3term3 = A1last*ka*k23*(exp(-t*lam1)*(E4-lam1)/((lam2-lam1)*(lam3-lam1)*(ka-lam1))+exp(-t*lam2)*(E4-lam2)/((lam1-lam2)*(lam3-lam2)*(ka-lam2))+exp(-t*lam3)*(E4-lam3)/((lam1-lam3)*(lam2-lam3)*(ka-lam3))+exp(-t*ka)*(E4-ka)/((lam1-ka)*(lam2-ka)*(lam3-ka)))
    A3 = A3term1+A3term2+A3term3  #Amount in the first-peripheral compartment

    A4term1 = A4last*(exp(-t*lam1)*(E2-lam1)*(E3-lam1)/((lam2-lam1)*(lam3-lam1))+exp(-t*lam2)*(E2-lam2)*(E3-lam2)/((lam1-lam2)*(lam3-lam2))+exp(-t*lam3)*(E2-lam3)*(E3-lam3)/((lam1-lam3)*(lam2-lam3)))
    A4term2 = exp(-t*lam1)*(J-A2last*k24*lam1)/((lam1-lam2)*(lam1-lam3))+exp(-t*lam2)*(A2last*k24*lam2-J)/((lam1-lam2)*(lam2-lam3))+exp(-t*lam3)*(A2last*k24*lam3-J)/((lam1-lam3)*(lam3-lam2))
    A4term3 = A1last*ka*k24*(exp(-t*lam1)*(E3-lam1)/((lam2-lam1)*(lam3-lam1)*(ka-lam1))+exp(-t*lam2)*(E3-lam2)/((lam1-lam2)*(lam3-lam2)*(ka-lam2))+exp(-t*lam3)*(E3-lam3)/((lam1-lam3)*(lam2-lam3)*(ka-lam3))+exp(-t*ka)*(E3-ka)/((lam1-ka)*(lam2-ka)*(lam3-ka)))
    A4 = A4term1+A4term2+A4term3  #Amount in the second-peripheral compartment

    A1last = A1last*exp(-t*ka)
    A1 = A1last
  })

  env <- rxS(m)
  mod <- paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n",
                "A4=", paste0(env$A4),"\n")


  ## 3 compartment bolus steady state redo

  m <- RxODE({
    A1last <- b1
    A2last <- 0
    A3last <- 0
    k20 <- 0
    k30 <- 0

    B = A2last*k21+A3last*k31
    C = E3*A2last*k21+E2*A3last*k31
    I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31
    J = A1last*k13*E2+A2last*k13*k21-A3last*k12*k21

    A1term1 = A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A1term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))

    A1 <- (A1term1+A1term2)  #Amount in the central compartment

    A2term1 = A2last*(exp(-t*lambda1)*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))

    A2 <- A2term1+A2term2             #Amount in the first-peripheral compartment

    A3term1 = A3last*(exp(-t*lambda1)*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))

    A3 <- A3term1+A3term2            #Amount in the second-peripheral compartment
  })

  env <- rxS(m)

  mod <- gsub("\\b(ka|k[1-4][1-4]|[rb][1-2])\\b", "(*\\1)",
              gsub("exp[(]-t[*]lambda([1-3])[)]", "eL\\1", paste0("A1=", paste0(env$A1),"\n",
                "A2=", paste0(env$A2),"\n",
                "A3=", paste0(env$A3),"\n")))

  env <- rxS("A1last=0
A2last=0
A3last=0
r2=0
A1=r1/ka-((r1-A1last*ka)*exp(-ka*t))/ka
A2=(((ka-k32)*r1-A1last*ka^2+A1last*k32*ka)*exp(-ka*t))/(ka^2+(-beta-alpha)*ka+alpha*beta)+((((k32-beta)*ka-beta*k32+beta^2)*r2+(k32-beta)*ka*r1+((-A3last-A2last-A1last)*beta*k32+(A2last+A1last)*beta^2)*ka+(A3last+A2last)*beta^2*k32-A2last*beta^3)*exp(-beta*t))/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-((((k32-alpha)*ka-alpha*k32+alpha^2)*r2+(k32-alpha)*ka*r1+((-A3last-A2last-A1last)*alpha*k32+(A2last+A1last)*alpha^2)*ka+(A3last+A2last)*alpha^2*k32-A2last*alpha^3)*exp(-alpha*t))/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k32*r2+k32*r1)/(alpha*beta)
A3=-((k23*r1-A1last*k23*ka)*exp(-ka*t))/(ka^2+(-beta-alpha)*ka+alpha*beta)+(((k23*ka-beta*k23)*r2+k23*ka*r1+((-A2last-A1last)*beta*k23+A3last*beta^2-A3last*E2*beta)*ka+A2last*beta^2*k23-A3last*beta^3+A3last*E2*beta^2)*exp(-beta*t))/((beta^2-alpha*beta)*ka-beta^3+alpha*beta^2)-(((k23*ka-alpha*k23)*r2+k23*ka*r1+((-A2last-A1last)*alpha*k23+A3last*alpha^2-A3last*E2*alpha)*ka+A2last*alpha^2*k23-A3last*alpha^3+A3last*E2*alpha^2)*exp(-alpha*t))/((alpha*beta-alpha^2)*ka-alpha^2*beta+alpha^3)+(k23*r2+k23*r1)/(alpha*beta)")

  env2 <- rxS(paste0(gsub("-t[*]", "-tinf*", paste0("A1last=", paste0(env$A1),"\n",
         "A2last=", paste0(env$A2),"\n",
         "A3last=", paste0(env$A3),"\n")),
         gsub("lambda2", "beta", (gsub("lambda1", "alpha", gsub("KA", "ka", "
E2=k20+k23
E3=k32
A2term1 = (((A2last*E3+A3last*k32)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E3+A3last*k32)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A2term2 = A1last*KA*(exp(-t*KA)*(E3-KA)/((lambda1-KA)*(lambda2-KA))+exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(KA-lambda2)))
    A2 = A2term1+A2term2  #Amount in the central compartment

    A3term1 = (((A3last*E2+A2last*k23)-A3last*lambda1)*exp(-t*lambda1)-((A3last*E2+A2last*k23)-A3last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A3term2 = A1last*KA*k23*(exp(-t*KA)/((lambda1-KA)*(lambda2-KA))+exp(-t*lambda1)/((lambda2-lambda1)*(KA-lambda1))+exp(-t*lambda2)/((lambda1-lambda2)*(KA-lambda2)))
    A3 = A3term1+A3term2  #Amount in the peripheral compartment

    A1 = A1last*exp(-t*KA)"))))))


  m <- gsub("\\b(ka|k[1-4][0-4]|r[1-2])\\b", "(*\\1)",
              gsub("exp[(]-tinf[*]beta[)]", "eiB",
             gsub("exp[(]-tinf[*]alpha[)]", "eiA",
                  gsub("exp[(]-t[*]alpha[)]", "eA",
                       gsub("exp[(]-t[*]beta[)]", "eB",
                            gsub("exp(-t*ka)", "eKa",
                                 gsub("exp(-ka*tinf)", "eiKa",
                                      gsub("(alpha|beta|ka)\\^([1-4])[.][0]", "\\1\\2",
                                           paste0("A1=", env2$A1,"\n",
                                                  "A2=", env2$A2,"\n",
                                                  "A3=", env2$A3,"\n")), fixed=TRUE), fixed=TRUE))))),
       perl=TRUE)



}
