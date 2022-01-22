test_that("parsing for nlmixr; Issue RxODE#281", {
  .mv <- rxModelVars("Resp(0)=1;\nemaxD=exp(temaxD);\nec50=exp(tec50);\nemaxT=exp(temaxT);\net50=exp(tet50);\nslope=exp(tslope);\ndelay=exp(tdelay);\nkin=exp(tkin);\nkout=kin;\nGAM=exp(1)/2;\nC2=centr/V;\nCONC=(C2)^2;\nStim1=emaxT*(t)/(t+et50);\nStim2=emaxD*(CONC^GAM)/(CONC^GAM+ec50^GAM);\nStim=Stim1*(1+TRX*Stim2);\nDelta=1/(1+exp(-20*(t-delay)));\nd/dt(depot)=-KA*depot;\nd/dt(centr)=KA*depot-CL*C2;\nd/dt(Resp)=kin*(1+Delta*slope)-kout*(1+Stim)*Resp;\nnlmixr_pred=Resp;\nrx_yj_~2;\nrx_lambda_~1;\nrx_hi_~1;\nrx_low_~0;\nrx_pred_f_~nlmixr_pred;\nrx_pred_=nlmixr_pred;\nrx_r_=(nlmixrAdd)^2;\n")
  
  expect_error(rxS(.mv, TRUE, FALSE), NA)
  
  .mv <- rxModelVars("ktr=exp(tktr);\nka=exp(tka);\ncl=exp(tcl);\nv=exp(tv);\nemax=expit(temax);\nec50=exp(tec50);\nkout=exp(tkout);\ne0=exp(te0);\nDCP=center/v;\nPD=1-emax*DCP/(ec50+DCP);\neffect(0)=e0;\nkin=e0*kout;\nd/dt(depot)=-ktr*depot;\nd/dt(gut)=ktr*depot-ka*gut;\nd/dt(center)=ka*gut-cl/v*center;\nd/dt(effect)=kin*PD-kout*effect;\ncp=center/v;\nnlmixr_pred=(CMT==5)*(cp);\nnlmixr_pred=(CMT==6)*(effect)+(1-((CMT==6)))*(nlmixr_pred);\ncmt(cp);\ncmt(pca);\nrx_yj_~2;\nrx_lambda_~1;\nrx_hi_~1;\nrx_low_~0;\nrx_pred_f_~nlmixr_pred;\nrx_pred_=nlmixr_pred;\nrx_r_=(nlmixrAdd)^2;\n")
  
  expect_error(rxS(.mv, TRUE, FALSE), NA)
})
