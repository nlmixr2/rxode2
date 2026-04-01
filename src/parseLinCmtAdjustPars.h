static inline void assertQorCldNeedsCl(linCmtStruct *lin){
  if (lin->cl == -1){
    if (lin->clStyle == linCmtCld1style){
      _rxode2parse_unprotect();
      err_trans("'Cld' parameterization needs 'Cl'");
    } else {
      parseFree(0);
      reset();
      _rxode2parse_unprotect();
      err_trans("'Q' parameterization needs 'Cl'");
    }
  }
}

static inline int linCmtAdjustParsQstyleOrCldStyleCl1(linCmtStruct *lin) {
  if (lin->cl1 != -1) {
    if (lin->cl2  != -1) {
      // Cl, Q, Q1
      if (lin->clStyle == linCmtQstyle){
        _rxode2parse_unprotect();
        err_trans("cannot mix 'Q' and 'Q1'");
      } else {
        _rxode2parse_unprotect();
        err_trans("cannot mix 'Cld' and 'Cld1'");
      }
    } else if (lin->cl3 != -1) {
      // Cl, Q (cl1->cl2), Q2 (cl3->cl3)
      lin->cl2 = lin->cl1;
      lin->cl1 = -1;
    } else if (lin->cl4 != -1){
      // Cl, Q, Q3
      if (lin->clStyle == linCmtQstyle){
        _rxode2parse_unprotect();
        err_trans("cannot mix 'Q' and 'Q3'");
      } else {
        _rxode2parse_unprotect();
        err_trans("cannot mix 'Cld' and 'Cld3'");
      }
    } else {
      // Cl, Q (cl1->cl2), Q2 (cl3->cl3)
      lin->cl2 = lin->cl1;
      lin->cl1 = -1;
    }
    return 1;
  }
  return 0;
}

static inline int linCmtAdjustParsQstyleOrCldStyleCl2(linCmtStruct *lin) {
  if (lin->cl2  != -1) {
    // Cl, Q1
    if (lin->cl4 != -1) {
      if (lin->clStyle == linCmtQstyle){
        _rxode2parse_unprotect();
        err_trans("cannot mix 'Q1' and 'Q3'");
      } else {
        _rxode2parse_unprotect();
        err_trans("cannot mix 'Cld1' and 'Cld3'");
      }
    }
    return 1;
  }
  return 0;
}

static inline int linCmtAdjustParsQstyleOrCldStyleCl3(linCmtStruct *lin) {
  if (lin->cl3 != -1){
    lin->cl2 = lin->cl3;
    lin->cl3 = lin->cl4;
    return 1;
  }
  return 0;
}

static inline int linCmtAdjustParsQstyleOrCldStyle(linCmtStruct *lin) {
  if (lin->clStyle == linCmtQstyle || lin->clStyle == linCmtCld1style) {
    // cl,
    assertQorCldNeedsCl(lin);
    int tmp = linCmtAdjustParsQstyleOrCldStyleCl1(lin) ||
      linCmtAdjustParsQstyleOrCldStyleCl2(lin) ||
      linCmtAdjustParsQstyleOrCldStyleCl3(lin);
    (void)tmp;
    return 1;
  }
  return 0;
}

static inline int linCmtAdjustParsClNumStyle(linCmtStruct *lin) {
  if (lin->cl1 != -1){
    // Cl1, Cl2, Cl3
    // -> cl, cl2, cl3
    if (lin->cl != -1) {
      // cl, cl1,
      if (lin->cl2 == -1){
        if (lin->cl4 != -1){
          _rxode2parse_unprotect();
          err_trans("error parsing higher 'cl'");
        }
        lin->cl4 = lin->cl3;
        lin->cl3 = lin->cl2;
        lin->cl2 = lin->cl1;
        lin->cl1 = -1;
      } else {
        _rxode2parse_unprotect();
        err_trans("cannot mix 'Cl' and 'Cl1'");
      }
    } else {
      linCmtCmt(lin, 1);
      lin->cl = lin->cl1;
      lin->cl1 = -1;
      if (lin->cl4 != -1){
        _rxode2parse_unprotect();
        err_trans("specified clearance for 4th compartment, which does not make sense in this context");
      }
    }
  } else if (lin->cl2 != -1){
    if (lin->cl == -1){
      //  Cl2, Cl3, Cl4
      // -> Cl, cl2, cl3
      linCmtCmt(lin, 2);
    } else if (lin->cl4 != -1) {
      // Cl, Cl2, Cl3 keeps the same;  Cl4 doesn't make sense
      _rxode2parse_unprotect();
      err_trans("specified clearance for 4th compartment, which does not make sense in this context");
    }
  } else if (lin->cl != -1){
    if (lin->cl3 != -1){
      // Cl, Cl3, Cl4
      //-> Cl, Cl2, cl3
      lin->cl2 = lin->cl3;
      lin->cl3 = lin->cl4;
      lin->cl4 = -1;
    }
  }
  return 0;
}

static inline int linCmtAdjustParsV(linCmtStruct *lin) {
  if (lin->v != -1) {
    if (lin->v1 != -1){
      _rxode2parse_unprotect();
      err_trans("Cannot specify 'v1' and 'vc'");
    }
    if (lin->v4 != -1){
      _rxode2parse_unprotect();
      err_trans("Cannot specify 'v4' and 'vc'");
    }
    if (lin->v2 != -1) {
      // v, v2, v3; Central Compartment is 1
      linCmtCmt(lin, 1);
      linCmtVStyle(lin, 4); // V#
    } else if (lin->v3 != -1) {
      // v, v3, v4; Central compartment is 2
      linCmtCmt(lin, 2);
      linCmtVStyle(lin, 4); // V#
      lin->v2 = lin->v3;
      lin->v3 = lin->v4;
    } else if (lin->vp != -1){
      lin->v2 = lin->vp;
      if (lin->vp1 != -1){
        // v, vp, vp, vp1
        lin->v3 = lin->vp1;
      } else if (lin->vp2 != -1) {
        // v, vp, vp, vp2
        lin->v3 = lin->vp2;
      } else if (lin->vp3 != -1) {
        // v, vp, vp, vp3
        linCmtCmt(lin, 1);
        linCmtCmt(lin, 2);
      }
    } else if (lin->vp1 != -1) {
      // v, vp1, vp2
      lin->v2 = lin->vp1;
      lin->v3 = lin->vp2;
    } else if (lin->vp2 != -1) {
      lin->v2 = lin->vp2;
      lin->v3 = lin->vp3;
    }
    return 1;
  }
  return 0;
}

static inline int linCmtAdjustParsV1(linCmtStruct *lin) {
  if (lin->v1 != -1) {
    linCmtCmt(lin, 1);
    lin->v = lin->v1;
    if (lin->v2 != -1) {
      // v1, v2, v3; Central Compartment is 1
      linCmtCmt(lin, 1);
      linCmtVStyle(lin, 4); // V#
    } else if (lin->v3 != -1) {
      // v1, v3, v4; Central compartment is 2
      linCmtCmt(lin, 2);
    } else if (lin->vp != -1){
      // v1, vp,
      lin->v2 = lin->vp;
      if (lin->vp1 != -1){
        // v1, vp, vp1
        lin->v3 = lin->vp1;
      } else if (lin->vp2 != -1) {
        // v, vp, vp2
        lin->v3 = lin->vp2;
      } else if (lin->vp3 != -1) {
        linCmtCmt(lin, 2);
      }
    }
    return 1;
  }
  return 0;
}

static inline int linCmtAdjustParsV2(linCmtStruct *lin) {
  if (lin->v2 != -1) {
    linCmtCmt(lin, 2);
    lin->v = lin->v2;
    lin->v2 = -1;
    if (lin->v3 != -1) {
      // v2, v3, v4; Central compartment is 2
      lin->v2 = lin->v3;
      lin->v3 = lin->v4;
    } else if (lin->vp != -1){
      // v2, vp,
      lin->v2 = lin->vp;
      if (lin->vp1 != -1){
        // v2, vp, vp1
        lin->v3 = lin->vp1;
      } else if (lin->vp2 != -1) {
        // v2, vp, vp2
        lin->v3 = lin->vp2;
      } else if (lin->vp3 != -1) {
        linCmtCmt(lin, 2);
      }
    }
    return 1;
  }
  return 0;
}

static inline void assertCorrectClV(linCmtStruct *lin) {
  if (lin->cl != -1 && lin->v != -1) {
    if (lin->cl2 != -1) {
      if (lin->v2 == -1 && lin->vss == -1) {
        _rxode2parse_unprotect();
        err_trans("can find distributional clearance but not peripheral volume");
      }
    }
    if (lin->v2 != -1) {
      if (lin->cl2 == -1) {
        _rxode2parse_unprotect();
        err_trans("can find peripheral volume but not distributional clearance");
      }
    }
    if (lin->cl3 != -1) {
      if (lin->v3 == -1) {
        _rxode2parse_unprotect();
        err_trans("can find 2nd distributional clearance but not 2nd peripheral volume");
      }
    }
    if (lin->v3 != -1) {
      if (lin->cl3 == -1) {
        _rxode2parse_unprotect();
        err_trans("can find 2nd peripheral volume but not 2nd distributional clearance");
      }
    }
  }
}

static inline void assertCorrectV(linCmtStruct *lin) {
  if (lin->v != -1 && lin->v2 != -1) {
    if (lin->v == lin->v2) {
      _rxode2parse_unprotect();
      err_trans("cannot distinguish between central and peripheral volumes");
    }
  }
  if (lin->v2 != -1 && lin->v3 != -1) {
    if (lin->v2 == lin->v3) {
      _rxode2parse_unprotect();
      err_trans("cannot distinguish between 1st and 2nd peripheral volumes");
    }
  }

  if (lin->cl != -1 && lin->cl2 != -1) {
    if (lin->cl == lin->cl2) {
      _rxode2parse_unprotect();
      err_trans("cannot distinguish between central and peripheral clearances");
    }
  }
  if (lin->cl2 != -1 && lin->cl3 != -1) {
    if (lin->cl2 == lin->cl3) {
      _rxode2parse_unprotect();
      err_trans("cannot distinguish between 1st and 2nd distributional clearances");
    }
  }
}


static inline void linCmtAdjustPars(linCmtStruct *lin) {
  int tmp = linCmtAdjustParsQstyleOrCldStyle(lin) ||
    linCmtAdjustParsClNumStyle(lin);
  tmp = linCmtAdjustParsV(lin) ||
    linCmtAdjustParsV1(lin) ||
    linCmtAdjustParsV2(lin);
  assertCorrectClV(lin);
  assertCorrectV(lin);
  (void)tmp;
}
