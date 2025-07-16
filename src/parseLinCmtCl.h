static inline int isLinCmtK(linCmtStruct *lin, const char *in, int *index) {
  if (in[1] == '\0') {
    lin->kel = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtKeOrKel(linCmtStruct *lin, const char *in, int *index) {
  if ((in[1] == 'e' || in[1] == 'E')) {
    if (in[2] == '\0') {
      if (lin->kel != -1) {
        _rxode2parse_unprotect();
        err_trans("Ambiguous 'kel'");
      }
      lin->kel = *index;
      return 1;
    }
    if ((in[2] == 'l' || in[2] == 'L') && in[3] == '\0') {
      if (lin->kel != -1) {
        _rxode2parse_unprotect();
        err_trans("Ambiguous 'kel'");
      }
      lin->kel = *index;
      return 1;
    }
  }
  return 0;
}

static inline int isLinCmtKa(linCmtStruct *lin, const char *in, int *index) {
  if ((in[1] == 'a' || in[1] == 'A') &&
      in[2] == '\0') {
    lin->ka = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtK10orK12orK13(linCmtStruct *lin, const char *in, int *index) {
  if (in[1] == '1') {
    if (in[2] == '0' && in[3] == '\0') {
      linCmtCmt(lin, 1);
      if (lin->kel != -1) {
        _rxode2parse_unprotect();
        err_trans("Ambiguous 'kel'");
      }
      lin->kel = *index;
      return 1;
    }
    if (in[2] == '2' && in[3] == '\0') {
      linCmtCmt(lin, 1);
      lin->k12 = *index;
      return 1;
    }
    if (in[2] == '3' && in[3] == '\0') {
      linCmtCmt(lin, 1);
      lin->k13 = *index;
      return 1;
    }
  }
  return 0;
}

static inline int isLinCmtK20orK21orK23orK24(linCmtStruct *lin, const char *in, int *index){
  if (in[1] == '2') {
    if (in[2] == '0' && in[3] == '\0') {
      linCmtCmt(lin, 2);
      if (lin->kel != -1) {
        _rxode2parse_unprotect();
        err_trans("Ambiguous 'kel'");
      }
      lin->kel = *index;
      return 1;
    }
    if (in[2] == '1' && in[3] == '\0') {
      linCmtCmt(lin, 1);
      lin->k21 = *index;
      return 1;
    }
    if (in[2] == '3' && in[3] == '\0') {
      linCmtCmt(lin, 2);
      // k23->k12
      lin->k12 = *index;
      return 1;
    }
    if (in[2] == '4' && in[3] == '\0') {
      linCmtCmt(lin, 2);
      // k24->k13
      lin->k13 = *index;
      return 1;
    }
  }
  return 0;
}

static inline int isLinCmtK31orK32(linCmtStruct *lin, const char *in, int *index){
  if (in[1] == '3')  {
    if (in[2] == '1' && in[3] == '\0') {
      linCmtCmt(lin, 1);
      lin->k31 = *index;
      return 1;
    }
    if (in[2] == '2' && in[3] == '\0') {
      linCmtCmt(lin, 2);
      // k32->k21
      lin->k21 = *index;
      return 1;
    }
  }
  return 0;
}

static inline int isLinCmtK42(linCmtStruct *lin, const char *in, int *index) {
  if (in[1] == '4' && in[2] == '2' && in[3] == '\0') {
    linCmtCmt(lin, 2);
    // k42 -> k31
    lin->k31 = *index;
    return 1;
  }
  return 0;
}

static inline void linCmtK(linCmtStruct *lin, const char *in, int *index) {
  int tmp = isLinCmtK(lin, in, index) ||
    isLinCmtKeOrKel(lin, in, index) ||
    isLinCmtKa(lin, in, index) ||
    isLinCmtK10orK12orK13(lin, in, index) ||
    isLinCmtK20orK21orK23orK24(lin, in, index) ||
    isLinCmtK31orK32(lin, in, index) ||
    isLinCmtK42(lin, in, index);
  (void)tmp;
}

#define linCmtCl1style 1
#define linCmtCld1style 2
#define linCmtQstyle 3

static inline void linCmtClStr(const int style){
  switch(style){
  case linCmtCl1style:
    snprintf(errLin + errOff, errLinLen - errOff, "Cl#");
    errOff+=3;
    break;
  case linCmtCld1style:
    snprintf(errLin + errOff, errLinLen - errOff, "Cld#");
    errOff+=4;
    break;
  case linCmtQstyle:
    snprintf(errLin + errOff, errLinLen - errOff, "Q");
    errOff++;
    break;
  }
}

static inline void linCmtClStyle(linCmtStruct *lin, const int style) {
  if (lin->clStyle == -1) {
    lin->clStyle = style;
  }
  if (lin->clStyle != style) {
    errLin[0] = '\0';
    errOff=0;
    snprintf(errLin + errOff, errLinLen - errOff, "cannot mix '");
    errOff+=12;
    linCmtClStr(lin->clStyle);
    snprintf(errLin + errOff, errLinLen - errOff, "' and '");
    errOff+=7;
    linCmtClStr(style);
    snprintf(errLin + errOff, errLinLen - errOff, "' clearance styles");
    errOff+=18;
    _rxode2parse_unprotect();
    err_trans(errLin);
  }
}

static inline void linCmtQ(linCmtStruct *lin, const char *in, int *index) {
  if (in[1] == '\0') {
    // Q
    linCmtClStyle(lin, linCmtQstyle);
    lin->cl1 = *index;
    return;
  }
  if (in[1] == '1' && in[2] == '\0') {
    // Q1
    linCmtClStyle(lin, linCmtQstyle);
    lin->cl2 = *index;
    return;
  }
  if (in[1] == '2' && in[2] == '\0') {
    // Q2
    linCmtClStyle(lin, linCmtQstyle);
    lin->cl3 = *index;
    return;
  }
  if (in[1] == '3' && in[2] == '\0') {
    // Q3
    linCmtClStyle(lin, linCmtQstyle);
    lin->cl4 = *index;
    return;
  }
}

static inline int isLinCmtC(linCmtStruct *lin, const char *in, int *index) {
  if (in[1] == '\0'){
    // C
    lin->c = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtCl0(linCmtStruct *lin, const char *in, int *index) {
  if (in[2] == '\0') {
    lin->cl = *index;
    return 1;
  }
  return 0;
}
static inline int isLinCmtCl1(linCmtStruct *lin, const char *in, int *index) {
  if (in[2] == '1' && in[3] == '\0') {
    linCmtClStyle(lin, linCmtCl1style);
    lin->cl1 = *index;
    return 1;
  }
  return 0;
}
static inline int isLinCmtCl2(linCmtStruct *lin, const char *in, int *index) {
  if (in[2] == '2' && in[3] == '\0') {
    linCmtClStyle(lin, linCmtCl1style);
    lin->cl2 = *index;
    return 1;
  }
  return 0;
}
static inline int isLinCmtCl3(linCmtStruct *lin, const char *in, int *index) {
  if (in[2] == '3' && in[3] == '\0') {
    linCmtClStyle(lin, linCmtCl1style);
    lin->cl3 = *index;
    return 1;
  }
  return 0;
}
static inline int isLinCmtCl4(linCmtStruct *lin, const char *in, int *index) {
  if (in[2] == '4' && in[3] == '\0') {
    linCmtClStyle(lin, linCmtCl1style);
    lin->cl4 = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtCld0(linCmtStruct *lin, const char *in, int *index){
  if (in[3] == '\0') {
    linCmtClStyle(lin, linCmtCld1style);
    lin->cl1 = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtCld1(linCmtStruct *lin, const char *in, int *index){
  if (in[3] == '1' && in[4] == '\0') {
    linCmtClStyle(lin, linCmtCld1style);
    lin->cl2 = *index;
    return 1;
  }
  return 0;
}
static inline int isLinCmtCld2(linCmtStruct *lin, const char *in, int *index){
  if (in[3] == '2' && in[4] == '\0') {
    linCmtClStyle(lin, linCmtCld1style);
    lin->cl3 = *index;
    return 1;
  }
  return 0;
}
static inline int isLinCmtCld3(linCmtStruct *lin, const char *in, int *index){
  if (in[3] == '3' && in[4] == '\0') {
    linCmtClStyle(lin, linCmtCld1style);
    lin->cl4 = *index;
    return 1;
  }
  return 0;
}
static inline int isLinCmtCl(linCmtStruct *lin, const char *in, int *index) {
  if (in[1] == 'l' || in[1] == 'L') {
    if  (isLinCmtCl0(lin, in, index) ||
         isLinCmtCl1(lin, in, index) ||
         isLinCmtCl2(lin, in, index) ||
         isLinCmtCl3(lin, in, index) ||
         isLinCmtCl4(lin, in, index)){
      return 1;
    } else if (in[2] == 'd' || in[2] == 'D') {
      return isLinCmtCld0(lin, in, index) ||
        isLinCmtCld1(lin, in, index) ||
        isLinCmtCld2(lin, in, index) ||
        isLinCmtCld3(lin, in, index);
    }
  }
  return 0;
}

static inline void linCmtC(linCmtStruct *lin, const char *in, int *index) {
  // CL, CL1, CL2, CL3, CL4
  // CLD, CLD1, CLD2, CLD3, CL4
  int tmp = isLinCmtC(lin, in, index) ||
    isLinCmtCl(lin, in, index);
  (void) tmp;
}
