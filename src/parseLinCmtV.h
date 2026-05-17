#define linCmtVdStyle 1
#define linCmtVtStyle 2
#define linCmtVpStyle 3
#define linCmtVnStyle 4

static inline void linCmtVStr(const int style){
  switch(style){
  case linCmtVdStyle:
    snprintf(errLin + errOff, errLinLen-errOff, "Vd");
    errOff+=2;
    break;
  case linCmtVtStyle:
    snprintf(errLin + errOff, errLinLen-errOff, "Vt");
    errOff+=2;
    break;
  case linCmtVpStyle:
    snprintf(errLin + errOff, errLinLen-errOff, "Vp");
    errOff+=2;
    break;
  case linCmtVnStyle:
    snprintf(errLin + errOff, errLinLen-errOff, "V#");
    errOff+=2;
    break;
  }
}

static inline void linCmtVStyle(linCmtStruct *lin, int style) {
  if (lin->vStyle == -1) {
    lin->vStyle = style;
  }
  if (lin->vStyle != style) {
    errLin[0] = '\0';
    errOff = 0;
    snprintf(errLin + errOff, errLinLen-errOff, "cannot mix '");
    errOff += 12;
    linCmtVStr(lin->vStyle);
    snprintf(errLin + errOff, errLinLen-errOff, "' and '");
    errOff += 7;
    linCmtVStr(style);
    snprintf(errLin + errOff, errLinLen-errOff, "' volume styles");
    errOff += 15;
    _rxode2parse_unprotect();
    err_trans(errLin);
  }
}

static inline int isLinCmtVss(linCmtStruct *lin, const char *in, int *index) {
  if ((in[1] == 's' || in[1] == 'S') &&
      (in[2] == 's' || in[2] == 'S') &&
      in[3] == '\0') {
    lin->vss = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtV(linCmtStruct *lin, const char *in, int *index) {
  if (in[1] == '\0') {
    lin->v = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtV1(linCmtStruct *lin, const char *in, int *index){
  if (in[1] == '1' && in[2] == '\0') {
    linCmtVStyle(lin, 4);
    lin->v1 = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtV2(linCmtStruct *lin, const char *in, int *index){
  if (in[1] == '2' && in[2] == '\0') {
    linCmtVStyle(lin, 4);
    lin->v2 = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtV3(linCmtStruct *lin, const char *in, int *index){
  if (in[1] == '3' && in[2] == '\0') {
    linCmtVStyle(lin, 4);
    lin->v3 = *index;
    return 1;
  }
  return 0;
}
static inline int isLinCmtV4(linCmtStruct *lin, const char *in, int *index){
  if (in[1] == '4' && in[2] == '\0') {
    linCmtVStyle(lin, 4);
    lin->v4 = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtVc(linCmtStruct *lin, const char *in, int *index){
  if ((in[1] == 'c' || in[1] == 'C') &&
      in[2] == '\0') {
    lin->v = *index;
    return 1;
  }
  return 0;
}

static inline int lincmtGetVtype(linCmtStruct *lin, const char *in, int *index) {
  return (in[1] == 'd' || in[1] == 'D')*1 +
    (in[1] == 't' || in[1] == 'T')*2 +
    (in[1] == 'p' || in[1] == 'P')*3;
}

static inline int isLinCmtVp(linCmtStruct *lin, const char *in, int *index) {
  if (in[2] == '\0') {
    lin->vp = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtVp1(linCmtStruct *lin, const char *in, int *index) {
  if (in[2] == '1' && in[3] == '\0') {
    lin->vp1 = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtVp2(linCmtStruct *lin, const char *in, int *index) {
  if (in[2] == '2' && in[3] == '\0') {
    lin->vp2 = *index;
    return 1;
  }
  return 0;
}

static inline int isLinCmtVp3(linCmtStruct *lin, const char *in, int *index) {
  if (in[2] == '3' && in[3] == '\0') {
    lin->vp3 = *index;
    return 1;
  }
  return 0;
}

static inline void linCmtV(linCmtStruct *lin, const char *in, int *index) {
  if (!(isLinCmtVss(lin, in, index) ||
	isLinCmtV(lin, in, index) ||
	isLinCmtV1(lin, in, index) ||
	isLinCmtV2(lin, in, index) ||
	isLinCmtV3(lin, in, index) ||
	isLinCmtV4(lin, in, index) ||
	isLinCmtVc(lin, in, index))){
    int vType = lincmtGetVtype(lin, in, index);
    if (vType) {
      linCmtVStyle(lin, vType);
      // vp, vp1
      // vp, vp2
      // vp, vp3
      // vp1, vp2
      // vp2, vp3
      int tmp = isLinCmtVp(lin, in, index) ||
	isLinCmtVp1(lin, in, index) ||
	isLinCmtVp2(lin, in, index) ||
	isLinCmtVp3(lin, in, index);
      (void) tmp;
    }
  }
}
