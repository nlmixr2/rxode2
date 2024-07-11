#include "needSortDefines.h"

extern sbuf sbExtra;
extern D_Parser *curP;


static inline void handleFunctionLinCmtAlag(transFunctions *tf, D_ParseNode *xpn1, D_ParseNode *xpn2) {
  // 10 tlag
  xpn2 = d_get_child(xpn1, 10+tf->isLinB);
  int ii = 1;
  while (xpn2->start_loc.s[ii] == ' ') {
    ii++;
  }
  char* v2 = (char*)rc_dup_str(xpn2->start_loc.s+ii, xpn2->end);
  if (!((!strcmp(v2, "0") || !strcmp(v2, "0.0") ||
	 !strcmp(v2, "0.") || !strcmp(v2, "")))) {
    // has interesting tlag
    if (foundLag == 0) needSort+=needSortAlag; // & 2 when alag
    foundLag=1;
    sAppend(&sbExtra,"rxlin___=rxAlagLin(%s);\n", v2);
  }
}

static inline void handleFunctionRxLinGeneric(transFunctions *tf, int propId, int is1) {
  sb.o=0;sbDt.o=0; sbt.o=0;
  aType(propId);
  switch (propId) {
  case ALAG:
    sAppendN(&sb, "_alag[", 6);
    sAppendN(&sbDt, "_alag[", 6);
    break;
  case FBIO:
    sAppendN(&sb, "_f[", 3);
    sAppendN(&sbDt, "_f[", 3);
    break;
  case RATE:
    sAppendN(&sb, "_rate[", 6);
    sAppendN(&sbDt, "_rate[", 6);
    break;
  case DUR:
    sAppendN(&sb, "_dur[", 5);
    sAppendN(&sbDt, "_dur[", 5);
    break;
  }
  sAppendN(&sb, "(&_solveData->subjects[_cSub])->linCmt", 38);
  sAppendN(&sbDt, "(&_solveData->subjects[_cSub])->linCmt", 38);
  if (is1) {
    sAppendN(&sb, "+1", 2);
    sAppendN(&sbDt, "+1", 2);
  }
  sAppendN(&sb, "] = " , 4);
  sAppendN(&sbDt, "] = ", 4);
  /* tf->i[0] = 1;// Parse next arguments */
  /* tf->depth[0]=1; */
}

static inline void handleFunctionRxAlagLin(transFunctions *tf) {
  handleFunctionRxLinGeneric(tf,ALAG, 0);
}

static inline void handleFunctionLinCmtF1(transFunctions *tf, D_ParseNode *xpn1, D_ParseNode *xpn2) {
  // 11 f1
  xpn2 = d_get_child(xpn1, 11+tf->isLinB);
  int ii = 1;
  while (xpn2->start_loc.s[ii] == ' ') {
    ii++;
  }
  char* v2 = (char*)rc_dup_str(xpn2->start_loc.s+ii, xpn2->end);
  if (!((!strcmp(v2, "1") || !strcmp(v2, "1.0") ||
	 !strcmp(v2, "1.") || !strcmp(v2, "")))) {
    // has interesting f1
    if (foundF == 0) needSort+=needSortF;// & 1 when F
    foundF=1;
    sAppend(&sbExtra,"rxlin___=rxFLin(%s);\n", v2);
  }
}

static inline void handleFunctionRxFLin(transFunctions *tf) {
  handleFunctionRxLinGeneric(tf, FBIO, 0);
}

static inline void handleFunctionLinCmtDur1(transFunctions *tf, D_ParseNode *xpn1, D_ParseNode *xpn2) {
  // 13 dur1
  xpn2 = d_get_child(xpn1, 13+tf->isLinB);
  int ii = 1;
  while (xpn2->start_loc.s[ii] == ' ') {
    ii++;
  }
  char* v2 = (char*)rc_dup_str(xpn2->start_loc.s + ii, xpn2->end);
  if (!((!strcmp(v2, "0") || !strcmp(v2, "0.0") ||
	 !strcmp(v2, "0.")) || !strcmp(v2, ""))) {
    // has interesting dur
    if (foundDur == 0) needSort+= needSortDur;// & 4 when dur
    foundDur=1;
    sAppend(&sbExtra,"rxlin___=rxDurLin(%s);\n", v2);
  }
}

static inline void handleFunctionRxDurLin(transFunctions *tf) {
  handleFunctionRxLinGeneric(tf, DUR, 0);
}

static inline void handleFunctionLinCmtRate1(transFunctions *tf, D_ParseNode *xpn1, D_ParseNode *xpn2) {
  // 12 rate1
  xpn2 = d_get_child(xpn1, 12+tf->isLinB);
  int ii = 1;
  while (xpn2->start_loc.s[ii] == ' ') {
    ii++;
  }
  char* v2 = (char*)rc_dup_str(xpn2->start_loc.s+ii, xpn2->end);
  if (!((!strcmp(v2, "0") || !strcmp(v2, "0.0") ||
	 !strcmp(v2, "0.") || !strcmp(v2, "")))) {
    // has interesting rate
    if (foundRate == 0) needSort+= needSortRate;// & 8 when rate
    foundRate=1;
    sAppend(&sbExtra,"rxlin___=rxRateLin(%s);\n", v2);
  }
}

static inline void handleFunctionRxRateLin(transFunctions *tf) {
  handleFunctionRxLinGeneric(tf, RATE, 0);
}


static inline void handleFunctionLinCmtKa(transFunctions *tf, D_ParseNode *xpn1, D_ParseNode *xpn2) {
  // 14 -- ka
  xpn2 = d_get_child(xpn1, 14+tf->isLinB);
  int ii = 1;
  while (xpn2->start_loc.s[ii] == ' ') {
    ii++;
  }
  char* v2 = (char*)rc_dup_str(xpn2->start_loc.s+ii, xpn2->end);
  if (!((!strcmp(v2, "0") || !strcmp(v2, "0.0") ||
	 !strcmp(v2, "0.")))) {
    tb.hasKa=1;
  }
  /* Free(v2); */
  // lag2 = 15
  xpn2 = d_get_child(xpn1, 15+tf->isLinB);
  ii = 1;
  while (xpn2->start_loc.s[ii] == ' ') {
    ii++;
  }
  v2 = (char*)rc_dup_str(xpn2->start_loc.s+ii, xpn2->end);
  if (!((!strcmp(v2, "0") || !strcmp(v2, "0.0") ||
	 !strcmp(v2, "0.") || !strcmp(v2, "")))) {
    // has interesting tlag
    if (foundLag == 0) needSort+= needSortAlag; // & 2 when alag
    foundLag=1;
    sAppend(&sbExtra,"rxlin___=rxAlag1Lin(%s);\n", v2);
  }
}

static inline void handleFunctionRxAlag1Lin(transFunctions *tf) {
  handleFunctionRxLinGeneric(tf, ALAG, 1);
}


static inline void handleFunctionLinCmtF2(transFunctions *tf, D_ParseNode *xpn1, D_ParseNode *xpn2) {
  // f2 = 16 ; This is 1 instead of zero
  xpn2 = d_get_child(xpn1, 16+tf->isLinB);
  int ii = 1;
  while (xpn2->start_loc.s[ii] == ' ') {
    ii++;
  }
  char* v2 = (char*)rc_dup_str(xpn2->start_loc.s+ii, xpn2->end);
  if (!((!strcmp(v2, "1") || !strcmp(v2, "1.0") ||
	 !strcmp(v2, "1.") || !strcmp(v2, "")))) {
    // has interesting f1
    if (foundF == 0) needSort+= needSortF;// & 1 when F
    foundF=1;
    sAppend(&sbExtra,"rxlin___=rxF1Lin(%s);\n", v2);
  }
}
static inline void handleFunctionRxF1Lin(transFunctions *tf) {
  handleFunctionRxLinGeneric(tf, FBIO, 1);
}

static inline void handleFunctionLinCmtRate2(transFunctions *tf, D_ParseNode *xpn1, D_ParseNode *xpn2) {
  // rate2 = 17
  xpn2 = d_get_child(xpn1, 17+tf->isLinB);
  int ii = 1;
  while (xpn2->start_loc.s[ii] == ' ') {
    ii++;
  }
  char* v2 = (char*)rc_dup_str(xpn2->start_loc.s+ii, xpn2->end);
  if (!((!strcmp(v2, "0") || !strcmp(v2, "0.0") ||
	 !strcmp(v2, "0.") || !strcmp(v2, "")))) {
    // has interesting rate
    if (foundRate == 0) needSort+= needSortRate;// & 8 when rate
    foundRate=1;
    sAppend(&sbExtra,"rxlin___=rxRate1Lin(%s);\n", v2);
  }
}

static inline void handleFunctionRxRate1Lin(transFunctions *tf) {
  handleFunctionRxLinGeneric(tf, RATE, 1);
}

static inline void handleFunctionLinCmtDur2(transFunctions *tf, D_ParseNode *xpn1, D_ParseNode *xpn2) {
  xpn2 = d_get_child(xpn1, 18+tf->isLinB);
  int ii = 1;
  while (xpn2->start_loc.s[ii] == ' ') {
    ii++;
  }
  char* v2 = (char*)rc_dup_str(xpn2->start_loc.s+ii, xpn2->end);
  if (!((!strcmp(v2, "0") || !strcmp(v2, "0.0") ||
	 !strcmp(v2, "0.") || !strcmp(v2, "")))) {
    // has interesting duration
    if (foundDur == 0) needSort+= needSortDur;// & 4 when dur
    foundDur=1;
    sAppend(&sbExtra,"rxlin___=rxDur1Lin(%s);\n", v2);
  }
}

static inline void handleFunctionRxDur1Lin(transFunctions *tf) {
  handleFunctionRxLinGeneric(tf, DUR, 1);
}

static inline int handleFunctionLinCmtJitProp(transFunctions *tf) {
  if (!strcmp("rxAlagLin", tf->v)) {
    handleFunctionRxAlagLin(tf);
    return 1;
  }
  if (!strcmp("rxAlag1Lin", tf->v)) {
    handleFunctionRxAlag1Lin(tf);
    return 1;
  }
  if (!strcmp("rxFLin", tf->v)) {
    handleFunctionRxFLin(tf);
    return 1;
  }
  if (!strcmp("rxF1Lin", tf->v)) {
    handleFunctionRxF1Lin(tf);
    return 1;
  }
  if (!strcmp("rxRateLin", tf->v)) {
    handleFunctionRxRateLin(tf);
    return 1;
  }
  if (!strcmp("rxRate1Lin", tf->v)) {
    handleFunctionRxRate1Lin(tf);
    return 1;
  }
  if (!strcmp("rxDurLin", tf->v)) {
    handleFunctionRxDurLin(tf);
    return 1;
  }
  if (!strcmp("rxDur1Lin", tf->v)) {
    handleFunctionRxDur1Lin(tf);
    return 1;
  }
  return 0;
}

static inline int handleFunctionLinCmt(transFunctions *tf) {
  if (handleFunctionLinCmtJitProp(tf)) return 1;
  if (!strcmp("linCmtA", tf->v) || !strcmp("linCmtC", tf->v) ||
      (tf->isLinB=!strcmp("linCmtB", tf->v))) {
    D_ParseNode *xpn1 = d_get_child(tf->pn, 3);
    D_ParseNode *xpn2 = d_get_child(xpn1, 1);
    char *v2 = (char*)rc_dup_str(xpn2->start_loc.s, xpn2->end);
    tb.linCmtN = toInt(v2+1);
    xpn2 = d_get_child(xpn1, 2);
    v2 = (char*)rc_dup_str(xpn2->start_loc.s, xpn2->end);
    tb.ncmt = toInt(v2+1);
    if (tf->isLinB) tf->isLinB=1;
    tb.linB = tf->isLinB;
    if (!tb.linExtra) {
      handleFunctionLinCmtAlag(tf, xpn1, xpn2);
      handleFunctionLinCmtF1(tf, xpn1, xpn2);
      handleFunctionLinCmtDur1(tf, xpn1, xpn2);
      handleFunctionLinCmtRate1(tf, xpn1, xpn2);
      handleFunctionLinCmtKa(tf, xpn1, xpn2);
      handleFunctionLinCmtF2(tf, xpn1, xpn2);
      handleFunctionLinCmtRate2(tf, xpn1, xpn2);
      handleFunctionLinCmtDur2(tf, xpn1, xpn2);
      tb.linExtra=true; // Only first call
    }
    aType(TLIN);
    if (tb.linB){
      xpn2 = d_get_child(xpn1, 4);
      int ii = 1;
      while (xpn2->start_loc.s[ii] == ' ') {
        ii++;
      }
      v2 = (char*)rc_dup_str(xpn2->start_loc.s+ii, xpn2->end);
      int tmp = toInt(v2);
      if (tmp > 0) {
	tmp--;
	tmp = 1 << tmp;
	if ((tb.linCmtFlg & tmp) == 0){
	  tb.linCmtFlg += tmp;
	}
      }
    }
    return 1;
  }
  return 0;
}
