static inline void handleIndLinMat0(nodeInfo ni, char *name) {
  if (nodeHas(mat0)){
    aType(TMAT0);
    sb.o =0; sbDt.o =0;
    sAppend(&sb, "_mat[%d] = ", tb.matn++);
  }
}

static inline void handleIndLinMatf(nodeInfo ni, char *name) {
  if (nodeHas(matF)){
    aType(TMATF);
    sb.o =0; sbDt.o =0;
    sAppend(&sb, "_matf[%d] = _IR[%d] + ", tb.matnf, tb.matnf);
    tb.matnf++;
  }
}

static inline int finalizeLineMat(nodeInfo ni, char *name) {
  if ((nodeHas(mat0) || nodeHas(matF))){
    addLine(&sbPm,     "%s;\n", sb.s);
    addLine(&sbPmDt,   "%s;\n", sbDt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}
