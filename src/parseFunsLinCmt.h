#include "needSortDefines.h"

extern sbuf sbExtra;
extern D_Parser *curP;

static inline int handleFunctionLinCmt(transFunctions *tf) {
  if (!strcmp("linCmtA", tf->v) ||
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
    aType(TLIN);
    if (tb.linB) {
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
