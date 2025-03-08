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
    /* REprintf("linCmtN: %d\n", tb.linCmtN); */
    xpn2 = d_get_child(xpn1, 2);
    v2 = (char*)rc_dup_str(xpn2->start_loc.s, xpn2->end);
    tb.ncmt = toInt(v2+1);
    if (tf->isLinB) tf->isLinB=1;
    tb.linB = tf->isLinB;
    aType(TLIN);
    return 1;
  }
  return 0;
}
