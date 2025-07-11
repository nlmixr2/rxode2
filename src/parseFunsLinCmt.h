#include "../inst/include/needSortDefines.h"
#include "linCmtDiffConstant.h"

extern sbuf sbExtra;
extern D_Parser *curP;

static inline void addLinCmtBdiff(int var) {
  if ((tb.ndiff & var) == 0) {
    tb.ndiff |= var;
  }
}

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

    xpn2 = d_get_child(xpn1, 3);
    v2 = (char*)rc_dup_str(xpn2->start_loc.s, xpn2->end);
    tb.hasKa = toInt(v2+1);

    if (tf->isLinB) tf->isLinB=1;
    tb.linB = tf->isLinB;
    aType(TLIN);
    if (tf->isLinB) {
      // Here we figure out what derivatives are requested.
      // This allows the linCmtB Jacobian to focus only on
      // the derivatives that are needed.
      //
      // This should speed up the slower linCmtB function
      xpn2 = d_get_child(xpn1, 4);
      v2 = (char*)rc_dup_str(xpn2->start_loc.s, xpn2->end);
      int which1 = toInt(v2+1);

      xpn2 = d_get_child(xpn1, 5);
      v2 = (char*)rc_dup_str(xpn2->start_loc.s, xpn2->end);
      int which2 = toInt(v2+1);

      if (which2 == -2) {
        // amounts in function are returned

        // which1 is the compartment number, but it includes the
        // derivatives of each compartment; this means if a derivative
        // is requested, we need to capture what parameter is calculated
        // to save time when requesting AD.
        int sw = tb.ncmt*10 + tb.hasKa + which1*100;
        switch (sw) {
        case 1530: // 3 compartment, iv, which1=15
        case 930: // 3 compartment, iv, which1=9
        case 330: // 3 compartment, iv, which1=3
        case 620: // 2 compartment, iv, which1=6
        case 220: // 2 compartment, iv, which1=2
        case 110: // 1 compartment, iv, which1=1
        case 1831: // 3 compartment oral, which1=18
        case 1131: // 3 compartment oral, which1=11
        case 431: // 3 compartment oral, which1=4
        case 821: // 2 compartment oral which1=8
        case 321: // 2 compartment oral which1=3
        case 211: // 1 compartment oral which1=2
          addLinCmtBdiff(diffP1);
          break;
        case 1630: // 3 compartment, iv, which1=16
        case 1030: // 3 compartment, iv, which1=10
        case 430: // 3 compartment, iv, which1=4
        case 720: // 2 compartment, iv, which1=7
        case 320: // 2 compartment, iv, which1=3
        case 210: // 2 compartment, iv, which1=2
        case 1931: // 3 compartment oral, which1=19
        case 1231: // 3 compartment oral, which1=12
        case 531:// 3 compartment oral, which1=5
        case 921: // 2 compartment oral which1=9
        case 421: // 2 compartment oral which1=4
        case 311: // 1 compartmental oral, which1=3
          addLinCmtBdiff(diffV1);
          break;
        case 1730: // 3 compartment, iv, which1=17
        case 1130: // 3 compartment, iv, which1=11
        case 530: // 3 compartment, iv, which1=5
        case 820: // 2 compartment, iv, which1=7
        case 420: // 2 compartment, iv, which1=4
        case 2031: // 3 compartment oral, which1=20
        case 1331: // 3 compartment oral, which1=13
        case 631: // 3 compartment oral, which1=6
        case 1021: // 2 compartment oral, which1=10
        case 521: // 2 compartment oral, which1=5
          addLinCmtBdiff(diffP2);
          break;
        case 1830: // 3 compartment, iv, which1=18
        case 1230:// 3 compartment, iv, which1=12
        case 630: // 3 compartment, iv, which1=6
        case 920: // 2 compartment, iv, which1=9
        case 520: // 2 compartment, iv, which1=5
        case 2131: // 3 compartment oral, which1=21
        case 1431: // 3 compartment oral, which1=14
        case 731: // 3 compartment oral, which1=7
        case 1121: // 2 compartment oral, which1=11
        case 621: // 2 compartment oral, which1=6
          addLinCmtBdiff(diffP3);
          break;
        case 1930: // 3 compartment, iv, which1=19
        case 1330: // 3 compartment, iv, which1=13
        case 730: // 3 compartment, iv, which1=7
        case 2231: // 3 compartment oral, which1=22
        case 1531: // 3 compartment oral, which1=15
        case 831: // 3 compartment oral, which1=8
          addLinCmtBdiff(diffP4);
          break;
        case 2030: // 3 compartment, iv, which1=20
        case 1430: // 3 compartment, iv, which1=14
        case 830: // 3 compartment, iv, which1=8
        case 2331: // 3 compartment oral, which1=23
        case 1631: // 3 compartment oral, which1=16
        case 931: // 3 compartment oral, which1=9
          addLinCmtBdiff(diffP5);
          break;
        case 2531:// 3 compartment oral, which1=25
        case 2431:// 3 compartment oral, which1=24
        case 1731:// 3 compartment oral, which1=17
        case 1031:// 3 compartment oral, which1=10
        case 1321:// 2 compartment oral, which1=13
        case 1221:// 2 compartment oral, which1=12
        case 721: // 2 compartment oral, which1=7
        case 511: // 1 compartmental oral, which1=5
        case 411: // 1 compartmental oral, which1=4
          addLinCmtBdiff(diffKa);
          break;
        default:
          REprintf("Unknown linCmtB which2=-2: %d\n", sw);
          return 0;

        }
      } else if (which1 == -2) {
        // This means that we are only calculating the Jacobian
        // and not the function value.
        //
        int sw = tb.ncmt*10 + tb.hasKa + which2*100;
        switch (sw) {
        case 30: // 3 compartment, iv, which2=0
        case 31: // 3 compartment, oral, which2=0
        case 20: // 2 compartment, iv, which2=0
        case 21: // 2 compartment, oral, which2=0
        case 10: // 1 compartment, iv, which2=0
        case 11: // 1 compartment, oral, which2=0
          addLinCmtBdiff(diffP1);
          break;
        case 130: // 3 compartment, iv, which2=1
        case 131: // 3 compartment, oral, which2=1
        case 120: // 3 compartment, iv, which2=1
        case 121: // 2 compartment, oral, which2=1
        case 110: // 1 compartment, iv, which2=1
        case 111: // 1 compartment, oral, which2=1
          addLinCmtBdiff(diffV1);
          break;
        case 230: // 3 compartment, iv, which2=2
        case 231: // 3 compartment, oral, which2=2
        case 220: // 2 compartment, iv, which2=2
        case 221: // 2 compartment, oral, which2=2
          addLinCmtBdiff(diffP2);
          break;
        case 330: // 3 compartment, iv, which2=3
        case 331: // 3 compartment, oral, which2=3
        case 320: // 2 compartment, iv, which2=3
        case 321: // 2 compartment, oral, which2=3
          addLinCmtBdiff(diffP3);
          break;
        case 430: // 3 compartment, iv, which2=4
        case 431: // 3 compartment, oral, which2=4
          addLinCmtBdiff(diffP4);
          break;
        case 530: // 3 compartment, iv, which2=5
        case 531: // 3 compartment, oral, which2=5
          addLinCmtBdiff(diffP5);
          break;
        case 211: // 1 compartment, oral, which2=2
        case 421: // 2 compartment, oral, which2=4
        case 621: // 2 compartment, oral, which2=6
          addLinCmtBdiff(diffKa);
          break;
        default:
          REprintf("Unknown linCmtB which1=-2: %d\n", sw);
          return 0;
        }
      }
    }
    return 1;
  }
  return 0;
}
