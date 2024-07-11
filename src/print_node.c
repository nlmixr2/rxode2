#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "print_node.h"
#include "tran.h"

extern SEXP _goodFuns;

void wprint_node(int depth, char *name, char *value, void *client_data) {
  int i;
  nodeInfo ni;
  niReset(&ni);
  int tmp = nodeTime(value) ||
    nodeCmt(value) ||
    nodeAmt(value) ||
    nodeTlast(value) ||
    nodePtr(value) ||
    nodeNaN(value) ||
    nodeNA(value) ||
    nodeInf(value);
  if (!tmp && nodeHas(identifier)) {
    tmp = nodeFunGamma(value) ||
      nodeFunLfactorial(value) ||
      nodeFunLog(value) ||
      nodeFunAbs(value) ||
      nodeFunLinCmt(value) ||
      nodeFunLinCmtA(value) ||
      nodeFunLinCmtB(value) ||
      nodeFunLinCmtC(value);
  }
  if (!tmp) {
    // See if this variable is a function too
    if (!nodeHas(identifier)) {
      for (int j = Rf_length(_goodFuns); j--;){
        if (!strcmp(CHAR(STRING_ELT(_goodFuns, j)),value)) {
          aAppendN("_rxNotFun_", 10);
          break;
        }
      }
    }
    // Apply fix for dot.syntax
    for (i = 0; i < (int)strlen(value); i++){
      if (value[i] == '.' && nodeHas(identifier_r)){
        aAppendN("_DoT_", 5);
        sAppendN(&sbt, ".", 1);
      } else {
        sPut(&sb, value[i]);
        sPut(&sbDt, value[i]);
        sPut(&sbt, value[i]);
      }
    }
  }
}
