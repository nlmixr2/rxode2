#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "print_node.h"
#include "tran.h"

extern SEXP _goodFuns;

// Terminals translated to a different C spelling
static inline int wprintSpecialValue(char *value) {
  return nodeTime(value) ||
    nodeCmt(value) ||
    nodeAmt(value) ||
    nodeTlast(value) ||
    nodePtr(value) ||
    nodeNaN(value) ||
    nodeNA(value) ||
    nodeInf(value) ||
    nodeMixnum(value) ||
    nodeMixest(value) ||
    nodeMixunif(value) ||
    nodeMixSel(value);
}

// Function names translated to a different C function
static inline int wprintSpecialFun(char *value) {
  return nodeFunGamma(value) ||
    nodeFunLfactorial(value) ||
    nodeFunLog(value) ||
    nodeFunAbs(value) ||
    nodeFunLinCmt(value) ||
    nodeFunLinCmtA(value) ||
    nodeFunLinCmtB(value);
}

// A variable sharing a function's name gets a prefix so C sees a variable
static inline void wprintNotFunPrefix(char *value) {
  for (int j = Rf_length(_goodFuns); j--;){
    if (!strcmp(CHAR(STRING_ELT(_goodFuns, j)),value)) {
      aAppendN("_rxNotFun_", 10);
      return;
    }
  }
}

// Keep 'a - -b' / 'a + +b' from becoming the C '--' / '++' operators
static inline void wprintSignSpace(char *value) {
  if (value[0] != '-' && value[0] != '+') return;
  if (sb.o > 0 && sb.s[sb.o-1] == value[0]) sPut(&sb, ' ');
  if (sbDt.o > 0 && sbDt.s[sbDt.o-1] == value[0]) sPut(&sbDt, ' ');
}

// Copy the terminal, translating '.' in identifiers to '_DoT_'
static inline void wprintValue(char *value, int isId) {
  for (int i = 0; i < (int)strlen(value); i++){
    if (value[i] == '.' && isId){
      aAppendN("_DoT_", 5);
      sAppendN(&sbt, ".", 1);
    } else {
      sPut(&sb, value[i]);
      sPut(&sbDt, value[i]);
      sPut(&sbt, value[i]);
    }
  }
}

void wprint_node(int depth, char *name, char *value, void *client_data) {
  (void)depth; (void)client_data;
  nodeInfo ni;
  niReset(&ni);
  if (wprintSpecialValue(value)) return;
  if (nodeHas(identifier)) {
    if (wprintSpecialFun(value)) return;
  } else {
    wprintNotFunPrefix(value);
  }
  wprintSignSpace(value);
  wprintValue(value, nodeHas(identifier_r));
}
