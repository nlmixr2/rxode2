#ifndef __TRAN_H__
#define __TRAN_H__
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
extern int needSort;
void updateSyntaxCol(void);
void trans_syntax_error_report_fn(char *err);
void parseFree(int last);
void RSprintf(const char *format, ...);

SEXP _rxode2parse_trans(SEXP parse_file, SEXP prefix, SEXP model_md5, SEXP parseStr,
                        SEXP isEscIn, SEXP inME, SEXP goodFuns, SEXP fullPrintIn);

typedef struct symtab {
  vLines ss; // Symbol string or symbol lines
  /* char ss[64*MXSYM]; */                     /* symbol string: all vars*/
  vLines de;             /* symbol string: all Des*/
  vLines str; /* symbol string: all assigned string variables*/
  vLines strVal; /* symbol string for rxode2 assigned strings */
  int *strValI; /* which variable is assigned a string */
  int *strValII; /* The number that the string is assigned (what C sees) */
  int lhi; // ith for lhs
  int *lho; /* lhs order */
  int *lh;        /*
lhs symbols?
=0 not LHS
=1 LHS
=9 if a state var;
=10 if suppressed lhs;
=11 suppress parameter printout;
=19 is LHS with stateExtra
=29
=70 LHS + param
*/
  int *interp;  // Interpolation flag:
  // 0, = default via any declaration including param()
  // 1 = linear via linear() or lin()
  // 2 = constant/locf  via constant() or locf()
  // 3 = nocb via nocb()
  // 4 = midpoint via midpoint()
  int interpC;// = 0; // current interpolation method
  int *ini;        /* initial variable assignment =2 if there are two assignments */
  int *mtime;
  double *iniv;        /* Initial values */
  int *ini0;        /* state initial variable assignment =2 if there are two assignments */
  int *di;        /* ith of state vars */
  int *didx;
  int *dprop;  /* property of state vars */
  int didxn; /* nth of declared states */
  int *si;      /* ith of string vars */
  int *sin;      /* n values in each string var */
  int *idi;       /* should ith state variable be ignored 0/1 */
  int *isi;      /* should ith string variable be ignored 0/1 */
  int *idu;       /* Has the ith state been used in a derivative expression? */
  int *lag;  // Lag number (if present)
  int *alag; // absorption lag compartments seen
  int alagn;
  int *dvid;
  int dvidn;
  int ix;                       /* ith of curr symbol */
  int id;                       /* ith of curr symbol */
  int fn;                     /* curr symbol a fn?*/
  int ixL;// New assignment index
  int didEq;
  int NEnd;
  int pos_de;
  int ini_i; // #ini
  int statei; // # states
  int nExtra;
  int sensi;
  int li; // # lhs
  int sli; // # suppressed lhs
  int pi; // # param
  int isPi; // # pi?
  int isNA; // # pi?
  int linCmt; // Unparsed linear compartment
  int linCmtCmt; // Are all non linear-system compartments derived from cmt()
  int linCmtN; // Unparsed linear compartment
  int linCmtFlg;
  // Save Jacobian information
  int *df;
  int *dy;
  int *sdfdy;
  int cdf;
  int ndfdy;
  int maxtheta;
  int hasCmt;
  int maxeta;
  int hasDepotCmt;
  int hasCentralCmt;
  int hasKa;
  int allocS;
  int allocSV;
  int allocD;
  int matn;
  int matnf;
  int ncmt;
  int linB;
  int ndiff; // flag of the linCmtB derivatives requested
  // curPropN
  int curPropN;
  int depotN;
  int centralN;
  // linCmt extras
  int nwhile;
  int nInd;
  int simflg;
  int thread;
  int lastDdt;
  int nLlik;
  int lvlStr;
  int dummyLhs;
} symtab;

extern symtab tb;

extern vLines depotLines;
extern vLines centralLines;
extern vLines sbPm, sbPmDt, sbNrmL;

#define FBIO 1
#define ALAG 2
#define RATE 3
#define DUR 4
#define TINI 5
#define TLOGIC 6
#define PODE0 7
#define PJAC 8
#define PJAC0 9
#define PODE 10
#define PPRN 11
#define TDDT 12
#define TJAC 13
#define TF0 14
#define PLHS 15
#define PFPRN 16
#define TASSIGN 17
#define TMTIME 18
#define TMAT0 19
#define TMATF 20
#define TLIN 21
#define TNONE 22

// new de type
#define fromDDT 2
#define fromCMTprop 1
#define fromCMT 3

#define NEEDSEMI _("lines need to end with ';'\n     to match R's handling of line endings set 'options(rxode2.syntax.require.semicolon = FALSE)'")
#define NOSTATE _("defined 'df(%s)/dy(%s)', but '%s' is not a state")
#define NOSTATEVAR _("defined 'df(%s)/dy(%s)', but '%s' is not a state or variable")
#define ODEFIRST _("ODEs compartment 'd/dt(%s)' must be defined before changing/accessing its properties (f/alag/rate/dur/tad/tafd)\nIf you want to change this set 'options(rxode2.syntax.require.ode.first = FALSE).\nBe warned this may number compartments based on first occurance of property or ODE")
#define ZERODVID _("'dvid()' cannot have zeros in it")
#define ONEDVID _("rxode2 only supports one dvid() statement per model")

typedef struct nodeInfo {
  int alag;
  int assignment;
  int assign_str;
  int levels_str;
  int levels_str1;
  int constant;
  int der_rhs;
  int derivative;
  int dfdy;
  int dfdy_rhs;
  int dur;
  int end_statement;
  int eta;
  int factorial;
  int factorial_exp;
  int fbio;
  int function;
  int function_name;
  int identifier;
  int identifier_r;
  int identifier_r_no_output;
  int ini0;
  int ini0f;
  int ini;
  int jac;
  int jac_rhs;
  int lfactorial;
  int lfactorial_exp;
  int max;
  int min;
  int mtime;
  int mult_part;
  int power_expression;
  /* int print_command; */
  int printf_statement;
  int prod;
  int rate;
  int selection_statement;
  int selection_statement__9;
  int break_statement;
  int sign;
  int sum;
  int theta0;
  int theta0_noout;
  int theta;
  int cmt_statement;
  int param_statement;
  int interp_statement;
  int dvid_statementI;
  int ifelse;
  int ifelse_statement;
  int mat0;
  int matF;
  int equality_str1;
  int equality_str2;
  int simfun_statement;
  int relational_op;
  int string;
} nodeInfo;

static inline void niReset(nodeInfo *ni){
  ni->mtime = -1;
  ni->alag = -1;
  ni->assignment = -1;
  ni->assign_str = -1;
  ni->levels_str = -1;
  ni->levels_str1 = -1;
  ni->constant = -1;
  ni->der_rhs = -1;
  ni->derivative = -1;
  ni->dfdy = -1;
  ni->dfdy_rhs = -1;
  ni->dur = -1;
  ni->end_statement = -1;
  ni->eta = -1;
  ni->factorial = -1;
  ni->factorial_exp = -1;
  ni->fbio = -1;
  ni->function = -1;
  ni->function_name=-1;
  ni->identifier = -1;
  ni->identifier_r = -1;
  ni->identifier_r_no_output = -1;
  ni->ini = -1;
  ni->ini0 = -1;
  ni->ini0f = -1;
  ni->jac = -1;
  ni->jac_rhs = -1;
  ni->lfactorial = -1;
  ni->lfactorial_exp = -1;
  ni->max = -1;
  ni->min = -1;
  ni->mult_part = -1;
  ni->power_expression = -1;
  /* ni->print_command = -1; */
  ni->printf_statement = -1;
  ni->prod = -1;
  ni->rate = -1;
  ni->rate = -1;
  ni->selection_statement = -1;
  ni->selection_statement__9 = -1;
  ni->break_statement = -1;
  ni->sign = -1;
  ni->sum = -1;
  ni->theta = -1;
  ni->theta0 = -1;
  ni->theta0_noout = -1;
  ni->cmt_statement = -1;
  ni->param_statement = -1;
  ni->interp_statement = -1;
  ni->dvid_statementI = -1;
  ni->ifelse = -1;
  ni->ifelse_statement=-1;
  ni->mat0 = -1;
  ni->matF = -1;
  ni->equality_str1 = -1;
  ni->equality_str2 = -1;
  ni->simfun_statement = -1;
  ni->relational_op = -1;
  ni->string = -1;
}

#define STRINGIFY(...) STRINGIFY_AUX(__VA_ARGS__)
#define STRINGIFY_AUX(...) #__VA_ARGS__

#define NIB(what) ni.what
#define nodeHas(what) (NIB(what) == -1 ? (NIB(what) = !strcmp(STRINGIFY(what), name)) : NIB(what))

extern sbuf sbOut;

#define notLHS 0
#define isLHS 1
#define isLHSstr 100
#define isState 9
#define isSuppressedLHS 10
#define isSuppressedLHSstr 110
#define isSuppressedParam 11
#define isLhsStateExtra 19
#define isLHSparam 70

extern sbuf _gbuf, _mv;

#define SBPTR sb.s+sb.o
#define NV tb.ss.n

extern char *gBuf;
extern int gBufFree;
extern int gBufLast;

extern int maxSumProdN, SumProdLD, foundF0, foundF, foundLag, foundRate, foundDur,
  good_jac, extraCmt, badMd5, maxUdf;
extern unsigned int found_jac, nmtime;

extern sbuf sbNrm;

#define max(a,b) (a)>(b) ? (a):(b)
#define min(a,b) (a)<(b) ? (a):(b)

void err_msg(int chk, const char *msg, int code);

extern const char *md5;
extern const char *model_prefix;
extern const char *me_code;

void reset(void);
char * rc_dup_str(const char *s, const char *e);

void trans_syntax_error_report_fn(char *err);
void trans_syntax_error_report_fn0(char *err);

extern sbuf _bufw, _bufw2;
extern sbuf sb, sbDt; /* buffer w/ current parsed & translated line */
extern sbuf sbt;


#define ENDLINE tb.ixL=-1; tb.didEq=0; tb.NEnd=NV;

#define aAppendN(str, len) sAppendN(&sb, str, len); sAppendN(&sbDt, str, len);
#define aProp(prop) curLineProp(&sbPm, prop); curLineProp(&sbPmDt, prop); curLineProp(&sbNrmL, prop);
#define aType(type) curLineType(&sbPm, type); curLineType(&sbPmDt, type); curLineType(&sbNrmL, type);

static inline int toInt(char *v2){
  errno = 0;
  char *v3 = v2;
  char *endptr = NULL;
  long lagNoL = strtol(v3, &endptr, 10);
  int lagNo;
  if (errno == 0 && v3 && !*endptr){
    lagNo = (int)(lagNoL);
  } else {
    lagNo = NA_INTEGER;
  }
  errno=0;
  return lagNo;
}

static inline int allSpaces(char *v2) {
  int iii=0;
  int allSpace=1;
  while(v2[iii] != '\0'){
    if (!isspace(v2[iii])){
      allSpace=0;
      break;
    }
  }
  return allSpace;
}

void parseFree(int last);

#define err_trans(chr) Rf_errorcall(R_NilValue, "%s", _(chr));

void _rxode2parse_unprotect(void);

char *getLine (char *src, int line, int *lloc);

#define prop0     1
#define propF     2
#define propAlag  4
#define propRate  8
#define propDur   16
// tad
#define propTad   32
#define propTad0  64
// tafd
#define propTafd  128
#define propTafd0 256
// tlast
#define propTlast 512
#define propTlast0 1024
// tfirst
#define propTfirst 2048
#define propTfirst0 4096
// podo and dose
#define propPodo 8192
#define propDose 16384

#define propPodo0 32768
#define propDose0 65536

#endif // __TRAN_H__
