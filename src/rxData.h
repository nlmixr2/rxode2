#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#ifndef __RXDATA_H__
#define __RXDATA_H__

#if defined(__cplusplus)
extern "C" {
#endif
  double get_fkeep(int col, int id, rx_solving_options_ind *ind);
  int get_fkeepType(int col);
  SEXP get_fkeepLevels(int col);
  SEXP assign_fkeepAttr(int col, SEXP in);
  SEXP get_fkeepChar(int col, double val);
  double *getLlikSave(void);
  SEXP get_fkeepn(void);
  void cliAlert(const char *format, ...);
  void setZeroMatrix(int which);
  double * getAol(int n, double atol);
  double * getRol(int n, double rtol);
  void gFree(void);
  double *rxGetErrs(void);
  int rxGetErrsNcol(void);
  int rxGetErrsNrow(void);
  void rxSolveFreeC(void);
  void sortIds(rx_solve* rx, int ini);
  void setupRxInd(rx_solving_options_ind* ind, int first);
  SEXP rxGetModelLib(const char *s);
  void rxRmModelLib(const char* s);
  void rxAssignPtrC(SEXP obj);
  SEXP rxModelVarsC(char *ptr);
  SEXP rxStateNames(char *ptr);
  SEXP rxLhsNames(char *ptr);
  SEXP rxParamNames(char *ptr);
  int rxIsCurrentC(SEXP obj);
  int Rcat(char *msg);
  int isRstudio(void);
  int isProgSupported(void);

  void updateExtraDoseGlobals(rx_solving_options_ind* ind);

#if defined(__cplusplus)
}
#endif
#endif // __RXDATA_H__
