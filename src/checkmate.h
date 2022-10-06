#ifndef _checkmate_h_inrxode2__
#define _checkmate_h_inrxode2__
#if defined(__cplusplus)
extern "C" {
#endif
  
bool qtest(SEXP in, const char *test);
SEXP qassertS(SEXP in, const char *test, const char *what);
  
#if defined(__cplusplus)
}
#endif  
#endif
