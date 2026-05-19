#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#ifndef __RXODE2_DF_H__
#define __RXODE2_DF_H__


SEXP rxode2_df(int doDose0, int doTBS, std::vector<int>& lvlI, bool isIdentity);


#endif // __RXODE2_DF_H__
