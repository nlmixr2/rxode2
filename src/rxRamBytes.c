#define STRICT_R_HEADERS
#define USE_FC_LEN_T
#include <R.h>
#include <Rinternals.h>
#include "rxProtect.h"
#include "rxMemAvail.h" /* rxAvailableMemoryBytes() */

#ifdef _WIN32
#include <windows.h>
#elif defined(__APPLE__)
#include <sys/types.h>
#include <sys/sysctl.h>
#else
#include <unistd.h>
#endif

// Total physical RAM in bytes, or NA_REAL when the OS query fails.
double rxRamBytes(void) {
#ifdef _WIN32
  MEMORYSTATUSEX st;
  st.dwLength = sizeof(st);
  if (GlobalMemoryStatusEx(&st)) {
    return (double) st.ullTotalPhys;
  }
#elif defined(__APPLE__)
  uint64_t mem = 0;
  size_t len = sizeof(mem);
  if (sysctlbyname("hw.memsize", &mem, &len, NULL, 0) == 0) {
    return (double) mem;
  }
#else
  long pages = sysconf(_SC_PHYS_PAGES);
  long pageSize = sysconf(_SC_PAGE_SIZE);
  if (pages > 0 && pageSize > 0) {
    return (double) pages * (double) pageSize;
  }
#endif
  return NA_REAL;
}

// Currently available memory in bytes (rxAvailableMemoryBytes() from
// rxMemAvail.h, the same estimate the C allocator preflight uses), or
// NA_REAL when the OS query fails.
double rxFreeRamBytes(void) {
  uint64_t avail = rxAvailableMemoryBytes();
  if (avail == UINT64_MAX) return NA_REAL;
  return (double) avail;
}

SEXP _rxode2_rxRamBytes_(void) {
  rxProtectGuard;
  SEXP ret = rxP(Rf_allocVector(REALSXP, 2));
  REAL(ret)[0] = rxRamBytes();
  REAL(ret)[1] = rxFreeRamBytes();
  SEXP names = rxP(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("total"));
  SET_STRING_ELT(names, 1, Rf_mkChar("free"));
  Rf_setAttrib(ret, R_NamesSymbol, names);
  rxUPAll();
  return ret;
}
