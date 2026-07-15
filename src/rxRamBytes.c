#define STRICT_R_HEADERS
#define USE_FC_LEN_T
#include <R.h>
#include <Rinternals.h>

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

SEXP _rxode2_rxRamBytes_(void) {
  return Rf_ScalarReal(rxRamBytes());
}
