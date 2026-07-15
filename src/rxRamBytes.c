#define STRICT_R_HEADERS
#define USE_FC_LEN_T
#include <R.h>
#include <Rinternals.h>
#include "rxProtect.h"

#ifdef _WIN32
#include <windows.h>
#elif defined(__APPLE__)
#include <sys/types.h>
#include <sys/sysctl.h>
#include <mach/mach.h>
#else
#include <stdio.h>
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

// Currently available physical RAM in bytes, or NA_REAL when the OS query
// fails.
double rxFreeRamBytes(void) {
#ifdef _WIN32
  MEMORYSTATUSEX st;
  st.dwLength = sizeof(st);
  if (GlobalMemoryStatusEx(&st)) {
    return (double) st.ullAvailPhys;
  }
#elif defined(__APPLE__)
  vm_size_t pageSize = 0;
  vm_statistics64_data_t vm;
  mach_msg_type_number_t count = HOST_VM_INFO64_COUNT;
  mach_port_t host = mach_host_self();
  if (host_page_size(host, &pageSize) == KERN_SUCCESS &&
      host_statistics64(host, HOST_VM_INFO64, (host_info64_t) &vm,
                        &count) == KERN_SUCCESS) {
    // free_count includes speculative pages
    return (double) vm.free_count * (double) pageSize;
  }
#else
  // Prefer MemAvailable (includes reclaimable cache) over MemFree
  FILE *f = fopen("/proc/meminfo", "r");
  if (f != NULL) {
    char line[256];
    double availKb = -1.0, freeKb = -1.0;
    unsigned long long kb;
    while (fgets(line, sizeof(line), f) != NULL) {
      if (sscanf(line, "MemAvailable: %llu kB", &kb) == 1) {
        availKb = (double) kb;
        break;
      }
      if (freeKb < 0 && sscanf(line, "MemFree: %llu kB", &kb) == 1) {
        freeKb = (double) kb;
      }
    }
    fclose(f);
    if (availKb >= 0) return availKb * 1024.0;
    if (freeKb >= 0) return freeKb * 1024.0;
  }
#ifdef _SC_AVPHYS_PAGES
  long pages = sysconf(_SC_AVPHYS_PAGES);
  long pageSize = sysconf(_SC_PAGE_SIZE);
  if (pages > 0 && pageSize > 0) {
    return (double) pages * (double) pageSize;
  }
#endif
#endif
  return NA_REAL;
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
