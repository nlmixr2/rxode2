#ifndef RXODE2_MEM_AVAIL_H
#define RXODE2_MEM_AVAIL_H

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#include <stdint.h>

/* rxAvailableMemoryBytes()
 *
 * Returns an estimate of the bytes available for C-heap allocation,
 * or UINT64_MAX if the platform does not support this query.
 *
 * This is a heuristic snapshot subject to race conditions: another process
 * can allocate memory between this call and the subsequent calloc().  Use
 * only as a pre-flight check to catch obviously-too-large requests before
 * they crash the process.  See rxMemAvail.h comments for platform caveats.
 */

#if defined(_WIN32) || defined(WIN32)
/* -----------------------------------------------------------------------
 * Windows: GlobalMemoryStatusEx reports available virtual address space
 * (ullAvailVirtual) and available commit budget (ullAvailPhys +
 * ullAvailPageFile).  Take the minimum of both — the process cannot
 * allocate more than either limit allows.
 * ----------------------------------------------------------------------- */
#include <windows.h>
static inline uint64_t rxAvailableMemoryBytes(void) {
  MEMORYSTATUSEX ms;
  ms.dwLength = sizeof(MEMORYSTATUSEX);
  if (!GlobalMemoryStatusEx(&ms)) return UINT64_MAX;
  uint64_t avail_virt   = (uint64_t)ms.ullAvailVirtual;
  uint64_t avail_commit = (uint64_t)ms.ullAvailPhys +
                          (uint64_t)ms.ullAvailPageFile;
  return avail_virt < avail_commit ? avail_virt : avail_commit;
}

#elif defined(__linux__)
/* -----------------------------------------------------------------------
 * Linux: MemAvailable from /proc/meminfo (kernel 3.14+) is the most
 * accurate measure — it accounts for reclaimable caches and is the
 * same value shown by `free`.  Falls back to sysinfo() if unavailable.
 *
 * Caveat: Linux may overcommit memory (vm.overcommit_memory=0 default),
 * so the actual allocation limit can exceed what MemAvailable reports.
 * This check may therefore block allocations that Linux would honour.
 * ----------------------------------------------------------------------- */
#include <stdio.h>
#include <sys/sysinfo.h>
static inline uint64_t rxAvailableMemoryBytes(void) {
  FILE *f = fopen("/proc/meminfo", "r");
  if (f) {
    char line[256];
    while (fgets(line, sizeof(line), f)) {
      unsigned long long kb = 0;
      if (sscanf(line, "MemAvailable: %llu kB", &kb) == 1) {
        fclose(f);
        return (uint64_t)kb * 1024ULL;
      }
    }
    fclose(f);
  }
  struct sysinfo si;
  if (sysinfo(&si) == 0) {
    return ((uint64_t)si.freeram + (uint64_t)si.freeswap) *
           (uint64_t)si.mem_unit;
  }
  return UINT64_MAX;
}

#elif defined(__APPLE__) || defined(__MACH__)
/* -----------------------------------------------------------------------
 * macOS: Mach VM statistics.  free_count is immediately usable;
 * inactive_count is reclaimable under memory pressure.  Both are
 * included to avoid blocking allocations that macOS would satisfy
 * after a brief reclaim cycle.
 *
 * Caveat: inactive pages may be compressed (memory compression is
 * transparent on macOS), so this can overestimate available memory.
 * ----------------------------------------------------------------------- */
#include <mach/mach.h>
#include <mach/vm_statistics.h>
static inline uint64_t rxAvailableMemoryBytes(void) {
  vm_size_t page_size = 0;
  host_page_size(mach_host_self(), &page_size);
  if (page_size == 0) return UINT64_MAX;
  struct vm_statistics64 vm_stat;
  mach_msg_type_number_t count = HOST_VM_INFO64_COUNT;
  if (host_statistics64(mach_host_self(), HOST_VM_INFO64,
                        (host_info64_t)&vm_stat, &count) != KERN_SUCCESS) {
    return UINT64_MAX;
  }
  return ((uint64_t)vm_stat.free_count + (uint64_t)vm_stat.inactive_count)
         * (uint64_t)page_size;
}

#else
/* Unsupported platform — skip the check */
static inline uint64_t rxAvailableMemoryBytes(void) {
  return UINT64_MAX;
}
#endif

#endif /* RXODE2_MEM_AVAIL_H */
