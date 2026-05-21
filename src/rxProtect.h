#ifndef RX_PROTECT_H
#define RX_PROTECT_H

#ifdef __cplusplus


class rxProtect {
    int _count;
public:
    rxProtect() : _count(0) {}
    ~rxProtect() {
        if (_count > 0) {
            Rf_unprotect(_count);
        }
    }
    SEXP protect(SEXP x) {
        Rf_protect(x);
        _count++;
        return x;
    }
    void unprotect(int n = 0) {
        if (n == 0) n = _count;
        if (n > 0 && n <= _count) {
            Rf_unprotect(n);
            _count -= n;
        }
    }
};

// Map rxError to Rf_error for C++ exceptions to easily jump out
#define rxError Rf_error

#else /* C */
/* C equivalent of rxProtect: RAII guard using __attribute__((cleanup)).
 *
 * IMPORTANT: cleanup does NOT fire on longjmp. C functions that call
 * Rf_error/Rf_errorcall must still manually UNPROTECT before the error call.
 *
 * Portability:
 *   __has_attribute(cleanup): GCC 5+, all Clang, ICX (Intel LLVM-based)
 *   fallback guard: GCC < 5, excluding ICC classic (__INTEL_COMPILER)
 *   no-op fallback: ICC classic, MSVC -- code is still correct, just no RAII
 */

typedef struct { int _count; } rxProtectC;

static inline void _rxProtectC_dtor(rxProtectC *p) {
#ifdef RXODE2_DEBUG_PROTECT
  if (p->_count != 0)
    REprintf("protect imbalance: %d remaining (%s:%d)\n",
             p->_count, __FILE__, __LINE__);
#endif
  if (p->_count > 0) Rf_unprotect(p->_count);
}

#if defined(__has_attribute)
#  if __has_attribute(cleanup)
#    define RX_HAS_CLEANUP_ATTR 1
#  endif
#elif defined(__GNUC__) && !defined(__INTEL_COMPILER)
#  define RX_HAS_CLEANUP_ATTR 1
#endif

#ifdef RX_HAS_CLEANUP_ATTR
#  define rxProtectGuard \
     rxProtectC _rxpg \
     __attribute__((cleanup(_rxProtectC_dtor))) = {0}
#  define rxP(x)    (_rxpg._count++, PROTECT(x))
#  define rxUP(n)   do { _rxpg._count -= (n); UNPROTECT(n); } while (0)
#  define rxUPAll() do { if (_rxpg._count > 0) { \
                           UNPROTECT(_rxpg._count); _rxpg._count = 0; \
                         } } while (0)
#else
static inline SEXP rxP_(SEXP x, int *count) {
  (*count)++;
  return PROTECT(x);
}
static inline void rxUP_(int n, int *count) {
  if (n > 0) {
    *count -= n;
    UNPROTECT(n);
  }
}
static inline void rxUPAll_(int *count) {
  if (*count > 0) {
    UNPROTECT(*count);
    *count = 0;
  }
}
#  define rxProtectGuard  int _rxpgCount = 0;/* no RAII on this compiler */
#  define rxP(x)          rxP_(x, &_rxpgCount)
#  define rxUP(n)         rxUP_(n, &_rxpgCount)
#  define rxUPAll()       rxUPAll_()/* nothing */
#endif

#endif /* __cplusplus */

#endif /* RX_PROTECT_H */
