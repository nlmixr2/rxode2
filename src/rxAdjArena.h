// Adjoint backward-work arena: each thread's slice of the region at the end of
// gsolve (rxAdjWorkPerThread() in inst/include/rxMemoryCalc.h, carved in
// src/rxData.cpp and src/rxSerialize.cpp).  A subject's backward pass opens an
// rxAdjArenaScope and its rxAdjVec buffers bump through the slice; a request
// that does not fit (or a solve without the region) goes to the heap.  Freeing
// arena memory is a no-op: the scope gives the whole slice back on exit.
#ifndef RXODE2_ADJ_ARENA_H
#define RXODE2_ADJ_ARENA_H
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <new>
#include <type_traits>
#include <utility>
#include <vector>
#include "../inst/include/rxMemAlloc.h"
#include "../inst/include/rxMemoryCalc.h"
// rx_get_thread(): include rxomp.h (it has no include guard) before this header

extern "C" double *rxAdjWorkSlab(int slot, int64_t *n); // slot's slice (n doubles), or NULL
extern "C" int rxAdjWorkOwns(const void *p);            // p lies in the arena
extern "C" unsigned rxAdjWorkGen(void);                 // bumped whenever the arena moves
extern "C" void rxAdjWorkSpill(int tape);               // count a heap request (tape: a period tape)

struct rxAdjArenaState {
  double *cur = NULL, *end = NULL;
  int64_t *owner = NULL;
  unsigned gen = 0;
  int depth = 0;
};
extern thread_local rxAdjArenaState rxAdjTls;

// Tape = true: the heap kind, counted apart -- a buffer whose size the forward
// pass's step count sets (a per-period Jacobian tape, a per-step segment), so
// it cannot be sized at solve setup, or one held across subjects in a
// thread_local container (arena memory must not outlive its subject's scope).
// It leaves the slice to the buffers sized at setup.
template <class T, bool Tape = false>
struct rxAdjAlloc {
  typedef T value_type;
  template <class U>
  struct rebind {
    typedef rxAdjAlloc<U, Tape> other;
  };
  rxAdjAlloc() noexcept {}
  template <class U>
  rxAdjAlloc(const rxAdjAlloc<U, Tape> &) noexcept {}
  T *allocate(std::size_t n) {
    std::size_t nd = (n * sizeof(T) + sizeof(double) - 1) / sizeof(double);
    rxAdjArenaState &s = rxAdjTls;
    if (!Tape && s.owner != NULL && s.gen == rxAdjWorkGen() && (std::size_t)(s.end - s.cur) >= nd) {
      T *p = reinterpret_cast<T *>(s.cur);
      s.cur += nd;
      return p;
    }
    rxAdjWorkSpill(Tape ? 1 : 0);
    void *p = rxMemMalloc(adjointWork, n * sizeof(T));
    if (p == NULL) throw std::bad_alloc();
    return static_cast<T *>(p);
  }
  void deallocate(T *p, std::size_t) noexcept {
    if (!rxAdjWorkOwns(p)) free(p);
  }
};
template <class T, class U, bool A, bool B>
bool operator==(const rxAdjAlloc<T, A> &, const rxAdjAlloc<U, B> &) { return A == B; }
template <class T, class U, bool A, bool B>
bool operator!=(const rxAdjAlloc<T, A> &, const rxAdjAlloc<U, B> &) { return A != B; }

typedef std::vector<double, rxAdjAlloc<double> > rxAdjVec;
static_assert(sizeof(rxAdjVec) <= RX_ADJ_VEC_HDR * sizeof(double), "RX_ADJ_VEC_HDR is too small");
typedef std::vector<double, rxAdjAlloc<double, true> > rxAdjTapeVec;
typedef std::vector<rxAdjTapeVec, rxAdjAlloc<rxAdjTapeVec, true> > rxAdjTape;  // one per stage
typedef rxAdjTapeVec rxAdjHeapVec;  // the heap kind, for other step-scaled or thread_local buffers
typedef std::vector<char, rxAdjAlloc<char, true> > rxAdjHeapVecC;

// The only way to size an adjointWork buffer: it must be an arena vector, so a
// heap std::vector cannot be counted as adjointWork (test-mem-alloc-tags.R
// rejects any other adjointWork tag outside this header).
template <class V>
struct rxAdjIsArena : std::false_type {};
template <class T, bool Tape>
struct rxAdjIsArena<std::vector<T, rxAdjAlloc<T, Tape> > > : std::true_type {};
template <class V, class... A>
inline void rxAdjAssign_(V &v, A &&...a) {
  static_assert(rxAdjIsArena<V>::value, "an adjointWork buffer must be an arena vector (rxAdjVec)");
  v.assign(std::forward<A>(a)...);
}
template <class V, class... A>
inline void rxAdjResize_(V &v, A &&...a) {
  static_assert(rxAdjIsArena<V>::value, "an adjointWork buffer must be an arena vector (rxAdjVec)");
  v.resize(std::forward<A>(a)...);
}
template <class V>
inline void rxAdjReserve_(V &v, std::size_t n) {
  static_assert(rxAdjIsArena<V>::value, "an adjointWork buffer must be an arena vector (rxAdjVec)");
  v.reserve(n);
}
#define rxAdjAssign(v, ...) rxAdjAssign_((v), __VA_ARGS__)
#define rxAdjReserve(v, n) rxAdjReserve_((v), (n))
#define rxAdjResize(v, ...) rxAdjResize_((v), __VA_ARGS__)

// One subject's backward work: claims this thread's slice and gives it back on
// exit; a nested scope keeps bumping through the outer one's.  A slice another thread holds (a
// caller running wider than op->cores) is left alone, and that subject uses
// the heap.
struct rxAdjArenaScope {
  rxAdjArenaState saved;
  bool claimed = false, nested = false;
  explicit rxAdjArenaScope(int cores) {
    rxAdjArenaState &s = rxAdjTls;
    saved = s;
    unsigned gen = rxAdjWorkGen();
    if (s.depth > 0 && s.gen == gen) {
      s.depth++;
      nested = true;
      return;
    }
    s = rxAdjArenaState();
    s.depth = 1;
    s.gen = gen;
    int64_t n = 0;
    double *slab = rxAdjWorkSlab(rx_get_thread(cores), &n);
    if (slab == NULL || n <= 1) return;
    int64_t *own = reinterpret_cast<int64_t *>(slab);
    int64_t expect = 0;
    if (!__atomic_compare_exchange_n(own, &expect, (int64_t)1, false, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) return;
    s.owner = own;
    s.cur = slab + 1;
    s.end = slab + n;
    claimed = true;
  }
  ~rxAdjArenaScope() {
    rxAdjArenaState &s = rxAdjTls;
    if (nested) {  // what it handed out may outlive it: keep bumping
      s.depth--;
      return;
    }
    if (claimed) __atomic_store_n(s.owner, (int64_t)0, __ATOMIC_RELEASE);
    s = saved;
  }
  rxAdjArenaScope(const rxAdjArenaScope &) = delete;
  rxAdjArenaScope &operator=(const rxAdjArenaScope &) = delete;
};

#endif
