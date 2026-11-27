#ifndef RXODE2_MEM_ALLOC_H
#define RXODE2_MEM_ALLOC_H
// Tagged allocation: every allocation names the rxSolve() memory-estimate
// component that counts it -- a name rxMemoryComponents_() or
// rxMemoryGpuComponents_() (src/rxMemoryComponents.cpp) returns, or `fixed`
// for memory that does not scale with subjects, events or threads (parser
// tables, a few index vectors).  tests/testthat/test-mem-alloc-tags.R rejects
// a raw allocator (malloc, calloc, realloc, R_alloc, R_Calloc, new T[],
// std::vector assign/resize, clCreateBuffer) and a tag that names no
// component, so the estimate cannot drift from what rxode2 allocates.  The tag
// is not compiled; the test reads it.
#define rxMemMalloc(comp, n) malloc(n)
#define rxMemCalloc(comp, n, s) calloc((n), (s))
#define rxMemRealloc(comp, p, n) realloc((p), (n))
#define rxMemRalloc(comp, n, t) R_alloc((n), (t))
#define rxMemNew(comp, T, n) (new T[n]())
#define rxMemAssign(comp, v, ...) (v).assign(__VA_ARGS__)
#define rxMemResize(comp, v, ...) (v).resize(__VA_ARGS__)
// any other allocation (a constructor, a device buffer)
#define rxMemTag(comp, expr) (expr)
#endif
