// OpenCL rounds for the device integrators (inst/include/rxode2gpu).
//
// rxode2 neither links OpenCL nor needs its headers: the few entry points
// used are looked up at run time in the OpenCL library that the OpenCL R
// package has already loaded, and the context, queue and kernel are the raw
// handles that package keeps in its external pointers.  R builds those once
// per solve; each round (rxGpuClRound()) is plain C.
#define STRICT_R_HEADERS
#define USE_FC_LEN_T
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include "rxProtect.h"
#include "gpu_opencl.h"

#ifdef _WIN32
#include <windows.h>
#define RXCL_API __stdcall
#else
#include <dlfcn.h>
#define RXCL_API
#endif

typedef int32_t cl_int;
typedef uint32_t cl_uint;
typedef uint64_t cl_mem_flags;

#define RXCL_MEM_READ_WRITE ((cl_mem_flags)1)
#define RXCL_MEM_READ_ONLY ((cl_mem_flags)4)
#define RXCL_KERNEL_NUM_ARGS 0x1191
#define RXCL_SUCCESS 0

typedef void *(RXCL_API *clCreateBuffer_t)(void *, cl_mem_flags, size_t,
                                           void *, cl_int *);
typedef cl_int (RXCL_API *clReleaseMemObject_t)(void *);
typedef cl_int (RXCL_API *clEnqueueWriteBuffer_t)(void *, void *, cl_uint,
                                                  size_t, size_t, const void *,
                                                  cl_uint, const void *,
                                                  void *);
typedef cl_int (RXCL_API *clEnqueueReadBuffer_t)(void *, void *, cl_uint,
                                                 size_t, size_t, void *,
                                                 cl_uint, const void *,
                                                 void *);
typedef cl_int (RXCL_API *clSetKernelArg_t)(void *, cl_uint, size_t,
                                            const void *);
typedef cl_int (RXCL_API *clEnqueueNDRangeKernel_t)(void *, void *, cl_uint,
                                                    const size_t *,
                                                    const size_t *,
                                                    const size_t *, cl_uint,
                                                    const void *, void *);
typedef cl_int (RXCL_API *clGetKernelInfo_t)(void *, cl_uint, size_t, void *,
                                             size_t *);
typedef cl_int (RXCL_API *clFinish_t)(void *);

static struct {
  int loaded;
  clCreateBuffer_t createBuffer;
  clReleaseMemObject_t releaseMem;
  clEnqueueWriteBuffer_t write;
  clEnqueueReadBuffer_t read;
  clSetKernelArg_t setArg;
  clEnqueueNDRangeKernel_t run;
  clGetKernelInfo_t kernelInfo;
  clFinish_t finish;
} rxcl = {0};

#ifdef _WIN32
static void *rxclSym(void *lib, const char *name) {
  return (void *)GetProcAddress((HMODULE)lib, name);
}
static void *rxclOpen(void) {
  HMODULE h = GetModuleHandleA("OpenCL.dll");
  if (h == NULL) h = LoadLibraryA("OpenCL.dll");
  return (void *)h;
}
#else
static void *rxclSym(void *lib, const char *name) {
  return dlsym(lib, name);
}
static void *rxclOpen(void) {
  static const char *names[] = {
#ifdef __APPLE__
    "/System/Library/Frameworks/OpenCL.framework/OpenCL",
#endif
    "libOpenCL.so.1", "libOpenCL.so"};
  int n = (int)(sizeof(names)/sizeof(names[0]));
  // the library the OpenCL package loaded, else load it
  for (int i = 0; i < n; i++) {
    void *h = dlopen(names[i], RTLD_LAZY | RTLD_NOLOAD);
    if (h != NULL) return h;
  }
  for (int i = 0; i < n; i++) {
    void *h = dlopen(names[i], RTLD_LAZY);
    if (h != NULL) return h;
  }
  return NULL;
}
#endif

// 1 when every entry point was found
int rxGpuClLoad(void) {
  if (rxcl.loaded) return rxcl.loaded == 1;
  void *lib = rxclOpen();
  rxcl.loaded = 2;
  if (lib == NULL) return 0;
  rxcl.createBuffer = (clCreateBuffer_t)rxclSym(lib, "clCreateBuffer");
  rxcl.releaseMem = (clReleaseMemObject_t)rxclSym(lib, "clReleaseMemObject");
  rxcl.write = (clEnqueueWriteBuffer_t)rxclSym(lib, "clEnqueueWriteBuffer");
  rxcl.read = (clEnqueueReadBuffer_t)rxclSym(lib, "clEnqueueReadBuffer");
  rxcl.setArg = (clSetKernelArg_t)rxclSym(lib, "clSetKernelArg");
  rxcl.run = (clEnqueueNDRangeKernel_t)rxclSym(lib, "clEnqueueNDRangeKernel");
  rxcl.kernelInfo = (clGetKernelInfo_t)rxclSym(lib, "clGetKernelInfo");
  rxcl.finish = (clFinish_t)rxclSym(lib, "clFinish");
  if (rxcl.createBuffer && rxcl.releaseMem && rxcl.write && rxcl.read &&
      rxcl.setArg && rxcl.run && rxcl.kernelInfo && rxcl.finish) {
    rxcl.loaded = 1;
  }
  return rxcl.loaded == 1;
}

// Request (r), subject state (s) and option (o) buffers, in kernel
// argument order after (y, n)
enum {RXG_B_Y, RXG_B_RD, RXG_B_RI, RXG_B_P0, RXG_B_P1, RXG_B_PL, RXG_B_IR,
  RXG_B_ON, RXG_B_AD, RXG_B_AI, RXG_B_HS, RXG_B_HREC, RXG_B_OD, RXG_B_OI,
  RXG_B_N};
typedef char rxGpuClBufCheck[(sizeof(((rxGpuCl *)0)->buf) ==
                              RXG_B_N*sizeof(void *)) ? 1 : -1];

static int rxGpuClIsInt(int b) {
  return b == RXG_B_RI || b == RXG_B_ON || b == RXG_B_AI || b == RXG_B_OI;
}

// Values per request (or per subject) in each buffer
static void rxGpuClWidths(const rxGpuCl *g, size_t *w) {
  const int *d = g->dims;
  w[RXG_B_Y] = (size_t)d[RXG_D_NEQ1];
  w[RXG_B_RD] = (size_t)d[RXG_D_RD];
  w[RXG_B_RI] = (size_t)d[RXG_D_RI];
  w[RXG_B_P0] = w[RXG_B_P1] = (size_t)d[RXG_D_NPAR1];
  w[RXG_B_PL] = (size_t)d[RXG_D_NLHS1];
  w[RXG_B_IR] = w[RXG_B_ON] = (size_t)d[RXG_D_NEQ1];
  w[RXG_B_AD] = (size_t)d[RXG_D_AD];
  w[RXG_B_AI] = (size_t)d[RXG_D_AI];
  w[RXG_B_HS] = (size_t)d[RXG_D_HHDR];
  w[RXG_B_HREC] = d[RXG_D_DELAY] ?
    (size_t)d[RXG_D_HREC]*(size_t)d[RXG_D_HCAP] : 1;
  w[RXG_B_OD] = (size_t)d[RXG_D_OD];
  w[RXG_B_OI] = (size_t)d[RXG_D_OI];
}

static size_t rxGpuClElt(const rxGpuCl *g, int b) {
  return rxGpuClIsInt(b) ? sizeof(int) : (g->single ? sizeof(float) : sizeof(double));
}

void rxGpuClFree(rxGpuCl *g) {
  if (g == NULL) return;
  for (int b = 0; b < RXG_B_N; b++) {
    if (g->buf[b] != NULL) rxcl.releaseMem(g->buf[b]);
    g->buf[b] = NULL;
  }
  free(g->stage);
  free(g);
}

// Copy n doubles into the device precision (float stages through g->stage)
static const void *rxGpuClIn(rxGpuCl *g, const double *x, size_t n) {
  if (!g->single) return x;
  float *f = (float *)g->stage;
  for (size_t i = 0; i < n; i++) f[i] = (float)x[i];
  return f;
}

static cl_int rxGpuClWrite(rxGpuCl *g, int b, const void *x, size_t n) {
  if (n == 0) return RXCL_SUCCESS;
  const void *src = rxGpuClIsInt(b) ? x : rxGpuClIn(g, (const double *)x, n);
  return rxcl.write(g->queue, g->buf[b], 1, 0, n*rxGpuClElt(g, b), src, 0,
                    NULL, NULL);
}

static cl_int rxGpuClRead(rxGpuCl *g, int b, void *x, size_t n) {
  if (n == 0) return RXCL_SUCCESS;
  if (rxGpuClIsInt(b) || !g->single) {
    return rxcl.read(g->queue, g->buf[b], 1, 0, n*rxGpuClElt(g, b), x, 0,
                     NULL, NULL);
  }
  float *f = (float *)g->stage;
  cl_int e = rxcl.read(g->queue, g->buf[b], 1, 0, n*sizeof(float), f, 0,
                       NULL, NULL);
  double *d = (double *)x;
  for (size_t i = 0; i < n; i++) d[i] = (double)f[i];
  return e;
}

// Buffers for up to dims[RXG_D_NSUB] requests on a built rxg_kernel; od/oi
// are written once.  NULL with *err set on failure.
rxGpuCl *rxGpuClNew(void *ctx, void *queue, void *kernel, const int *dims,
                    const double *od, const int *oi, int single, int *err) {
  *err = 0;
  if (!rxGpuClLoad()) {
    *err = 1;
    return NULL;
  }
  cl_uint nargs = 0;
  if (rxcl.kernelInfo(kernel, RXCL_KERNEL_NUM_ARGS, sizeof(nargs), &nargs,
                      NULL) != RXCL_SUCCESS || nargs != RXG_B_N + 1) {
    *err = 2;
    return NULL;
  }
  rxGpuCl *g = (rxGpuCl *)calloc(1, sizeof(rxGpuCl));
  if (g == NULL) {
    *err = 3;
    return NULL;
  }
  g->ctx = ctx;
  g->queue = queue;
  g->kernel = kernel;
  g->single = single;
  memcpy(g->dims, dims, sizeof(int)*RXG_D_N);
  size_t w[RXG_B_N], most = 0, nsub = (size_t)dims[RXG_D_NSUB];
  rxGpuClWidths(g, w);
  for (int b = 0; b < RXG_B_N; b++) {
    size_t n = (b == RXG_B_OD || b == RXG_B_OI) ? w[b] : w[b]*nsub;
    if (n == 0) n = 1;
    if (n > most && b < RXG_B_AD) most = n;
    cl_int e = 0;
    g->buf[b] = rxcl.createBuffer(g->ctx, RXCL_MEM_READ_WRITE,
                                  n*rxGpuClElt(g, b), NULL, &e);
    if (e != RXCL_SUCCESS || g->buf[b] == NULL) {
      g->buf[b] = NULL;
      rxGpuClFree(g);
      *err = 4;
      return NULL;
    }
  }
  // float staging covers the largest request or option buffer
  if (most < w[RXG_B_OD]) most = w[RXG_B_OD];
  g->stage = malloc(most*sizeof(double));
  if (g->stage == NULL) {
    rxGpuClFree(g);
    *err = 3;
    return NULL;
  }
  if (rxGpuClWrite(g, RXG_B_OD, od, w[RXG_B_OD]) != RXCL_SUCCESS ||
      rxGpuClWrite(g, RXG_B_OI, oi, w[RXG_B_OI]) != RXCL_SUCCESS) {
    rxGpuClFree(g);
    *err = 5;
    return NULL;
  }
  return g;
}

// One round: n requests in the layout of rxg_round.h.  y, rd and ri are
// updated in place.  Returns 0, or the OpenCL error code.
int rxGpuClRound(rxGpuCl *g, int n, double *y, double *rd, int *ri,
                 const double *p0, const double *p1, const double *pl,
                 const double *ir, const int *on) {
  if (n <= 0) return 0;
  if (n > g->dims[RXG_D_NSUB]) return -1000;
  size_t w[RXG_B_N], nn = (size_t)n;
  rxGpuClWidths(g, w);
  const void *in[RXG_B_ON + 1] = {y, rd, ri, p0, p1, pl, ir, on};
  cl_int e = RXCL_SUCCESS;
  for (int b = 0; b <= RXG_B_ON && e == RXCL_SUCCESS; b++) {
    e = rxGpuClWrite(g, b, in[b], w[b]*nn);
  }
  if (e != RXCL_SUCCESS) return e;
  // every round sets all arguments: runs may share a cached kernel
  for (int b = 0; b < RXG_B_N && e == RXCL_SUCCESS; b++) {
    e = rxcl.setArg(g->kernel, (cl_uint)(b == 0 ? 0 : b + 1), sizeof(void *),
                    &(g->buf[b]));
  }
  cl_int ni = (cl_int)n;
  if (e == RXCL_SUCCESS) e = rxcl.setArg(g->kernel, 1, sizeof(cl_int), &ni);
  if (e != RXCL_SUCCESS) return e;
  size_t global = nn;
  e = rxcl.run(g->queue, g->kernel, 1, NULL, &global, NULL, 0, NULL, NULL);
  // the OpenCL package's queue may run out of order
  if (e == RXCL_SUCCESS) e = rxcl.finish(g->queue);
  if (e != RXCL_SUCCESS) return e;
  e = rxGpuClRead(g, RXG_B_Y, y, w[RXG_B_Y]*nn);
  if (e == RXCL_SUCCESS) e = rxGpuClRead(g, RXG_B_RD, rd, w[RXG_B_RD]*nn);
  if (e == RXCL_SUCCESS) e = rxGpuClRead(g, RXG_B_RI, ri, w[RXG_B_RI]*nn);
  return e;
}

//////////////////////////////////////////////////////////////////////////////
// R interface: setup, and a round entry for the tests (the solver calls
// rxGpuClRound() directly)

static void rxGpuClFinalize(SEXP p) {
  rxGpuCl *g = (rxGpuCl *)R_ExternalPtrAddr(p);
  if (g != NULL) {
    rxGpuClFree(g);
    R_ClearExternalPtr(p);
  }
}

static rxGpuCl *rxGpuClGet(SEXP p) {
  if (TYPEOF(p) != EXTPTRSXP) (Rf_error)("invalid OpenCL run");
  rxGpuCl *g = (rxGpuCl *)R_ExternalPtrAddr(p);
  if (g == NULL) (Rf_error)("the OpenCL run was released");
  return g;
}

static void *rxGpuClHandle(SEXP x, const char *what) {
  if (TYPEOF(x) != EXTPTRSXP || R_ExternalPtrAddr(x) == NULL) {
    (Rf_error)("invalid OpenCL %s", what);
  }
  return R_ExternalPtrAddr(x);
}

SEXP _rxode2_rxGpuClAvailable(void) {
  return Rf_ScalarLogical(rxGpuClLoad());
}

// ctx/queue/kernel are the OpenCL package's objects; keep is held by the
// result so they outlive it
SEXP _rxode2_rxGpuClNew(SEXP ctx, SEXP queue, SEXP kernel, SEXP dims,
                        SEXP od, SEXP oi, SEXP single, SEXP keep) {
  rxProtectGuard;
  if (Rf_length(dims) != RXG_D_N) (Rf_error)("dims must have %d values", RXG_D_N);
  void *c = rxGpuClHandle(ctx, "context");
  void *q = rxGpuClHandle(queue, "command queue");
  void *k = rxGpuClHandle(kernel, "kernel");
  const int *d = INTEGER(dims);
  if (Rf_length(od) != d[RXG_D_OD] || Rf_length(oi) != d[RXG_D_OI]) {
    (Rf_error)("the solver options do not match the model");
  }
  int err = 0;
  rxGpuCl *g = rxGpuClNew(c, q, k, d, REAL(od), INTEGER(oi),
                             Rf_asLogical(single), &err);
  if (g == NULL) {
    switch (err) {
    case 1: (Rf_error)("cannot find the OpenCL library");
    case 2: (Rf_error)("the OpenCL kernel is not an rxode2 round kernel");
    case 3: (Rf_error)("out of memory");
    default: (Rf_error)("cannot set up the OpenCL buffers (step %d)", err);
    }
  }
  SEXP ret = rxP(R_MakeExternalPtr(g, R_NilValue, keep));
  R_RegisterCFinalizerEx(ret, rxGpuClFinalize, TRUE);
  rxUPAll();
  return ret;
}

SEXP _rxode2_rxGpuClRound(SEXP run, SEXP n, SEXP y, SEXP rd, SEXP ri,
                          SEXP p0, SEXP p1, SEXP pl, SEXP ir, SEXP on) {
  rxProtectGuard;
  rxGpuCl *g = rxGpuClGet(run);
  int nn = Rf_asInteger(n);
  const int *d = g->dims;
  if (nn < 0 || nn > d[RXG_D_NSUB] ||
      Rf_xlength(y) < (R_xlen_t)nn*d[RXG_D_NEQ1] ||
      Rf_xlength(rd) < (R_xlen_t)nn*d[RXG_D_RD] ||
      Rf_xlength(ri) < (R_xlen_t)nn*d[RXG_D_RI] ||
      Rf_xlength(p0) < (R_xlen_t)nn*d[RXG_D_NPAR1] ||
      Rf_xlength(p1) < (R_xlen_t)nn*d[RXG_D_NPAR1] ||
      Rf_xlength(pl) < (R_xlen_t)nn*d[RXG_D_NLHS1] ||
      Rf_xlength(ir) < (R_xlen_t)nn*d[RXG_D_NEQ1] ||
      Rf_xlength(on) < (R_xlen_t)nn*d[RXG_D_NEQ1]) {
    (Rf_error)("the round's requests do not match the run");
  }
  SEXP yo = rxP(Rf_duplicate(y));
  SEXP rdo = rxP(Rf_duplicate(rd));
  SEXP rio = rxP(Rf_duplicate(ri));
  int e = rxGpuClRound(g, nn, REAL(yo), REAL(rdo), INTEGER(rio), REAL(p0),
                       REAL(p1), REAL(pl), REAL(ir), INTEGER(on));
  if (e != 0) {
    rxUPAll();
    (Rf_error)("OpenCL round failed (error %d)", e);
  }
  SEXP ret = rxP(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(ret, 0, yo);
  SET_VECTOR_ELT(ret, 1, rdo);
  SET_VECTOR_ELT(ret, 2, rio);
  SEXP nm = rxP(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(nm, 0, Rf_mkChar("y"));
  SET_STRING_ELT(nm, 1, Rf_mkChar("rd"));
  SET_STRING_ELT(nm, 2, Rf_mkChar("ri"));
  Rf_setAttrib(ret, R_NamesSymbol, nm);
  rxUPAll();
  return ret;
}

SEXP _rxode2_rxGpuClFree(SEXP run) {
  rxGpuClFinalize(run);
  return R_NilValue;
}
