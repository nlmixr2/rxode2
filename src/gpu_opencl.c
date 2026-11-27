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
#include "../inst/include/rxMemAlloc.h"

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
typedef cl_int (RXCL_API *clGetDeviceInfo_t)(void *, cl_uint, size_t, void *,
                                             size_t *);
typedef cl_int (RXCL_API *clReleaseEvent_t)(void *);
typedef cl_int (RXCL_API *clWaitForEvents_t)(cl_uint, void *const *);
typedef cl_int (RXCL_API *clGetEventInfo_t)(void *, cl_uint, size_t, void *,
                                            size_t *);
#define RXCL_EVENT_COMMAND_EXECUTION_STATUS 0x11D3
typedef cl_int (RXCL_API *clGetCommandQueueInfo_t)(void *, cl_uint, size_t,
                                                   void *, size_t *);
typedef cl_int (RXCL_API *clGetKernelWorkGroupInfo_t)(void *, void *, cl_uint,
                                                      size_t, void *, size_t *);
#define RXCL_QUEUE_DEVICE 0x1091
#define RXCL_DEVICE_MAX_COMPUTE_UNITS 0x1002
#define RXCL_DEVICE_MAX_WORK_ITEM_SIZES 0x1005
#define RXCL_KERNEL_WORK_GROUP_SIZE 0x11B0
#define RXCL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE 0x11B3

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
  clGetDeviceInfo_t deviceInfo; // optional (the device memory check)
  // optional (the one-wait rounds)
  clReleaseEvent_t releaseEvent;
  clWaitForEvents_t waitEvents;
  clGetEventInfo_t eventInfo;
  // optional (the work-group size)
  clGetCommandQueueInfo_t queueInfo;
  clGetKernelWorkGroupInfo_t kernelWgInfo;
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
  rxcl.deviceInfo = (clGetDeviceInfo_t)rxclSym(lib, "clGetDeviceInfo");
  rxcl.releaseEvent = (clReleaseEvent_t)rxclSym(lib, "clReleaseEvent");
  rxcl.waitEvents = (clWaitForEvents_t)rxclSym(lib, "clWaitForEvents");
  rxcl.eventInfo = (clGetEventInfo_t)rxclSym(lib, "clGetEventInfo");
  rxcl.queueInfo = (clGetCommandQueueInfo_t)rxclSym(lib, "clGetCommandQueueInfo");
  rxcl.kernelWgInfo = (clGetKernelWorkGroupInfo_t)rxclSym(lib, "clGetKernelWorkGroupInfo");
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
  RXG_B_OFF, RXG_B_N};
// the kernel's mode argument (0 rounds, 1 timelines) follows the buffers
#define RXG_ARG_TL (RXG_B_N + 1)
typedef char rxGpuClBufCheck[(sizeof(((rxGpuCl *)0)->buf) ==
                              RXG_B_N*sizeof(void *)) ? 1 : -1];

static int rxGpuClIsInt(int b) {
  return b == RXG_B_RI || b == RXG_B_ON || b == RXG_B_AI || b == RXG_B_OI ||
    b == RXG_B_OFF;
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
  w[RXG_B_OFF] = 1;
}

// Values in buffer b: request buffers hold RXG_D_NROW rows, subject states
// RXG_D_NSUB, the timeline offsets RXG_D_NSUB + 1, the options one copy
static size_t rxGpuClCount(const int *d, const size_t *w, int b) {
  size_t nsub = (size_t)d[RXG_D_NSUB];
  size_t nrow = (size_t)(d[RXG_D_NROW] > d[RXG_D_NSUB] ? d[RXG_D_NROW] : d[RXG_D_NSUB]);
  size_t n;
  if (b <= RXG_B_ON) {
    n = w[b]*nrow;
  } else if (b == RXG_B_OD || b == RXG_B_OI) {
    n = w[b];
  } else if (b == RXG_B_OFF) {
    n = nsub + 1;
  } else {
    n = w[b]*nsub;
  }
  return n == 0 ? 1 : n;
}

static size_t rxGpuClElt(const rxGpuCl *g, int b) {
  return rxGpuClIsInt(b) ? sizeof(int) : (g->single ? sizeof(float) : sizeof(double));
}

// Bytes rxGpuClNew() allocates for layout dims: out[0] on the device
// (gpuDevice), out[1] on the host (gpuStage: the float staging buffer and the
// zeroed block that starts the subject states, live together at setup),
// out[2] the largest single device buffer
void rxGpuClBytes(const int *dims, int single, double *out) {
  rxGpuCl g;
  memset(&g, 0, sizeof(g));
  memcpy(g.dims, dims, sizeof(int)*RXG_D_N);
  g.single = single;
  size_t w[RXG_B_N], most = 0, nsub = (size_t)dims[RXG_D_NSUB];
  rxGpuClWidths(&g, w);
  double dev = 0.0, big = 0.0;
  for (int b = 0; b < RXG_B_N; b++) {
    size_t n = rxGpuClCount(dims, w, b);
    if (n > most && b < RXG_B_AD) most = n;
    double nb = (double)n*rxGpuClElt(&g, b);
    if (nb > big) big = nb;
    dev += nb;
  }
  if (most < w[RXG_B_OD]) most = w[RXG_B_OD];
  size_t nz = w[RXG_B_AI]*nsub;
  if (w[RXG_B_AD]*nsub > nz) nz = w[RXG_B_AD]*nsub;
  if (w[RXG_B_HS]*nsub > nz) nz = w[RXG_B_HS]*nsub;
  out[0] = dev;
  out[1] = ((double)most + (double)(nz > 0 ? nz : 1))*sizeof(double);
  out[2] = big;
}

void rxGpuClFree(rxGpuCl *g) {
  if (g == NULL) return;
  // a run made before rxode2 was reloaded reaches a DLL whose entry points
  // are not resolved yet
  int ok = rxGpuClLoad();
  for (int b = 0; b < RXG_B_N; b++) {
    if (!ok) break;
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

// does the kernel take the arguments rxGpuClArgs() sets?
static int rxGpuClKernelOk(void *kernel) {
  cl_uint nargs = 0;
  return rxcl.kernelInfo(kernel, RXCL_KERNEL_NUM_ARGS, sizeof(nargs), &nargs,
                         NULL) == RXCL_SUCCESS && nargs == RXG_B_N + 2;
}

// create g's device buffers of widths w (rxGpuClCount() values each); *most
// gets the largest request buffer (in elements).  Returns 0, or 4 on failure.
static int rxGpuClBuffers(rxGpuCl *g, const size_t *w, size_t *most) {
  *most = 0;
  for (int b = 0; b < RXG_B_N; b++) {
    size_t n = rxGpuClCount(g->dims, w, b);
    if (n > *most && b < RXG_B_AD) *most = n;
    cl_int e = 0;
    g->buf[b] = rxMemTag(gpuDevice, rxcl.createBuffer(g->ctx, RXCL_MEM_READ_WRITE,
                                                      n*rxGpuClElt(g, b), NULL, &e));
    if (e != RXCL_SUCCESS || g->buf[b] == NULL) {
      g->buf[b] = NULL;
      return 4;
    }
  }
  return 0;
}

// the subject states start zeroed: ai[RXG_AI_SEEN] = 0 starts a subject
// fresh on its first round.  Returns 0, 3 (no memory) or 6 (a failed write).
static int rxGpuClZero(rxGpuCl *g, const size_t *w, size_t nsub) {
  size_t nz = w[RXG_B_AI]*nsub;
  if (w[RXG_B_AD]*nsub > nz) nz = w[RXG_B_AD]*nsub;
  if (w[RXG_B_HS]*nsub > nz) nz = w[RXG_B_HS]*nsub;
  void *zero = rxMemCalloc(gpuStage, nz > 0 ? nz : 1, sizeof(double));
  if (zero == NULL) return 3;
  int zb[3] = {RXG_B_AD, RXG_B_AI, RXG_B_HS};
  int ret = 0;
  for (int i = 0; i < 3 && ret == 0; i++) {
    int b = zb[i];
    if (rxcl.write(g->queue, g->buf[b], 1, 0,
                   w[b]*nsub*rxGpuClElt(g, b), zero, 0, NULL, NULL) != RXCL_SUCCESS) {
      ret = 6;
    }
  }
  free(zero);
  return ret;
}

// buffers, staging and the one-time writes of a new g; returns 0 or the
// rxGpuClNew() error code
static int rxGpuClFill(rxGpuCl *g, const double *od, const int *oi) {
  size_t w[RXG_B_N], most = 0, nsub = (size_t)g->dims[RXG_D_NSUB];
  rxGpuClWidths(g, w);
  int e = rxGpuClBuffers(g, w, &most);
  if (e != 0) return e;
  // float staging covers the largest request or option buffer
  if (most < w[RXG_B_OD]) most = w[RXG_B_OD];
  g->stage = rxMemMalloc(gpuStage, most*sizeof(double));
  if (g->stage == NULL) return 3;
  if (rxGpuClWrite(g, RXG_B_OD, od, w[RXG_B_OD]) != RXCL_SUCCESS ||
      rxGpuClWrite(g, RXG_B_OI, oi, w[RXG_B_OI]) != RXCL_SUCCESS) {
    return 5;
  }
  return rxGpuClZero(g, w, nsub);
}

// The device's compute units and the kernel's work-group limits; zero when
// the driver does not report them (the driver then picks the work-group size)
static void rxGpuClWorkGroup(rxGpuCl *g) {
  g->cu = g->wgMax = g->wgMult = 0;
  if (rxcl.queueInfo == NULL || rxcl.kernelWgInfo == NULL || rxcl.deviceInfo == NULL) return;
  void *dev = NULL;
  cl_uint cu = 0;
  size_t wgMax = 0, wgMult = 0;
  if (rxcl.queueInfo(g->queue, RXCL_QUEUE_DEVICE, sizeof(dev), &dev, NULL) != RXCL_SUCCESS ||
      dev == NULL ||
      rxcl.deviceInfo(dev, RXCL_DEVICE_MAX_COMPUTE_UNITS, sizeof(cu), &cu, NULL) != RXCL_SUCCESS ||
      rxcl.kernelWgInfo(g->kernel, dev, RXCL_KERNEL_WORK_GROUP_SIZE, sizeof(wgMax),
                        &wgMax, NULL) != RXCL_SUCCESS) {
    return;
  }
  if (rxcl.kernelWgInfo(g->kernel, dev, RXCL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE,
                        sizeof(wgMult), &wgMult, NULL) != RXCL_SUCCESS) {
    wgMult = 0;
  }
  // the first dimension's limit also bounds a one-dimensional work group
  size_t items[3] = {0, 0, 0};
  if (rxcl.deviceInfo(dev, RXCL_DEVICE_MAX_WORK_ITEM_SIZES, sizeof(items), items,
                      NULL) == RXCL_SUCCESS && items[0] > 0 && items[0] < wgMax) {
    wgMax = items[0];
  }
  if (cu == 0 || wgMax == 0) return;
  g->cu = (size_t)cu;
  g->wgMax = wgMax;
  g->wgMult = wgMult > 0 ? wgMult : 1;
}

// Work-group size for n work items (0: the driver's choice).  From the
// device, enough groups to give every compute unit work: a few work items
// leave most compute units idle when the driver packs them into one or two
// large groups.
static size_t rxGpuClLocal(const rxGpuCl *g, size_t n) {
  int lws = g->dims[RXG_D_LWS];
  if (lws == 0 || g->wgMax == 0) return 0;
  size_t L;
  if (lws > 0) {
    L = (size_t)lws;
  } else {
    // the largest multiple of the preferred size that still gives every
    // compute unit a group
    size_t m = g->wgMult, per = n/g->cu;
    L = (per/m)*m;
    if (L < m) L = m;
    size_t cap = 128;
    if (cap < m) cap = m;
    if (L > cap) L = cap;
  }
  if (L > g->wgMax) L = g->wgMax;
  return L;
}

// Launch nwork work items, padded to a multiple of the work-group size (the
// kernel skips items past n)
static cl_int rxGpuClLaunch(rxGpuCl *g, int nwork, cl_uint nev, void **ev, void **evk) {
  size_t global = (size_t)nwork, L = rxGpuClLocal(g, global);
  // the kernel's index is an int: padding must not pass INT_MAX
  if (L > 0 && global > (size_t)2147483647 - L) L = 0;
  if (L > 0) global = ((global + L - 1)/L)*L;
  return rxcl.run(g->queue, g->kernel, 1, NULL, &global, L > 0 ? &L : NULL, nev,
                  nev ? ev : NULL, evk);
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
  if (!rxGpuClKernelOk(kernel)) {
    *err = 2;
    return NULL;
  }
  rxGpuCl *g = (rxGpuCl *)rxMemCalloc(fixed, 1, sizeof(rxGpuCl));
  if (g == NULL) {
    *err = 3;
    return NULL;
  }
  g->ctx = ctx;
  g->queue = queue;
  g->kernel = kernel;
  g->single = single;
  memcpy(g->dims, dims, sizeof(int)*RXG_D_N);
  rxGpuClWorkGroup(g);
  *err = rxGpuClFill(g, od, oi);
  if (*err != 0) {
    rxGpuClFree(g);
    return NULL;
  }
  return g;
}

// Set every kernel argument (runs may share a cached kernel): the buffers,
// the work-item count and the mode
static cl_int rxGpuClArgs(rxGpuCl *g, int nwork, int tl) {
  cl_int e = RXCL_SUCCESS;
  for (int b = 0; b < RXG_B_N && e == RXCL_SUCCESS; b++) {
    e = rxcl.setArg(g->kernel, (cl_uint)(b == 0 ? 0 : b + 1), sizeof(void *),
                    &(g->buf[b]));
  }
  cl_int ni = (cl_int)nwork, t = (cl_int)tl;
  if (e == RXCL_SUCCESS) e = rxcl.setArg(g->kernel, 1, sizeof(cl_int), &ni);
  if (e == RXCL_SUCCESS) e = rxcl.setArg(g->kernel, RXG_ARG_TL, sizeof(cl_int), &t);
  return e;
}

// queue the writes of the nrow request rows (and, with tl, the nwork + 1
// timeline offsets) without blocking; ev[*nev] receive their events
static cl_int rxGpuClQueueWrites(rxGpuCl *g, size_t nn, const size_t *w,
                                 const void **in, int tl, int nwork,
                                 const int *off, void **ev, cl_uint *nev) {
  cl_int e = RXCL_SUCCESS;
  for (int b = 0; b <= RXG_B_ON && e == RXCL_SUCCESS; b++) {
    if (w[b] == 0) continue;
    e = rxcl.write(g->queue, g->buf[b], 0, 0, w[b]*nn*rxGpuClElt(g, b), in[b], 0,
                   NULL, &ev[*nev]);
    if (e == RXCL_SUCCESS) (*nev)++;
  }
  if (tl && e == RXCL_SUCCESS) {
    e = rxcl.write(g->queue, g->buf[RXG_B_OFF], 0, 0, ((size_t)nwork + 1)*sizeof(int),
                   off, 0, NULL, &ev[*nev]);
    if (e == RXCL_SUCCESS) (*nev)++;
  }
  return e;
}

// queue the reads of y, rd and ri after the launch evk; evr[*nr] receive
// their events
static cl_int rxGpuClQueueReads(rxGpuCl *g, size_t nn, const size_t *w,
                                double *y, double *rd, int *ri, void **evk,
                                void **evr, cl_uint *nr) {
  void *out[3] = {y, rd, ri};
  int ob[3] = {RXG_B_Y, RXG_B_RD, RXG_B_RI};
  cl_int e = RXCL_SUCCESS;
  for (int i = 0; i < 3 && e == RXCL_SUCCESS; i++) {
    int b = ob[i];
    if (w[b] == 0) continue;
    e = rxcl.read(g->queue, g->buf[b], 0, 0, w[b]*nn*rxGpuClElt(g, b), out[i], 1,
                  evk, &evr[*nr]);
    if (e == RXCL_SUCCESS) (*nr)++;
  }
  return e;
}

// wait for the na queued commands all[] (even after an error e: the host
// buffers must not be reused while a transfer still reads or writes them),
// then release them.  Returns e, else the first failure of the wait or of a
// command.
static cl_int rxGpuClWaitAll(rxGpuCl *g, void **all, cl_uint na, cl_int e) {
  cl_int ef = rxcl.finish(g->queue);
  if (ef != RXCL_SUCCESS && na > 0) {
    // the queue-wide wait failed: wait on the commands themselves
    rxcl.waitEvents(na, all);
  }
  if (e == RXCL_SUCCESS) e = ef;
  // a command can fail after it was queued while the queue still finishes
  for (cl_uint i = 0; i < na; i++) {
    cl_int st = RXCL_SUCCESS;
    if (rxcl.eventInfo(all[i], RXCL_EVENT_COMMAND_EXECUTION_STATUS, sizeof(cl_int),
                       &st, NULL) == RXCL_SUCCESS && st < 0 && e == RXCL_SUCCESS) {
      e = st;
    }
  }
  for (cl_uint i = 0; i < na; i++) rxcl.releaseEvent(all[i]);
  return e;
}

// Double precision launch with one wait: the writes, the launch and the reads
// are queued without blocking and ordered by events (the OpenCL package's
// queue may run out of order), then the host waits once.  The inputs stay
// untouched until then (the caller's buffers).  nwork work items over nrow
// request rows; tl 1 also writes the nwork + 1 timeline offsets.
static int rxGpuClRunAsync(rxGpuCl *g, int nwork, int nrow, int tl,
                           const int *off, size_t *w, const void **in,
                           double *y, double *rd, int *ri) {
  size_t nn = (size_t)nrow;
  void *ev[RXG_B_ON + 2], *evk = NULL, *evr[3];
  cl_uint nev = 0, nr = 0;
  cl_int e = rxGpuClQueueWrites(g, nn, w, in, tl, nwork, off, ev, &nev);
  if (e == RXCL_SUCCESS) e = rxGpuClArgs(g, nwork, tl);
  if (e == RXCL_SUCCESS) e = rxGpuClLaunch(g, nwork, nev, ev, &evk);
  if (e == RXCL_SUCCESS) e = rxGpuClQueueReads(g, nn, w, y, rd, ri, &evk, evr, &nr);
  void *all[RXG_B_ON + 6];
  cl_uint na = 0;
  for (cl_uint i = 0; i < nev; i++) all[na++] = ev[i];
  if (evk != NULL) all[na++] = evk;
  for (cl_uint i = 0; i < nr; i++) all[na++] = evr[i];
  return rxGpuClWaitAll(g, all, na, e);
}

// can launches queue their commands on events?  Single precision stages
// every buffer through g->stage, one at a time
static int rxGpuClCanAsync(rxGpuCl *g) {
  return !g->single && rxcl.releaseEvent != NULL && rxcl.waitEvents != NULL &&
    rxcl.eventInfo != NULL;
}

static int rxGpuClRun(rxGpuCl *g, int nwork, int nrow, int tl, const int *off,
                      double *y, double *rd, int *ri, const double *p0,
                      const double *p1, const double *pl, const double *ir,
                      const int *on) {
  size_t w[RXG_B_N], nn = (size_t)nrow;
  rxGpuClWidths(g, w);
  const void *in[RXG_B_ON + 1] = {y, rd, ri, p0, p1, pl, ir, on};
  if (rxGpuClCanAsync(g)) {
    return rxGpuClRunAsync(g, nwork, nrow, tl, off, w, in, y, rd, ri);
  }
  cl_int e = RXCL_SUCCESS;
  for (int b = 0; b <= RXG_B_ON && e == RXCL_SUCCESS; b++) {
    e = rxGpuClWrite(g, b, in[b], w[b]*nn);
  }
  if (tl && e == RXCL_SUCCESS) e = rxGpuClWrite(g, RXG_B_OFF, off, (size_t)nwork + 1);
  if (e == RXCL_SUCCESS) e = rxGpuClArgs(g, nwork, tl);
  if (e != RXCL_SUCCESS) return e;
  e = rxGpuClLaunch(g, nwork, 0, NULL, NULL);
  // the OpenCL package's queue may run out of order
  if (e == RXCL_SUCCESS) e = rxcl.finish(g->queue);
  if (e != RXCL_SUCCESS) return e;
  e = rxGpuClRead(g, RXG_B_Y, y, w[RXG_B_Y]*nn);
  if (e == RXCL_SUCCESS) e = rxGpuClRead(g, RXG_B_RD, rd, w[RXG_B_RD]*nn);
  if (e == RXCL_SUCCESS) e = rxGpuClRead(g, RXG_B_RI, ri, w[RXG_B_RI]*nn);
  return e;
}

// One round: n requests in the layout of rxg_round.h.  y, rd and ri are
// updated in place.  Returns 0, or the OpenCL error code.
int rxGpuClRound(rxGpuCl *g, int n, double *y, double *rd, int *ri,
                 const double *p0, const double *p1, const double *pl,
                 const double *ir, const int *on) {
  if (n <= 0) return 0;
  if (n > g->dims[RXG_D_NSUB]) return -1000;
  return rxGpuClRun(g, n, n, 0, NULL, y, rd, ri, p0, p1, pl, ir, on);
}

// n subjects' timelines in one launch: subject i's rows are off[i] to
// off[i + 1] - 1 of the nrow rows (rxg_timeline1()).  y, rd and ri are
// updated in place.  Returns 0, or the OpenCL error code.
int rxGpuClTimeline(rxGpuCl *g, int n, int nrow, const int *off, double *y,
                    double *rd, int *ri, const double *p0, const double *p1,
                    const double *pl, const double *ir, const int *on) {
  if (n <= 0 || nrow <= 0) return 0;
  int cap = g->dims[RXG_D_NROW] > g->dims[RXG_D_NSUB] ? g->dims[RXG_D_NROW] : g->dims[RXG_D_NSUB];
  if (n > g->dims[RXG_D_NSUB] || nrow > cap) return -1000;
  return rxGpuClRun(g, n, nrow, 1, off, y, rd, ri, p0, p1, pl, ir, on);
}

//////////////////////////////////////////////////////////////////////////////
// R interface: setup, and a round entry for the tests (the solver calls
// rxGpuClRound() directly)

// A run is an external pointer tagged rxGpuCl.  It has no C finalizer (a
// finalizer in this DLL would crash R once the DLL is unloaded); R releases
// it through _rxode2_rxGpuClFree() while rxode2 is loaded (.rxGpuRun()).
static SEXP rxGpuClTag(void) {
  return Rf_install("rxGpuCl");
}

static int rxGpuClIsRun(SEXP p) {
  return TYPEOF(p) == EXTPTRSXP && R_ExternalPtrTag(p) == rxGpuClTag();
}

static rxGpuCl *rxGpuClGet(SEXP p) {
  if (!rxGpuClIsRun(p)) (Rf_error)("invalid OpenCL run");
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

// Global memory and largest single allocation (bytes) of an OpenCL device
// (the OpenCL package's device object); NA when they cannot be read
SEXP _rxode2_rxGpuClDeviceMem(SEXP dev) {
  rxProtectGuard;
  SEXP ret = rxP(Rf_allocVector(REALSXP, 2));
  REAL(ret)[0] = REAL(ret)[1] = NA_REAL;
  if (rxGpuClLoad() && rxcl.deviceInfo != NULL && TYPEOF(dev) == EXTPTRSXP &&
      R_ExternalPtrAddr(dev) != NULL) {
    uint64_t v = 0;
    // CL_DEVICE_GLOBAL_MEM_SIZE, CL_DEVICE_MAX_MEM_ALLOC_SIZE
    if (rxcl.deviceInfo(R_ExternalPtrAddr(dev), 0x101F, sizeof(v), &v, NULL) == RXCL_SUCCESS) {
      REAL(ret)[0] = (double)v;
    }
    v = 0;
    if (rxcl.deviceInfo(R_ExternalPtrAddr(dev), 0x1010, sizeof(v), &v, NULL) == RXCL_SUCCESS) {
      REAL(ret)[1] = (double)v;
    }
  }
  rxUPAll();
  return ret;
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
  // the R object first, so no allocation can fail while the run is live
  // and unowned
  SEXP ret = rxP(R_MakeExternalPtr(NULL, rxGpuClTag(), keep));
  int err = 0;
  rxGpuCl *g = rxGpuClNew(c, q, k, d, REAL(od), INTEGER(oi),
                             Rf_asLogical(single), &err);
  if (g == NULL) {
    rxUPAll();
    switch (err) {
    case 1: (Rf_error)("cannot find the OpenCL library");
    case 2: (Rf_error)("the OpenCL kernel is not an rxode2 round kernel");
    case 3: (Rf_error)("out of memory");
    case 4: (Rf_error)("cannot allocate the OpenCL buffers on the device; lower rxControl(gpuBatch=)");
    default: (Rf_error)("cannot set up the OpenCL buffers (step %d)", err);
    }
  }
  R_SetExternalPtrAddr(ret, g);
  rxUPAll();
  return ret;
}

// is x shorter than nn requests of w values?
static int rxGpuClShort(SEXP x, int nn, int w) {
  return Rf_xlength(x) < (R_xlen_t)nn*w;
}

// do the round's arrays hold nn requests of the run's layout?
static int rxGpuClSizesOk(const int *d, int nn, SEXP y, SEXP rd, SEXP ri,
                          SEXP p0, SEXP p1, SEXP pl, SEXP ir, SEXP on) {
  if (nn < 0 || nn > d[RXG_D_NSUB]) return 0;
  return !(rxGpuClShort(y, nn, d[RXG_D_NEQ1]) ||
           rxGpuClShort(rd, nn, d[RXG_D_RD]) ||
           rxGpuClShort(ri, nn, d[RXG_D_RI]) ||
           rxGpuClShort(p0, nn, d[RXG_D_NPAR1]) ||
           rxGpuClShort(p1, nn, d[RXG_D_NPAR1]) ||
           rxGpuClShort(pl, nn, d[RXG_D_NLHS1]) ||
           rxGpuClShort(ir, nn, d[RXG_D_NEQ1]) ||
           rxGpuClShort(on, nn, d[RXG_D_NEQ1]));
}

// every request names a distinct subject of the run
static void rxGpuClCheckSubjects(const int *d, int nn, SEXP ri) {
  int ns = d[RXG_D_NSUB];
  char *seen = (char *)rxMemRalloc(gpuStage, (size_t)(ns > 0 ? ns : 1), sizeof(char));
  memset(seen, 0, (size_t)(ns > 0 ? ns : 1));
  const int *r = INTEGER(ri);
  for (int i = 0; i < nn; i++) {
    int sb = r[(R_xlen_t)i*d[RXG_D_RI]];
    if (sb < 0 || sb >= ns || seen[sb]) {
      (Rf_error)("request %d names an invalid or repeated subject", i + 1);
    }
    seen[sb] = 1;
  }
}

SEXP _rxode2_rxGpuClRound(SEXP run, SEXP n, SEXP y, SEXP rd, SEXP ri,
                          SEXP p0, SEXP p1, SEXP pl, SEXP ir, SEXP on) {
  rxProtectGuard;
  rxGpuCl *g = rxGpuClGet(run);
  int nn = Rf_asInteger(n);
  const int *d = g->dims;
  if (!rxGpuClSizesOk(d, nn, y, rd, ri, p0, p1, pl, ir, on)) {
    (Rf_error)("the round's requests do not match the run");
  }
  rxGpuClCheckSubjects(d, nn, ri);
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
  if (!rxGpuClIsRun(run)) (Rf_error)("invalid OpenCL run");
  rxGpuCl *g = (rxGpuCl *)R_ExternalPtrAddr(run);
  if (g != NULL) {
    rxGpuClFree(g);
    R_ClearExternalPtr(run);
  }
  return R_NilValue;
}
