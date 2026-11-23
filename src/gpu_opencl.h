#ifndef RXODE2_GPU_OPENCL_H
#define RXODE2_GPU_OPENCL_H
// OpenCL rounds of the device integrators (src/gpu_opencl.c)

// dims[] passed to rxGpuClNew(): the round layout of rxg_round.h for one
// model, as rxg_host_dims() reports it, and the run size
enum {
  RXG_D_NSUB,  // most requests per round (subjects)
  RXG_D_NEQ1,
  RXG_D_NPAR1,
  RXG_D_NLHS1,
  RXG_D_DELAY, // the model calls delay()
  RXG_D_HREC,  // values per history record
  RXG_D_HCAP,  // history records per subject
  RXG_D_HHDR,
  RXG_D_RD,
  RXG_D_RI,
  RXG_D_AD,
  RXG_D_AI,
  RXG_D_OD,
  RXG_D_OI,
  RXG_D_NROW,  // request rows of a timeline launch (>= RXG_D_NSUB); 0: no timelines
  RXG_D_N
};

typedef struct rxGpuCl {
  void *ctx, *queue, *kernel; // owned by the OpenCL R package
  void *buf[15];              // cl_mem, in kernel argument order
  void *stage;                // float conversion (single precision)
  int single;
  int dims[RXG_D_N];
} rxGpuCl;

#ifdef __cplusplus
extern "C" {
#endif
int rxGpuClLoad(void);
rxGpuCl *rxGpuClNew(void *ctx, void *queue, void *kernel, const int *dims,
                    const double *od, const int *oi, int single, int *err);
int rxGpuClRound(rxGpuCl *g, int n, double *y, double *rd, int *ri,
                 const double *p0, const double *p1, const double *pl,
                 const double *ir, const int *on);
int rxGpuClTimeline(rxGpuCl *g, int n, int nrow, const int *off, double *y,
                    double *rd, int *ri, const double *p0, const double *p1,
                    const double *pl, const double *ir, const int *on);
void rxGpuClFree(rxGpuCl *g);
void rxGpuClBytes(const int *dims, int single, double *out);
void rxGpuHostBytes(const int *d, int opencl, int threads, double *out);
#ifdef __cplusplus
}
#endif

#endif
