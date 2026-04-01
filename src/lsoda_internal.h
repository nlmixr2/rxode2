#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
int stoda(struct lsoda_context_t * ctx, double *y, int jstart);
int      correction(struct lsoda_context_t * ctx, double *y, double pnorm, double *del, double *delp, double told, int *m);
int prja(struct lsoda_context_t * ctx, double *y);

int solsy(struct lsoda_context_t * ctx, double *y);
int      orderswitch(struct lsoda_context_t * ctx, double rhup, double dsm, double *rh, int kflag, int maxord);
int      intdy(struct lsoda_context_t * ctx, double t, int k, double *dky);
int      corfailure(struct lsoda_context_t * ctx, double told);
void     methodswitch(struct lsoda_context_t * ctx, double dsm, double pnorm, double *rh);
void     cfode(struct lsoda_context_t * ctx, int meth);
void     cfode_static(struct lsoda_context_t * ctx, int meth);
void     scaleh(struct lsoda_context_t * ctx, double rh);

char * _strdup_printf(char * fmt, ...);
#ifdef CFODE_STATIC
	#define cfode cfode_static
#endif
#ifdef ERROR
#undef ERROR
#endif
#define ERROR0(fmt, ...) (ctx->error? free(ctx->error):(void)1, ctx->error=_strdup_printf("EE:" fmt " @(%s:%d)", __FILE__, __LINE__))
#ifdef ERROR
#undef ERROR
#endif
#define ERROR(fmt, ...) (ctx->error? free(ctx->error):(void)1, ctx->error=_strdup_printf("EE:" fmt " @(%s:%d)", __VA_ARGS__, __FILE__, __LINE__))
