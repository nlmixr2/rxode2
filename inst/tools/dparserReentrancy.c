/* dparserReentrancy.c -- is dparser's dparse() safe to call from several
 * threads at once?
 *
 * This is a standalone spike, not part of the package build.  It exists
 * because the answer gates threading anything that parses -- src/seFromSE.c
 * and src/rxToSE.c here, and potentially src/tran.c -- and because "it seemed
 * to work" is not an answer worth relying on.
 *
 * ANSWER (dparser 1.31, x86_64 linux, 2026-08): YES, provided each thread
 * builds its OWN D_Parser from the shared read-only parser tables.
 *
 *   plain      8 and 16 threads, 48000 parses total   0 mismatches
 *   TSAN       8 threads, 16000 parses                no data races reported
 *   ASAN       8 threads, 16000 parses                no memory errors
 *   repeated   12 threads x3                          0 mismatches
 *
 * Supporting evidence: the shipped dparser.so uses plain malloc rather than a
 * pooled or GC allocator, and its only non-table writable globals
 * (__curP, __pn, __buf) are unreferenced from the parse path.
 *
 * What this does NOT license: sharing one D_Parser between threads, or
 * calling the R API / symengine from inside a parallel region.  symengine in
 * particular is built here WITHOUT thread-safe refcounting.
 *
 * Build and run:
 *   DP=$(Rscript -e 'cat(file.path(find.package("dparser"),"include"))')
 *   tail -n +4 src/seFromSE.g.d_parser.h > /tmp/tables.h   # drop the R headers
 *   gcc -O2 -I"$DP" -I/tmp -o /tmp/reent inst/tools/dparserReentrancy.c \
 *       -include /tmp/tables.h -ldl -lpthread
 *   /tmp/reent "$(Rscript -e 'cat(find.package("dparser"))')/libs/dparser.so" 8
 *
 * Under TSAN add `setarch $(uname -m) -R` -- it otherwise dies with
 * "unexpected memory mapping".
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <dlfcn.h>
#include "dparse.h"

static D_Parser *(*p_new)(D_ParserTables*, int);
static void (*p_free)(D_Parser*);
static D_ParseNode *(*p_dparse)(D_Parser*, char*, int);
static void (*p_freeNode)(D_Parser*, D_ParseNode*);
static int (*p_nchild)(D_ParseNode*);
static D_ParseNode *(*p_child)(D_ParseNode*, int);

extern D_ParserTables parser_tables_rxode2seFromSE;

static const char *INPUTS[] = {
  "a*b + c/d - e", "exp(lcl - lvc)*cent", "Rx_pow_di(a,2)+sqrt(b)",
  "rx__d_dt_depot__", "THETA_1_ + ETA_2_*rx__sens_cent_BY_lka__",
  "(a+b)*(c-d)/(e+f)", "log(2)*pi/4", "atan2(y, x) + erf(z)",
  "exp(-0.4 - 4.0*exp(-t)*(x - y) + 3.0*(1.38 + log(x - y)))",
  "rx__df_center_dy_cl__*rx__sens_center_BY_eta.ka__"
};
#define NIN ((int)(sizeof(INPUTS)/sizeof(INPUTS[0])))

static long countNodes(D_ParseNode *pn) {
  long n = 1; int i, k = p_nchild(pn);
  for (i = 0; i < k; i++) n += countNodes(p_child(pn, i));
  return n;
}

static long parseCount(const char *s) {
  D_Parser *p = p_new(&parser_tables_rxode2seFromSE, sizeof(D_ParseNode_User));
  if (!p) return -1;
  p->save_parse_tree = 1; p->error_recovery = 0;
  D_ParseNode *pn = p_dparse(p, (char*)s, (int)strlen(s));
  long c = -1;
  if (pn && p->syntax_errors == 0) c = countNodes(pn);
  if (pn) p_freeNode(p, pn);
  p_free(p);
  return c;
}

static long expected[NIN];
static int iters = 2000;
static volatile int failures = 0;

static void *worker(void *arg) {
  long id = (long) arg;
  for (int it = 0; it < iters; it++) {
    int k = (int)((id + it) % NIN);
    long c = parseCount(INPUTS[k]);
    if (c != expected[k]) { __sync_fetch_and_add(&failures, 1); }
  }
  return NULL;
}

int main(int argc, char **argv) {
  void *h = dlopen(argv[1], RTLD_NOW | RTLD_GLOBAL);
  if (!h) { printf("dlopen failed: %s\n", dlerror()); return 1; }
  p_new = dlsym(h, "new_D_Parser"); p_free = dlsym(h, "free_D_Parser");
  p_dparse = dlsym(h, "dparse"); p_freeNode = dlsym(h, "free_D_ParseNode");
  p_nchild = dlsym(h, "d_get_number_of_children"); p_child = dlsym(h, "d_get_child");
  if (!p_new || !p_dparse) { printf("dlsym failed\n"); return 1; }

  for (int i = 0; i < NIN; i++) {
    expected[i] = parseCount(INPUTS[i]);
    if (expected[i] < 0) { printf("serial parse FAILED for input %d\n", i); return 1; }
  }
  printf("serial baseline OK (node counts:");
  for (int i = 0; i < NIN; i++) printf(" %ld", expected[i]);
  printf(")\n");

  int nthr = (argc > 2) ? atoi(argv[2]) : 8;
  pthread_t th[64];
  for (long i = 0; i < nthr; i++) pthread_create(&th[i], NULL, worker, (void*)i);
  for (int i = 0; i < nthr; i++) pthread_join(th[i], NULL);
  printf("threads=%d iters/thread=%d total parses=%d  MISMATCHES=%d\n",
         nthr, iters, nthr * iters, failures);
  return failures != 0;
}
