#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#include <R.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <algorithm>
#include "../inst/include/rxode2.h"
#define min2( a , b )  ( (a) < (b) ? (a) : (b) )

#include <cstdint>
#include <cerrno>
#include <ctype.h>     // isspace

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif


// Much of this comes from data.table, with references to where it came from

// https://github.com/Rdatatable/data.table/blob/752012f577f8e268bb6d0084ca39a09fa7fbc1c4/src/openmp-utils.c#L12-L27

static int rxThreads=0;
static int rxThrottle=0;


static int getIntEnv(const char *name, int def)
{
  const char *val = getenv(name);
  if (val==NULL) return def;
  size_t nchar = strlen(val);
  if (nchar==0) return def;
  char *end;
  errno = 0;
  long int ans = strtol(val, &end, 10);  // ignores leading whitespace. If it fully consumed the string, *end=='\0' and isspace('\0')==false
  while (isspace(*end)) end++;  // ignore trailing whitespace
  if (errno || (size_t)(end-val)!=nchar || ans<1 || ans>INT_MAX) {
    Rf_warningcall(R_NilValue,
		   _("ignoring invalid %s==\"%s\"\n not an integer >= 1\nremove any characters that are not a digit [0-9]\n See ?rxode2::setDTthreads"), name, val);
    return def;
  }
  return (int)ans;
}

// See https://github.com/Rdatatable/data.table/blob/752012f577f8e268bb6d0084ca39a09fa7fbc1c4/src/openmp-utils.c for the code
// modified to use rxode2 environmental variables and to tune based on rxode2 variables
// Also uses Rf_errorcall and Rf_warnngcall
static inline int imin(int a, int b) { return a < b ? a : b; }
static inline int imax(int a, int b) { return a > b ? a : b; }


extern "C" void initRxThreads() {
  // called at package startup from init.c
  // also called by setDTthreads(threads=NULL) (default) to reread environment variables; see setDTthreads below
  // No verbosity here in this setter. Verbosity is in getRxThreads(verbose=TRUE)
  int ans = getIntEnv("rxode2_NUM_THREADS", INT_MIN);
  if (ans>=1) {
    ans = imin(ans, omp_get_num_procs());  // num_procs is a hard limit; user cannot achieve more. ifndef _OPENMP then myomp.h defines this to be 1
  } else {
    // Only when R_DATATABLE_NUM_THREADS is unset (or <=0) do we use PROCS_PERCENT; #4514
    int perc = getIntEnv("rxode2_NUM_PROCS_PERCENT", 50); // use "NUM_PROCS" to use the same name as the OpenMP function this uses
    // 50% of logical CPUs by default; half of 8 is 4 on laptop with 4 cores. Leaves plenty of room for other processes
    if (perc<=1 || perc>100) {
      Rf_warningcall(R_NilValue,
		     _("ignoring invalid rxode2_NUM_PROCS_PERCENT==%d.\nIf used it must be an integer between 2 and 100. Default is 50. See ?rxSetThreads"),
		     perc);
      // not allowing 1 is to catch attempts to use 1 or 1.0 to represent 100%.
      perc = 50;
    }
    ans = imax(omp_get_num_procs()*perc/100, 1); // imax for when formula would result in 0.
  }
  ans = imin(ans, omp_get_thread_limit());  // honors OMP_THREAD_LIMIT when OpenMP started; e.g. CRAN sets this to 2. Often INT_MAX meaning unlimited/unset
  ans = imin(ans, omp_get_max_threads());   // honors OMP_NUM_THREADS when OpenMP started, plus reflects any omp_set_* calls made since
  // max_threads() -vs- num_procs(): https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/302866
  ans = imin(ans, getIntEnv("OMP_THREAD_LIMIT", INT_MAX));  // user might expect `Sys.setenv(OMP_THREAD_LIMIT=2);rxSetThreads()` to work. Satisfy this
  ans = imin(ans, getIntEnv("OMP_NUM_THREADS", INT_MAX));   //   expectation by reading them again now. OpenMP just reads them on startup (quite reasonably)
  ans = imax(ans, 1);  // just in case omp_get_* returned <=0 for any reason, or the env variables above are set <=0
  rxThreads = ans;
  rxThrottle = imax(1, getIntEnv("rxode2_THROTTLE", 2)); // 2nd thread is used only when nid>2, 3rd thread when n>4, etc
}

static const char *mygetenv(const char *name, const char *unset) {
  const char *ans = getenv(name);
  return (ans==NULL || ans[0]=='\0') ? unset : ans;
}

extern "C" int getRxThreads(const int64_t n, const bool throttle) {
  // this is the main getter used by all parallel regions; they specify num_threads(n, true|false).
  // Keep this light, simple and robust. rxSetThreads() ensures 1 <= rxThreads <= omp_get_num_proc()
  // throttle introduced in 1.12.10 (see NEWS item); #4484
  // throttle==true  : a number of iterations per thread (rxThrottle) is applied before a second thread is utilized 
  // throttle==false : parallel region is already pre-chunked such as in fread; e.g. two batches intended for two threads
  if (n<1) return 1; // 0 or negative could be deliberate in calling code for edge cases where loop is not intended to run at all
  int64_t ans = throttle ? 1+(n-1)/rxThrottle :  // 1 thread for n<=2, 2 thread for n<=4, etc
    n;                    // don't use 20 threads for just one or two batches
  return ans >= rxThreads ? rxThreads : (int)ans;
}

extern "C" SEXP getRxThreads_R(SEXP verbose) {
  if (!isLogical(verbose) || LENGTH(verbose)!=1 || INTEGER(verbose)[0]==NA_LOGICAL)
    Rf_errorcall(R_NilValue, _("'verbose' must be TRUE or FALSE"));
  if (LOGICAL(verbose)[0]) {
#ifndef _OPENMP
    Rprintf(_("This installation of data.table has not been compiled with OpenMP support.\n"));
#endif
    // this output is captured, paste0(collapse="; ")'d, and placed at the end of test.data.table() for display in the last 13 lines of CRAN check logs
    // it is also printed at the start of test.data.table() so that we can trace any Killed events on CRAN before the end is reached
    // this is printed verbatim (e.g. without using data.table to format the output) in case there is a problem even with simple data.table creation/printing
    Rprintf(_("  omp_get_num_procs()            %d\n"), omp_get_num_procs());
    Rprintf(_("  rxode2_NUM_PROCS_PERCENT  %s\n"), mygetenv("rxode2_NUM_PROCS_PERCENT", "unset (default 50)"));
    Rprintf(_("  rxode2_NUM_THREADS        %s\n"), mygetenv("rxode2_NUM_THREADS", "unset"));
    Rprintf(_("  rxode2_THROTTLE           %s\n"), mygetenv("rxode2_THROTTLE", "unset (default 2)"));
    Rprintf(_("  omp_get_thread_limit()         %d\n"), omp_get_thread_limit());
    Rprintf(_("  omp_get_max_threads()          %d\n"), omp_get_max_threads());
    Rprintf(_("  OMP_THREAD_LIMIT               %s\n"), mygetenv("OMP_THREAD_LIMIT", "unset"));  // CRAN sets to 2
    Rprintf(_("  OMP_NUM_THREADS                %s\n"), mygetenv("OMP_NUM_THREADS", "unset"));
    /* Rprintf(_("  RestoreAfterFork               %s\n"), RestoreAfterFork ? "true" : "false"); */
    Rprintf(_("  rxode2 is using %d threads with throttle==%d. See ?setRxthreads.\n"), getRxThreads(INT_MAX, false), rxThrottle);
  }
  return ScalarInteger(getRxThreads(INT_MAX, false));
}

extern "C" SEXP setRxthreads(SEXP threads, SEXP percent, SEXP throttle) {
  if (length(throttle)) {
    if (!isInteger(throttle) || LENGTH(throttle)!=1 || INTEGER(throttle)[0]<1)
      error(_("'throttle' must be a single number, non-NA, and >=1"));
    rxThrottle = INTEGER(throttle)[0];
  }
  int old = rxThreads;
  if (!length(threads) && !length(throttle)) {
    initRxThreads();
    // Rerun exactly the same function used on startup (re-reads env variables); this is now default setDTthreads() behavior from 1.12.2
    // Allows robust testing of environment variables using Sys.setenv() to experiment.
    // Default  is now (as from 1.12.2) threads=NULL which re-reads environment variables.
    // If a CPU has been unplugged (high end servers allow live hardware replacement) then omp_get_num_procs() will
    // reflect that and a call to setDTthreads(threads=NULL) will update DTthreads.
  } else if (length(threads)) {
    int n=0;
    if (length(threads)!=1 || !isInteger(threads) || (n=INTEGER(threads)[0]) < 0) {  // <0 catches NA too since NA is negative (INT_MIN)
      Rf_errorcall(R_NilValue, _("threads= must be either NULL or a single number >= 0 See ?setRxthreads"));
    }
    int num_procs = imax(omp_get_num_procs(), 1); // max just in case omp_get_num_procs() returns <= 0 (perhaps error, or unsupported)
    if (!isLogical(percent) || length(percent)!=1 || LOGICAL(percent)[0]==NA_LOGICAL) {
      Rf_errorcall(R_NilValue, _("internal error: percent= must be TRUE or FALSE at C level"));  // # nocov
    }
    if (LOGICAL(percent)[0]) {
      if (n<2 || n>100) error(_("internal error: threads==%d should be between 2 and 100 (percent=TRUE at C level)"), n);  // # nocov
      n = num_procs*n/100;  // if 0 it will be reset to 1 in the imax() below
    } else {
      if (n==0 || n>num_procs) n = num_procs; // setRxThreads(0) == setRxThreads(percent=100); i.e. use all logical CPUs (the default in 1.12.0 and before, from 1.12.2 it's 50%)
    }
    n = imin(n, omp_get_thread_limit());  // can't think why this might be different from its value on startup, but call it just in case
    n = imin(n, getIntEnv("OMP_THREAD_LIMIT", INT_MAX));  // user might have called Sys.setenv(OMP_THREAD_LIMIT=) since startup and expect setRxThreads to respect it
    rxThreads = imax(n, 1);  // imax just in case
    // Do not call omp_set_num_threads() here. Any calls to omp_set_num_threads() affect other
    // packages and R itself too which has some OpenMP usage. Instead we set our own DTthreads
    // static variable and read that from getDTthreads(n, throttle).
    // All parallel regions should include num_threads(getDTthreads(n, true|false)) and this is ensured via
    // a grep in CRAN_Release.cmd.
  }
  return ScalarInteger(old);
}


// Like data.table throttle threads to 1 when forked.
static int pre_fork_rxThreads = 0;

extern "C" void when_fork() {
  pre_fork_rxThreads = rxThreads;
  rxThreads = 1;
}

extern "C" void after_fork() {
  rxThreads = pre_fork_rxThreads;
}

extern "C" void avoid_openmp_hang_within_fork() {
  // Called once on loading rxode2 from init.c
#ifdef _OPENMP
  pthread_atfork(&when_fork, &after_fork, NULL);
#endif
}

// Calculate nradix
extern "C" void calcNradix(int *nbyte, int *nradix, int *spare, uint64_t *maxD, uint64_t *minD){
  // Get the data range required for radix sort
  // This is adapted from forder:
  ////////////////////////////////////////////////////////////////////////////////
  // https://github.com/Rdatatable/data.table/blob/master/src/forder.c
  // in data.table keyAlloc=(ncol+n_cplx)*8+1 which translates to 9
  // Since the key is constant we can pre-allocate with stack instead key[9]
  // NA, NaN, and -Inf +Inf not supported
  uint64_t range = *maxD - *minD;// +1/*NA*/ +isReal*3/*NaN, -Inf, +Inf*/;
  int maxBit=0;
  while (range) { maxBit++; range>>=1; }
  *nbyte = 1+(maxBit-1)/8; // the number of bytes spanned by the value
  int firstBits = maxBit - (*nbyte-1)*8;  // how many bits used in most significant byte
  // There is only One split since we are only ordering time
  *spare = 8-firstBits; // left align to byte boundary to get better first split.
  // In forder they allocate the nradix(which is 0) + nbyte
  // Therefore we allocate nrow*nbyte uint8_t for the key pointers
  // This equates to n_all_times * nbyte int8_t
  // However you can also allocate just enough memory for sort based on threads
  // The key hash would be (max_n_all_times_id)*ncores*nbyte
  //
  // Radix sort memory need become http://itu.dk/people/pagh/ads11/11-RadixSortAndSearch.pdf which would be
  // N=n_all_times+R;  r = size of the "alphabet" which in this case would be the number of bytes
  // For the memory complexity it becomes n_all_times * nbyte

  // After allocating and dtwiddle threads nradix = nbyte-1 + (spare==0)
  // End borrowing and commenting from data.table
  *nradix = *nbyte-1 + (spare==0);
}


// For rxode2 sortType = 1
// FIXME key per thread

static int dround=0; // No rounding by default
static uint64_t dmask=0;
// Original comes from:
// https://github.com/Rdatatable/data.table/blob/588e0725320eacc5d8fc296ee9da4967cee198af/src/forder.c#L396-L414
// for signed integers it's easy: flip sign bit to swap positives and negatives; the resulting unsigned is in the right order with INT_MIN ending up as 0
// for floating point finite you have to flip the other bits too if it was signed: http://stereopsis.com/radix.html
// CHANGES for rxode2:
// By definition in rxode2, the value is always finite, drop the NA, NaN, and +-Inf
extern "C" uint64_t dtwiddle(const void *p, int i)
{
  union {
    double d;
    uint64_t u64;
  } u;  // local for thread safety
  u.d = ((double *)p)[i];
  if (u.d==0) u.d=0; // changes -0.0 to 0.0,  issue #743
  u.u64 ^= (u.u64 & 0x8000000000000000) ? 0xffffffffffffffff : 0x8000000000000000; // always flip sign bit and if negative (sign bit was set) flip other bits too
  u.u64 += (u.u64 & dmask) << 1/*is this shift really correct. No need to shift*/  ;   // when dround==1|2, if 8th|16th bit is set, round up before chopping last 1|2 bytes
  return u.u64 >> (dround*8);
}

// Note range_d will be calculated in rxData while loading into the parallel R interface instead.

// Adapted from:
// https://github.com/Rdatatable/data.table/blob/588e0725320eacc5d8fc296ee9da4967cee198af/src/forder.c#L824-L1258
static bool sort_ugrp(uint8_t *x, const int n)
// x contains n unique bytes; sort them in-place using insert sort
// always ascending. desc and nalast are done in WRITE_KEY because columns may cross byte boundaries
// maximum value for n is 256 which is one too big for uint8_t, hence int n
{
  bool skip = true;            // was x already sorted? if so, the caller can skip reordering
  for (int i=1; i<n; i++) {
    uint8_t tmp = x[i];
    if (tmp>x[i-1]) continue;  // x[i-1]==x[i] doesn't happen because x is unique
    skip = false;
    int j = i-1;
    do {
      x[j+1] = x[j];
    } while (--j>=0 && tmp<x[j]);
    x[j+1] = tmp;
  }
  return skip;
}
// Modified because:
//  - sortType=1 always for rxode2 (ascending)
//  - retgrp = 0 (all pushs are meaningless)
//  - Keys are stored based on core
//  - modified so that radix_r is 0 order not 1 order like R (since we are using it in C)
extern "C" void radix_r(const int from, const int to, const int radix,
	     rx_solving_options_ind *ind, rx_solve *rx) {
  uint8_t **key = rx->keys[0];
  int nradix = rx->nradix[0];
  int *anso = ind->ix;
  const int my_n = to-from+1;
  if (my_n==1) {  // minor TODO: batch up the 1's instead in caller (and that's only needed when retgrp anyway)
    return;
  }
  else if (my_n<=256) {
    // if nth==1
    // Rprintf(_("insert clause: radix=%d, my_n=%d, from=%d, to=%d\n"), radix, my_n, from, to);
    // insert sort with some twists:
    // i) detects if grouped; if sortType==0 can then skip
    // ii) keeps group appearance order at byte level to minimize movement
    // The key cannot be restricted because CRAN requires forder to use Cpp and restrict is a C only keyword
    // (ie any openmp needs to be in C++ since the c++ linker is used with the openmp options)
    uint8_t *my_key = key[radix]+from;  // safe to write as we don't use this radix again
    uint8_t *o = (uint8_t *)malloc(my_n*sizeof(uint8_t));
    // if last key (i.e. radix+1==nradix) there are no more keys to reorder so we could reorder osub by reference directly and save allocating and populating o just
    // to use it once. However, o's type is uint8_t so many moves within this max-256 vector should be faster than many moves in osub (4 byte or 8 byte ints) [1 byte
    // type is always aligned]
    bool skip = true;
    // Ascending; (sortType=1)
    // Skipped https://github.com/Rdatatable/data.table/blob/588e0725320eacc5d8fc296ee9da4967cee198af/src/forder.c#L869-L908 because sortType=1
    int start = 1;
    while (start<my_n && my_key[start]>=my_key[start-1]) {
      start++;
    }
    if (start<my_n) {
      skip = false;  // finding start is really just to take skip out of the loop below
      for (int i=0; i<start; i++) o[i]=i;  // always at least sets o[0]=0
      for (int i=start; i<my_n; i++) {
	uint8_t ktmp = my_key[i];
	int j=i-1;
	while (j>=0 && ktmp<my_key[j]) {
	  my_key[j+1] = my_key[j];
	  o[j+1] = o[j];
	  j--;
	}
	my_key[j+1] = ktmp;  // redundant write when while() did nothing, but that's unlikely given the common case of pre-ordered is handled by skip==true
	o[j+1] = i;          // important to initialize o[] even when while() did nothing.
      }
    }
    if (!skip) {
      // reorder osub and each remaining ksub
      int *TMP = (int*)malloc(my_n*sizeof(int)); // on stack fine since my_n is very small (<=256)
      const int *osub = anso+from;
      for (int i=0; i<my_n; i++) TMP[i] = osub[o[i]];
      memcpy((int *)(anso+from), TMP, my_n*sizeof(int));
      for (int r=radix+1; r< nradix; r++) {
	const uint8_t *ksub = key[r]+from;
	for (int i=0; i<my_n; i++) ((uint8_t *)TMP)[i] = ksub[o[i]];
	memcpy((uint8_t *)(key[r]+from), (uint8_t *)TMP, my_n);
      }
      free(TMP);
    }
    // my_key is now grouped (and sorted by group too if sort!=0)
    // all we have left to do is find the group sizes and either recurse or push
    if (radix+1==nradix) {
      free(o);
      return;
    }
    int ngrp=0;
    int *my_gs = (int *)malloc(my_n*sizeof(int)); //minor TODO: could know number of groups with certainty up above
    my_gs[ngrp]=1;
    for (int i=1; i<my_n; i++) {
      if (my_key[i]!=my_key[i-1]) my_gs[++ngrp] = 1;
      else my_gs[ngrp]++;
    }
    ngrp++;
    if (radix+1==nradix || ngrp==my_n) {  // ngrp==my_n => unique groups all size 1 and we can stop recursing now
    } else {
      for (int i=0, f=from; i<ngrp; i++) {
	radix_r(f, f+my_gs[i]-1, radix+1, ind, rx);
	f+=my_gs[i];
      }
    }
    free(my_gs);
    free(o);
    return;
  }
  else if (my_n<=UINT16_MAX) {    // UINT16_MAX==65535 (important not 65536)
    // if (nth==1) Rprintf(_("counting clause: radix=%d, my_n=%d\n"), radix, my_n);
    uint16_t my_counts[256];// = {0};  // Needs to be all-0 on entry. This ={0} initialization should be fast as it's on stack. Otherwise, we have to manage
    // a stack of counts anyway since this is called recursively and these counts are needed to make the recursive calls.
    // This thread-private stack alloc has no chance of false sharing and gives omp and compiler best chance.
    // rxode2 change use std::fill_n();  In my experience sometimes = {0}; doesn't always fill 0.
    std::fill_n(my_counts, 256, 0);
    //uint8_t * my_ugrp = rx->UGRP + 0*256;  // uninitialized is fine; will use the first ngrp items. Only used if sortType==0
    // TODO: ensure my_counts, my_grp and my_tmp below are cache line aligned on both Linux and Windows.
    const uint8_t * my_key = key[radix]+from;
    int ngrp = 0;          // number of groups (items in ugrp[]). Max value 256 but could be uint8_t later perhaps if 0 is understood as 1.
    bool skip = true;      // i) if already _grouped_ and sortType==0 then caller can skip, ii) if already _grouped and sorted__ when sort!=0 then caller can skip too
    // sortType = 1, keep https://github.com/Rdatatable/data.table/blob/be6c1fc66a411211c4ca944702c1cab7739445f3/src/forder.c#L956-L964
    for (int i=0; i<my_n; ++i) my_counts[my_key[i]]++;  // minimal branch-free loop first, #3647
    for (int i=1; i<my_n; ++i) {
      if (my_key[i]<my_key[i-1]) { skip=false; break; }
      // stop early as soon as not-ordered is detected; likely quickly when it isn't sorted
      // otherwise, it's worth checking if it is ordered because skip saves time later
    }
    // skip https://github.com/Rdatatable/data.table/blob/588e0725320eacc5d8fc296ee9da4967cee198af/src/forder.c#L965-L978
    if (!skip) {
      // reorder anso and remaining radix keys

      // avoid allocating and populating order vector (my_n long); we just use counts several times to push rather than pull
      // with contiguous-read from osub and ksub, 256 write live cache-lines is worst case. However, often there are many fewer ugrp and only that number of
      // write cache lines will be active. These write-cache lines will be constrained within the UINT16_MAX width, so should be close by in cache, too.
      // If there is a good degree of grouping, there contiguous-read/write both ways happens automatically in this approach.

      // cumulate; for forwards-assign to give cpu prefetch best chance (cpu may not support prefetch backwards).
      uint16_t my_starts[256], my_starts_copy[256];
      // TODO: could be allocated up front (like my_TMP below), or are they better on stack like this? TODO: allocating up front would provide to cache-align them.
      // Use https://github.com/Rdatatable/data.table/blob/be6c1fc66a411211c4ca944702c1cab7739445f3/src/forder.c#L991
      // since sortType=1
      for (int i=0, sum=0; i<256; i++) { int tmp=my_counts[i]; my_starts[i]=my_starts_copy[i]=sum; sum+=tmp; ngrp+=(tmp>0);}  // cumulate through 0's too (won't be used)
      int * my_TMP = rx->TMP; // Allocated up front to save malloc calls which i) block internally and ii) could fail
      // Modified nalast not used since NAs cannot occur for a good sort of times
      if (radix==0) {
	// anso contains 1:n so skip reading and copying it. Only happens when nrow<65535. Saving worth the branch (untested) when user repeatedly calls a small-n small-cardinality order.
	// rxode2: change 0 instead of +1 because C is 0-based
	for (int i=0; i<my_n; i++) anso[my_starts[my_key[i]]++] = i;  // +1 as R is 1-based.
	// The loop counter could be uint_fast16_t since max i here will be UINT16_MAX-1 (65534), hence ++ after last iteration won't overflow 16bits. However, have chosen signed
	// integer for counters for now, as signed probably very slightly faster than unsigned on most platforms from what I can gather.
      } else {
        const int *osub = anso+from;
        for (int i=0; i<my_n; i++) my_TMP[my_starts[my_key[i]]++] = osub[i];
        memcpy(anso+from, my_TMP, my_n*sizeof(int));
      }
      // reorder remaining key columns (radix+1 onwards).   This could be done in one-step too (a single pass through x[],  with a larger TMP
      //    that's how its done in the batched approach below.  Which is better?  The way here is multiple (but contiguous) passes through (one-byte) my_key
      if (radix+1<nradix) {
	for (int r=radix+1; r<nradix; r++) {
	  memcpy(my_starts, my_starts_copy, 256*sizeof(uint16_t));  // restore starting offsets
	  //for (int i=0,last=0; i<256; i++) { int tmp=my_counts[i]; if (tmp==0) continue; my_counts[i]=last; last=tmp; }  // rewind ++'s to offsets
	  const uint8_t * ksub = key[r]+from;
	  for (int i=0; i<my_n; i++) ((uint8_t *)my_TMP)[my_starts[my_key[i]]++] = ksub[i];
	  memcpy(key[r]+from, my_TMP, my_n);
	}
      }
    }

    if (radix+1== nradix) {
      return;  // we're done. avoid allocating and populating very last group sizes for last key
    }
    // int *my_gs = new int[ngrp==0 ? 256 : ngrp]; 
    int *my_gs = (int *)malloc((ngrp==0 ? 256 : ngrp)*sizeof(int)); // ngrp==0 when sort and skip==true; we didn't count the non-zeros in my_counts yet in that case
    // Use https://github.com/Rdatatable/data.table/blob/be6c1fc66a411211c4ca944702c1cab7739445f3/src/forder.c#L1028-L1029
    // sortType = 1
    ngrp=0;
    for (int i=0; i<256; i++) if (my_counts[i]) my_gs[ngrp++]=my_counts[i];  // this casts from uint16_t to int32, too
    if (radix+1==nradix) {
      // aside: cannot be all size 1 (a saving used in my_n<=256 case above) because my_n>256 and ngrp<=256
    } else {
      // this single thread will now descend and resolve all groups, now that the groups are close in cache
      for (int i=0, my_from=from; i<ngrp; i++) {
	radix_r(my_from, my_from+my_gs[i]-1, radix+1, ind, rx);
	my_from+=my_gs[i];
      }
    }
    free(my_gs);
    return;
  }
  // else parallel batches. This is called recursively but only once or maybe twice before resolving to UINT16_MAX branch above

  int batchSize = min2(UINT16_MAX, 1+my_n);  // (my_n-1)/nBatch + 1;   //UINT16_MAX == 65535
  int nBatch = (my_n-1)/batchSize + 1;   // TODO: make nBatch a multiple of nThreads?
  int lastBatchSize = my_n - (nBatch-1)*batchSize;
  uint16_t *counts = (uint16_t *)calloc(nBatch*256,sizeof(uint16_t));
  uint8_t  *ugrps =  (uint8_t *)malloc(nBatch*256*sizeof(uint8_t));
  int      *ngrps =  (int *)calloc(nBatch    ,sizeof(int));
  /* if (!counts || !ugrps || !ngrps) STOP(_("Failed to allocate parallel counts. my_n=%d, nBatch=%d"), my_n, nBatch); */

  bool skip=true;
  const int n_rem = nradix-radix-1;   // how many radix are remaining after this one
  // Since this should also be run on each thread (instead of using threads to speed up sorting), skip the pragma omp
  // here https://github.com/Rdatatable/data.table/blob/be6c1fc66a411211c4ca944702c1cab7739445f3/src/forder.c#L1059
  {
    int     *my_otmp = (int*)malloc(batchSize * sizeof(int)); // thread-private write
    uint8_t *my_ktmp = (uint8_t*)malloc(batchSize * sizeof(uint8_t) * n_rem);
    // TODO: move these up above and point restrict[me] to them. Easier to Error that way if failed to alloc.
    // Skip the omp parallel for
    for (int batch=0; batch<nBatch; batch++) {
      const int my_n = (batch==nBatch-1) ? lastBatchSize : batchSize;  // lastBatchSize == batchSize when my_n is a multiple of batchSize
      const int my_from = from + batch*batchSize;
      // restrict doesn't work in C++ (which is required here because
      // of C++ OpenMp requirement from CRAN only using one OpenMP
      // language...)
      uint16_t *my_counts = counts + batch*256;
      uint8_t  *my_ugrp   = ugrps  + batch*256;
      int                     my_ngrp   = 0;
      bool                    my_skip   = true;
      const uint8_t * my_key    = key[radix] + my_from;
      const uint8_t * byte = my_key;
      for (int i=0; i<my_n; i++, byte++) {
	if (++my_counts[*byte]==1) {   // always true first time when i==0
	  my_ugrp[my_ngrp++] = *byte;
	} else if (my_skip && byte[0]!=byte[-1]) {   // include 'my_skip &&' to save != comparison after it's realized this batch is not grouped
	  my_skip=false;
	}
      }
      ngrps[batch] = my_ngrp;  // write once to this shared cache line
      if (!my_skip) {
	skip = false;          // naked write to this shared byte is ok because false is only value written
	// gather this batch's anso and remaining keys. If we sorting too, urgrp is sorted later for that. Here we want to benefit from skip within batch
	// as much as possible which is a good chance since batchSize is relatively small (65535)
	for (int i=0, sum=0; i<my_ngrp; i++) { int tmp = my_counts[my_ugrp[i]]; my_counts[my_ugrp[i]]=sum; sum+=tmp; } // cumulate counts of this batch
	const int * osub = anso+my_from;
	byte = my_key;
	for (int i=0; i<my_n; i++, byte++) {
	  int dest = my_counts[*byte]++;
	  my_otmp[dest] = *osub++;  // wastefully copies out 1:n when radix==0, but do not optimize as unlikely worth code complexity. my_otmp is not large, for example. Use first TEND() to decide.
	  for (int r=0; r<n_rem; r++) my_ktmp[r*my_n + dest] = key[radix+1+r][my_from+i];   // reorder remaining keys
	}
	// or could do multiple passes through my_key like in the my_n<=65535 approach above. Test which is better depending on if TEND() points here.

	// we haven't completed all batches, so we don't know where these groups should place yet
	// So for now we write the thread-private small now-grouped buffers back in-place. The counts and groups across all batches will be used below to move these blocks.
	memcpy(anso+my_from, my_otmp, my_n*sizeof(int));
	for (int r=0; r<n_rem; r++) memcpy(key[radix+1+r]+my_from, my_ktmp+r*my_n, my_n*sizeof(uint8_t));

	// revert cumulate back to counts ready for vertical cumulate
	for (int i=0, last=0; i<my_ngrp; i++) { int tmp = my_counts[my_ugrp[i]]; my_counts[my_ugrp[i]]-=last; last=tmp; }
      }
    }
    free(my_otmp);
    free(my_ktmp);
  }

  // If my_n input is grouped and ugrp is sorted too (to illustrate), status now would be :
  // counts:                 ugrps:   ngrps:
  // 1: 20 18  2  0  0  0    0 1 2    3
  // 2:  0  0 17 21  5  0    2 3 4    3
  // 3:  0  0  0  0 15 19    4 5      2
  // If the keys within each and every batch were grouped, skip will be true.
  // Now we test if groups occurred in order across batches (like illustration above), and if not set skip=false

  uint8_t ugrp[256];  // head(ugrp,ngrp) will contain the unique values in appearance order
  bool    seen[256];  // is the value present in ugrp already
  int     ngrp=0;     // max value 256 so not uint8_t
  uint8_t last_seen=0;  // the last grp seen in the previous batch.  initialized 0 is not used
  for (int i=0; i<256; i++) seen[i]=false;
  for (int batch=0; batch<nBatch; batch++) {
    const uint8_t * my_ugrp = ugrps + batch*256;
    if (ngrp==256 && !skip) break;  // no need to carry on
    for (int i=0; i<ngrps[batch]; i++) {
      if (!seen[my_ugrp[i]]) {
	seen[my_ugrp[i]] = true;
	ugrp[ngrp++] = last_seen = my_ugrp[i];
      } else if (skip && my_ugrp[i]!=last_seen) {   // ==last_seen would occur accross batch boundaries, like 2=>17 and 5=>15 in illustration above
	skip=false;
      }
    }
  }

  // If skip==true (my_n was pre-grouped) and
  // i) sortType==0 (not sorting groups) then osub and ksub don't need reordering. ugrp may happen to be sorted too but nothing special to do in that case.
  // ii) sortType==1|-1 and ugrp is already sorted then osub and ksub don't need reordering either.

  if (!sort_ugrp(ugrp, ngrp))
    skip=false;

  // now cumulate counts vertically to see where the blocks in the batches should be placed in the result across all batches
  // the counts are uint16_t but the cumulate needs to be int32_t (or int64_t in future) to hold the offsets
  // If skip==true and we're already done, we still need the first row of this cummulate (diff to get total group sizes) to push() or recurse below

  int *starts = (int*)calloc(nBatch*256, sizeof(int));  // keep starts the same shape and ugrp order as counts
  for (int j=0, sum=0; j<ngrp; j++) {  // iterate through columns (ngrp bytes)
    uint16_t *tmp1 = counts+ugrp[j];
    int      *tmp2 = starts+ugrp[j];
    for (int batch=0; batch<nBatch; batch++) {
      *tmp2 = sum;
      tmp2 += 256;
      sum += *tmp1;
      tmp1 += 256;
    }
  }
  // the first row now (when diff'd) now contains the size of each group across all batches

  if (!skip) {
    int *TMP = (int*)malloc(my_n * sizeof(int));
    /* if (!TMP) STOP(_("Unable to allocate TMP for my_n=%d items in parallel batch counting"), my_n); */
    for (int batch=0; batch<nBatch; batch++) {
      const int *      my_starts = starts + batch*256;
      const uint16_t * my_counts = counts + batch*256;
      const int *      osub = anso + from + batch*batchSize;  // the groups sit here contiguously
      const uint8_t *  byte = ugrps + batch*256;              // in appearance order always logged here in ugrps
      const int                my_ngrp = ngrps[batch];
      for (int i=0; i<my_ngrp; i++, byte++) {
	const uint16_t len = my_counts[*byte];
	memcpy(TMP+my_starts[*byte], osub, len*sizeof(int));
	osub += len;
      }
    }
    memcpy(anso+from, TMP, my_n*sizeof(int));

    for (int r=0; r<n_rem; r++) {    // TODO: groups of sizeof(anso)  4 byte int currently  (in future 8).  To save team startup cost (but unlikely significant anyway)
      for (int batch=0; batch<nBatch; batch++) {
	const int *      my_starts = starts + batch*256;
	const uint16_t * my_counts = counts + batch*256;
	const uint8_t *  ksub = key[radix+1+r] + from + batch*batchSize;  // the groups sit here contiguosly
	const uint8_t *  byte = ugrps + batch*256;                        // in appearance order always logged here in ugrps
	const int                my_ngrp = ngrps[batch];
	for (int i=0; i<my_ngrp; i++, byte++) {
	  const uint16_t len = my_counts[*byte];
	  memcpy((uint8_t *)TMP + my_starts[*byte], ksub, len);
	  ksub += len;
	}
      }
      memcpy(key[radix+1+r]+from, (uint8_t *)TMP, my_n);
    }
    free(TMP);
  }
  // notFirst is not used except timing

  int *my_gs = new int[ngrp];
  for (int i=1; i<ngrp; i++) my_gs[i-1] = starts[ugrp[i]] - starts[ugrp[i-1]];   // use the first row of starts to get totals
  my_gs[ngrp-1] = my_n - starts[ugrp[ngrp-1]];

  if (radix+1==nradix) {
    // aside: ngrp==my_n (all size 1 groups) isn't a possible short-circuit here similar to my_n>256 case above, my_n>65535 but ngrp<=256
  }
  else {
    // TODO: explicitly repeat parallel batch for any skew bins
    // all groups are <=65535 and radix_r() will handle each one single-threaded. Therefore, this time
    // it does make sense to start a parallel team and there will be no nestedness here either.
    // Skip https://github.com/Rdatatable/data.table/blob/be6c1fc66a411211c4ca944702c1cab7739445f3/src/forder.c#L1234-L1242
    // retgrp=0
    for (int i=0; i<ngrp; i++) {
      int start = from + starts[ugrp[i]];
      radix_r(start, start+my_gs[i]-1, radix+1, ind, rx);
    }
  }
  free(counts);
  free(starts);
  free(ugrps);
  free(ngrps);
  delete[] my_gs;
}

extern "C" int getThrottle(){
  return rxThrottle;
}
