#ifndef __RXODE2_SOLVE_WARN_H__
#define __RXODE2_SOLVE_WARN_H__

#ifdef __cplusplus
extern "C" {
#endif

/* Warning families. New entries go at the end. */
#define RX_WARN_INTDY 0
#define RX_WARN_N     1

/* Aggregator API.

   Push: increment the counter for `family` and record `id` in a small
   per-thread deduplicated set. Called from inside the ODE solve loop on
   every miss; must be lock-free.

   Flush: if any family has count > 0, print one summary line per family
   listing the count and up to `maxIds` subject labels, then reset the
   buffers. Safe to call when no warnings accumulated (no-op).

   Reset: drop all state without printing. Useful at the start of a
   fresh solve. */
void rxSolveWarnPush(int family, int id);
void rxSolveWarnFlush(int maxIds);
void rxSolveWarnReset(void);

#ifdef __cplusplus
}
#endif

#endif
