#ifndef __RXODE2_SOLVE_WARN_H__
#define __RXODE2_SOLVE_WARN_H__

#ifdef __cplusplus
extern "C" {
#endif

/* Generic console-message aggregator for the ODE solver layer.

   Push: increment the counter for the message string `msg` and record
   `id` (an internal rxode2 subject id; -1 if not applicable) in a
   deduplicated set keyed by exact message text. The message text serves
   as the dedup key, so the right idiom is to pass a constant template
   string with %g/%d/%s placeholders rather than the rendered text — that
   way "intdy -- t = %g illegal" with t=1.1 and t=2.3 collapse into one
   summary line instead of two.

   Flush: for every distinct message string seen since the last reset,
   print one summary line of the form
       [<msg>]: <count> warning(s) for subject(s): <id1>, <id2>, ..., (<N> more)
   and clear the buffers. `maxIds` caps how many subject labels are
   listed before truncation. No-op if no warnings accumulated. Safe to
   call from serial code; uses an OpenMP critical region internally.

   Reset: drop all state without printing.

   Intended for non-stop warnings only. Anything that aborts the solve
   (Rf_error, hardfailure, etc.) must NOT route through this aggregator —
   those need to surface immediately, not be batched. */
void rxSolveWarnPush(int id, const char *msg, ...);
void rxSolveWarnFlush(int maxIds);
void rxSolveWarnReset(void);

#ifdef __cplusplus
}
#endif

#endif
