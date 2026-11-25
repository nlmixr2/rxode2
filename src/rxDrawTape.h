// In-model random draws made while solving (rxode2#1812).  The solve pass
// evaluates calc_lhs() (where pushes such as replace() fire), the initial
// conditions and mtime() under a key naming the record and context; each draw
// made under a key is taken from the subject's own stream and recorded, and the
// same key evaluated again (in the output pass, or again while solving)
// replays the recorded draws in order.  Without a key, draws stay 0 outside
// the output pass (eg in d/dt()).
#ifndef __RXDRAWTAPE_H__
#define __RXDRAWTAPE_H__

#include "../inst/include/rxode2.h"

#define RX_DRAW_LHS   0
#define RX_DRAW_INIS  1
#define RX_DRAW_MTIME 2

#if defined(__cplusplus)
extern "C" {
#endif
  void rxDrawTapeReset(rx_solving_options_ind *ind);
  void rxDrawTapeFree(rx_solving_options_ind *ind);
  void rxDrawTapeResumeLhs(rx_solving_options_ind *ind);
#if defined(__cplusplus)
}
#endif

// Draws until rxDrawKeyClear() belong to record `ix` (-1 = before the first
// record) evaluated in context `kind`.
static inline void rxDrawKeySet(rx_solving_options_ind *ind, int ix, int kind) {
  ind->drawKey = (ix + 1) * 3 + kind + 1;
  ind->drawPos = 0;
}

static inline void rxDrawKeyClear(rx_solving_options_ind *ind) {
  ind->drawKey = 0;
  ind->drawPos = 0;
}

#endif // __RXDRAWTAPE_H__
