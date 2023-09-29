// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#ifndef __PAR_SOLVE_H___
#define __PAR_SOLVE_H___

#if defined(__cplusplus)
extern "C" {
#endif


#include "../inst/include/rxode2.h"
#include "rxThreadData.h"

  void sortInd(rx_solving_options_ind *ind);

#include <rxode2parseIniSubject.h>

#if defined(__cplusplus)
}
#endif

#endif
