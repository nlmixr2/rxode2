#pragma once
#define STRICT_R_HEADERS
#ifndef __rxode2parse_H__
#define __rxode2parse_H__
#define rxLlikSaveSize 9

#ifndef __RXODE2PTR_H__ // these refer to abi and should not be used.
#define getAdvan(idx) ind->solve + (op->neq + op->nlin)*(idx) + op->neq
#define getSolve(idx) ind->solve + (op->neq + op->nlin)*(idx)
#endif // __RXODE2PTR_H__


#define isDose(evid) ((evid) == 3 || (evid) >= 100)
#define isObs(evid) ((evid) == 0 || (evid) == 2 || ((evid) >= 9 && (evid) <= 99))

#ifndef __RXODE2PTR_H__ // these refer to abi and should not be used.
#define getEvid(ind, idx) (idx >= 0 ? ind->evid[idx] : ind->extraDoseEvid[-1-idx])
#define getEvidP1(ind, idx) (idx >= 0 ? ind->evid[idx+1] : ind->extraDoseEvid[-idx])
#define getEvidM1(ind, idx) (idx >= 0 ? ind->evid[idx-1] : ind->extraDoseEvid[-2-idx])

#define getDose(ind, idx) (idx >= 0 ? ind->dose[idx] : ind->extraDoseDose[-1-idx])
#define getDoseP1(ind, idx) (idx >= 0 ? ind->dose[idx+1] : ind->extraDoseDose[-idx])
#define getDoseM1(ind, idx) (idx >= 0 ? ind->dose[idx-1] : ind->extraDoseDose[-2-idx])

#define setDoseP1(ind, idx, val) if (idx >= 0){ind->dose[idx+1] = val;} else {ind->extraDoseDose[-idx] = val;}

#define getIi(ind, idx) (idx >= 0 ? ind->ii[idx] : 0.0)
#define getIiP1(ind, idx) (idx >= 0 ? ind->ii[idx+1] : 0.0)
#define getIiM1(ind, idx) (ind >= 0 ? ind->ii[idx-1] : 0.0)

#define getDoseM1(ind, idx) (idx >= 0 ? ind->dose[idx-1] : ind->extraDoseDose[-2-idx])

#define getAllTimes(ind, idx) (idx >= 0 ? ind->all_times[idx] : ind->extraDoseTime[-1-idx])
#define getAllTimesP1(ind, idx) (idx >= 0 ? ind->all_times[idx+1] : ind->extraDoseTime[-idx])
#define getAllTimesM1(ind, idx) (idx >= 0 ? ind->all_times[idx-1] : ind->extraDoseTime[-2-idx])

#define setAllTimesP1(ind, idx, val) if (idx>= 0) {ind->all_times[idx+1] = val;} else {ind->extraDoseTime[-idx] = val;}
#endif // __RXODE2PTR_H__

#include <R.h>
#include <stdbool.h>

#include <float.h>
#include <stdio.h>
#include <stdarg.h>

#include "rxode2_control.h"
#include <stdint.h>    // for uint64_t rather than unsigned long long

#include "rxode2parseStruct.h"
#endif
