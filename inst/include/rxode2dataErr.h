#ifndef RXODE2DATAERR_H
#define RXODE2DATAERR_H

#define rxErrCorruptETSort    1
#define rxErrRate0            2
#define rxErrModelRateAbsent  4
#define rxErrCorruptETSort2   8
#define rxErrDurNeg0          16
#define rxErrModelDurAbsent   32
#define rxErrModelData686     64
#define rxErrModelDataNeg6    128
#define rxErrModelDataErr8    256
#define rxErrModelDataErr886  512
#define rxErrModelDataErr797  1024
#define rxErrModelDataNeg7    2048
#define rxErrModelDataErr9    4096
#define rxErrModelDataErr997  8192
#define rxErrCorruptETSort3   16384
#define rxErrCorruptET        32768
#define rxErrNegCmt           65536
#define rxErrCorruptET2       131072
#define rxErrSync             262144
#define rxErrSync2            524288
#define rxErrModeledFss2      1048576
#define rxErrModeledFss2n2    2097152
#define rxErrModeledFss2n3    4194304
#define rxErrRate02           8388608

#define rxErrNaTimeLag   1
#define rxErrNaTimeRate  2
#define rxErrNaTimeDur   3
// during infusion amt
#define rxErrNaTimeAmtI  4
#define rxErrNaTimeAmt   5

#endif // RXODE2DATAERR_H
