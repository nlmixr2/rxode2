#ifndef __SEED_H__
#define __SEED_H__

#if defined(__cplusplus)
extern "C" {
#endif
  typedef uint32_t (*getRxSeed1_t)(int ncores);
  extern getRxSeed1_t getRxSeed1;
  typedef void (*setSeedEng1V_t)(uint32_t seed);
  extern setSeedEng1V_t setSeedEng1V;
  typedef void (*setSeedEng1_t)(uint32_t seed);
  extern setSeedEng1_t setSeedEng1;
  typedef void (*setRxSeedFinal_t)(uint32_t seed);
  extern setRxSeedFinal_t setRxSeedFinal;
  typedef void (*seedEngV_t)(uint32_t seed, int ncores);
  extern seedEngV_t seedEngV;
  typedef void (*seedEng_t)(uint32_t ncores);
  extern seedEng_t seedEng;
#if defined(__cplusplus)
}
#endif

#endif // __SEED_H__
