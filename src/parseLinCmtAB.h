static inline void linCmtB(linCmtStruct *lin, const char *in, int *index) {
  if (in[1] == '\0') {
    lin->b = *index;
    return;
  }
  if ((in[1] == 'E' || in[1] == 'e') &&
      (in[2] == 'T' || in[2] == 't') &&
      (in[3] == 'A' || in[3] == 'a') &&
      in[4] == '\0') {
    lin->beta = *index;
    return;
  }
}

static inline int isOnlyA(linCmtStruct *lin, const char *in, int *index) {
  if (in[1] == '\0') {
    lin->a = *index;
    return 1;
  }
  return 0;
}

static inline int isOnlyAOB(linCmtStruct *lin, const char *in, int *index){
  if ((in[1] == 'O' || in[1] == 'o') &&
      (in[2] == 'B' || in[2] == 'b') &&
      in[3] == '\0') {
    lin->aob = *index;
    return 1;
  }
  return 0;
}

static inline int isOnlyAlpha(linCmtStruct *lin, const char *in, int *index) {
  if ((in[1] == 'L' || in[1] == 'l') &&
      (in[2] == 'P' || in[2] == 'p') &&
      (in[3] == 'H' || in[3] == 'h') &&
      (in[4] == 'A' || in[4] == 'a') &&
      in[5] == '\0') {
    lin->alpha = *index;
    return 1;
  }
  return 0;
}
static inline void linCmtA(linCmtStruct *lin, const char *in, int *index) {
  int tmp = isOnlyA(lin, in, index) ||
    isOnlyAOB(lin, in, index) ||
    isOnlyAlpha(lin, in, index);
  (void)tmp;
}
