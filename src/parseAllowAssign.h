static inline void parseAllowAssignOrState(const char *s) {
  char *strings[] = {"cmt", "dvid", "addl", "ii", "ss", "amt", "dur", "rate", "Rprintf", "printf", "print"};
  for (int i=0; i<11; i++) {
    if (!strcmp(strings[i], s)) {
      sPrint(&_gbuf,"'%s' cannot be a state or lhs expression",s);
      _rxode2parse_unprotect();
      err_trans(_gbuf.s);
      return;
    }
  }
}
