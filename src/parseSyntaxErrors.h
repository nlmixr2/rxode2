char *getLine (char *src, int line, int *lloc) {
  int cur = 1, col=0, i;
  for(i = 0; src[i] != '\0' && cur != line; i++){
    if(src[i] == '\n') cur++;
  }
  for(col = 0; src[i + col] != '\n' && src[i + col] != '\0'; col++);
  *lloc=i+col;
  char *buf = R_Calloc(col + 1, char);
  memcpy(buf, src + i, col);
  buf[col] = '\0';
  return buf;
}

void trans_syntax_error_report_fn0(char *err){
  if (!rx_suppress_syntax_info){
    if (lastSyntaxErrorLine == 0){
      if (isEsc) {
        RSprintf(_("\033[1mrxode2 model syntax error:\n================================================================================\033[0m"));
      }
      else {
        RSprintf(_("rxode2 model syntax error:\n================================================================================"));
      }
      lastSyntaxErrorLine=1;
    }
    if (isEsc) {
      RSprintf("\n\033[1m:ERR:\033[0m %s:\n",  err);
    }
    else {
      RSprintf("\n:ERR: %s:\n", err);
    }
  }
  rx_syntax_error = 1;
}

static inline void printSyntaxErrorHeader() {
  if (lastSyntaxErrorLine == 0){
    if (isEsc) {
      RSprintf(_("\033[1mrxode2 model syntax error:\n================================================================================\033[0m"));
    }
    else {
      RSprintf(_("rxode2 model syntax error:\n================================================================================"));
    }
    lastSyntaxErrorLine=1;
  }
}

static inline void printPriorLines(Parser *p) {
  char *buf;
  for (; lastSyntaxErrorLine < p->user.loc.line; lastSyntaxErrorLine++){
    buf = getLine(gBuf, lastSyntaxErrorLine, &gBufLast);
    RSprintf("\n:%03d: %s", lastSyntaxErrorLine, buf);
    R_Free(buf);
  }
  if (lastSyntaxErrorLine < p->user.loc.line){
    RSprintf("\n");
    lastSyntaxErrorLine++;
  }
}

static inline void printErrorInfo(Parser *p, char *err, char *after, int printLine) {
  if (printLine) {
    if (isEsc) {
      RSprintf("\n\033[1m:%03d:\033[0m %s:\n", p->user.loc.line, err);
    }
    else {
      RSprintf("\n:%03d: %s:\n", p->user.loc.line, err);
    }
  } else {
    if (after){
      if (isEsc){
        RSprintf(_("\n\n\033[1mrxode2 syntax error after\033[0m '\033[35m\033[1m%s\033[0m':\n"),  after);
      }
      else {
        RSprintf(_("\n\nrxode2 syntax error after '%s'\n"),  after);
      }
      if (firstErrD == 0) {
        sAppend(&firstErr, _("rxode2 syntax error after '%s':\n"), after);
      }
    }
    else{
      if (isEsc){
        RSprintf(_("\n\n\033[1mrxode2 syntax error\033[0m:\n"));
      }
      else{
        RSprintf(_("\n\nrxode2 syntax error:\n"));
      }
      if (firstErrD == 0) {
        sAppendN(&firstErr, "rxode2 syntax error:\n", 20);
      }
    }
  }
}

static inline void printErrorLineHighlightPoint(Parser *p) {
  char *buf = getLine(gBuf, p->user.loc.line, &gBufLast);
  RSprintf("      ");
  int i, len = strlen(buf);
  for (i = 0; i < p->user.loc.col; i++){
    RSprintf("%c", buf[i]);
    if (i == len-2) { i++; break;}
  }
  if (isEsc) {
    RSprintf("\033[35m\033[1m%c\033[0m", buf[i++]);
  }
  else {
    RSprintf("%c", buf[i++]);
  }
  for (; i < len; i++){
    RSprintf("%c", buf[i]);
  }
  RSprintf("\n      ");
  R_Free(buf);
  for (int i = 0; i < p->user.loc.col; i++){
    RSprintf(" ");
    if (i == len-2) { i++; break;}
  }
  if (isEsc) {
    RSprintf("\033[35m\033[1m^\033[0m");
  }
  else {
    RSprintf("^");
  }
  if (syntaxErrorExtra > 0 && syntaxErrorExtra < 40){
    for (int i = syntaxErrorExtra; i--;) {
      RSprintf("~");
    }
  }
  syntaxErrorExtra=0;
}

void trans_syntax_error_report_fn(char *err) {
  if (!rx_suppress_syntax_info){
    printSyntaxErrorHeader();
    Parser *p = (Parser *)curP;
    printPriorLines(p);
    printErrorInfo(p, err, 0, 1);
    printErrorLineHighlightPoint(p);
  }
  rx_syntax_error = 1;
}

static inline void printLineNumberAlone(Parser *p) {
  if (isEsc) {
    RSprintf("\033[1m:%03d:\033[0m ", p->user.loc.line);
  }
  else {
    RSprintf(":%03d: ", p->user.loc.line);
  }
  if (firstErrD == 0) {
    sAppend(&firstErr, ":%03d: ", p->user.loc.line);
  }
}

static inline void printErrorLineHighlight1(Parser *p, char *buf, char *after, int len) {
  int i;
  for (i = 0; i < p->user.loc.col; i++){
    RSprintf("%c", buf[i]);
    if (firstErrD == 0) {
      sAppend(&firstErr, "%c", buf[i]);
    }
    if (i == len-2) { i++; break;}
  }
  if (isEsc) {
    RSprintf("\033[35m\033[1m%c\033[0m", buf[i++]);
  }
  else {
    RSprintf("%c", buf[i++]);
  }
  if (firstErrD == 0) {
    sAppend(&firstErr, "%c", buf[i-1]);
  }
  for (; i < len; i++){
    RSprintf("%c", buf[i]);
    if (firstErrD == 0) {
      sAppend(&firstErr, "%c", buf[i]);
    }
  }
}

static inline int printErrorLineHighligt2afterCol(Parser *p, char *buf, char *after, int len, int col) {
  if (!col || col == len) return 0;
  for (int i = 0; i < col; i++){
    RSprintf(" ");
    if (firstErrD == 0) {
      sAppendN(&firstErr, " ", 1);
    }
    if (i == len-2) { i++; break;}
  }
  len = p->user.loc.col - col;
  if (len > 0 && len < 40){
    for (int i = len; i--;) {
      RSprintf("~");
      if (firstErrD == 0) {
        sAppendN(&firstErr, "~", 1);
      }
    }
  }
  if (isEsc) {
    RSprintf("\033[35m\033[1m^\033[0m");
  }
  else {
    RSprintf("^");
  }
  if (firstErrD == 0) {
    sAppendN(&firstErr, "^", 1);
  }
  return 1;
}

static inline void printErrorLineHighligt2after(Parser *p, char *buf, char *after, int len) {
  int col = 0, lenv = strlen(after);
  while (col != len && strncmp(buf + col, after, lenv) != 0) col++;
  if (col == len) col = 0;
  if (!printErrorLineHighligt2afterCol(p, buf, after, len, col)) {
    for (int i = 0; i < p->user.loc.col; i++){
      RSprintf(" ");
      if (firstErrD == 0) {
        sAppendN(&firstErr, " ", 1);
      }
      if (i == len-2) { i++; break;}
    }
    if (isEsc) {
      RSprintf("\033[35m\033[1m^\033[0m");
    }
    else {
      RSprintf("^");
    }
    if (firstErrD == 0) {
      sAppendN(&firstErr, "^", 1);
    }
  }
}

static inline void printErrorLineHighlight2(Parser *p, char *buf, char *after, int len) {
  RSprintf("\n      ");
  if (after){
    printErrorLineHighligt2after(p, buf, after, len);
  } else {
    for (int i = 0; i < p->user.loc.col; i++){
      RSprintf(" ");
      if (firstErrD == 0) {
        sAppendN(&firstErr, " ", 1);
      }
      if (i == len-2) { i++; break;}
    }
    if (isEsc) {
      RSprintf("\033[35m\033[1m^\033[0m");
    }
    else {
      RSprintf("^");
    }
    if (firstErrD == 0) {
      sAppendN(&firstErr, "^", 1);
    }
  }
}

static inline void printErrorLineHiglightRegion(Parser *p, char *after) {
  char *buf = getLine(gBuf, p->user.loc.line, &gBufLast);
  if (lastSyntaxErrorLine < p->user.loc.line) lastSyntaxErrorLine++;
  printLineNumberAlone(p);
  int len= strlen(buf);
  printErrorLineHighlight1(p, buf, after, len);
  printErrorLineHighlight2(p, buf, after, len);
  R_Free(buf);
}


static void rxSyntaxError(struct D_Parser *ap) {
  if (!rx_suppress_syntax_info){
    printSyntaxErrorHeader();
    Parser *p = (Parser *)ap;
    printPriorLines(p);
    char *after = 0;
    ZNode *z = p->snode_hash.last_all ? p->snode_hash.last_all->zns.v[0] : 0;
    while (z && z->pn->parse_node.start_loc.s == z->pn->parse_node.end)
      z = (z->sns.v && z->sns.v[0]->zns.v) ? z->sns.v[0]->zns.v[0] : 0;
    if (z && z->pn->parse_node.start_loc.s != z->pn->parse_node.end)
      after = rc_dup_str(z->pn->parse_node.start_loc.s, z->pn->parse_node.end);
    printErrorInfo(p, 0, after, 0);
    printErrorLineHiglightRegion(p, after);

    if (firstErrD == 0) {
      firstErrD = 1;
      sAppendN(&firstErr, "\nmore errors could be listed above", 34);
    }
  }
  rx_syntax_error = 1;
}

void updateSyntaxCol(){
  int i = lastStrLoc, lineNum=1, colNum=0;
  for(i = 0; gBuf[i] != '\0' && lastStr != gBuf + i; i++){
    if(gBuf[i] == '\n'){
      lineNum++;
      colNum=0;
    } else {
      colNum++;
    }
  }
  lastStrLoc=i;
  Parser *p = (Parser *)curP;
  p->user.loc.line=lineNum;
  p->user.loc.col=colNum;
}
