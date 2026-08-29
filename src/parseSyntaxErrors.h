char *getLine (char *src, int line, int *lloc) {
  int cur = 1, i;
  size_t col = 0;
  for(i = 0; src[i] != '\0' && cur != line; i++){
    if(src[i] == '\n') cur++;
  }
  for(col = 0; src[i + col] != '\n' && src[i + col] != '\0'; col++){
    if (col == (size_t)INT_MAX) {
      (Rf_error)(_("line too long in getLine"));
    }
  }
  if ((size_t)i + col > (size_t)INT_MAX) {
    (Rf_error)(_("source offset overflow in getLine"));
  }
  *lloc = i + (int)col;
  char *buf = R_Calloc((int)col + 1, char);
  memcpy(buf, src + i, col);
  buf[col] = '\0';
  return buf;
}

extern sbuf sbErr1;

int _rxode2_reallyHasAfter = 0;

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

static inline void printSyntaxErrorHeader(void) {
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
    if (_rxode2_reallyHasAfter == 1 && after){
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
  sAppend(&sbErr1, "      ");
  int i, len = strlen(buf);
  for (i = 0; i < p->user.loc.col; i++){
    sAppend(&sbErr1, "%c", buf[i]);
    if (i == len-2) { i++; break;}
  }
  if (isEsc) {
    sAppend(&sbErr1, "\033[35m\033[1m%c\033[0m", buf[i++]);
  }
  else {
    sAppend(&sbErr1, "%c", buf[i++]);
  }
  for (; i < len; i++){
    sAppend(&sbErr1, "%c", buf[i]);
  }
  sAppend(&sbErr1, "\n      ");
  R_Free(buf);
  for (int i = 0; i < p->user.loc.col; i++){
    sAppendN(&sbErr1, " ", 1);
    if (i == len-2) { i++; break;}
  }
  if (isEsc) {
    sAppend(&sbErr1, "\033[35m\033[1m^\033[0m");
  }
  else {
    sAppend(&sbErr1, "^");
  }
  if (syntaxErrorExtra > 0 && syntaxErrorExtra < 40){
    for (int i = syntaxErrorExtra; i--;) {
      sAppend(&sbErr1, "~");
      _rxode2_reallyHasAfter=1;
    }
  }
  syntaxErrorExtra=0;
}

void trans_syntax_error_report_fn(char *err) {
  if (!rx_suppress_syntax_info){
    printSyntaxErrorHeader();
    Parser *p = (Parser *)curP;
    printPriorLines(p);
    sClear(&sbErr1);
    sClear(&sbErr2);
    _rxode2_reallyHasAfter = 0;
    printErrorLineHighlightPoint(p);
    printErrorInfo(p, err, 0, 1);
    RSprintf("%s", sbErr1.s);
    lastSyntaxErrorLine++;
  }
  rx_syntax_error = 1;
}

static inline void printLineNumberAlone(Parser *p) {
  if (isEsc) {
    sAppend(&sbErr1, "\033[1m:%03d:\033[0m ", p->user.loc.line);
  }
  else {
    sAppend(&sbErr1, ":%03d: ", p->user.loc.line);
  }
  if (firstErrD == 0) {
    sAppend(&sbErr2, ":%03d: ", p->user.loc.line);
  }
}

static inline void printErrorLineHighlight1(Parser *p, char *buf, char *after, int len) {
  int i;
  for (i = 0; i < p->user.loc.col; i++){
    sAppend(&sbErr1, "%c", buf[i]);
    if (firstErrD == 0) {
      sAppend(&sbErr2, "%c", buf[i]);
    }
    if (i == len-2) { i++; break;}
  }
  if (isEsc) {
    sAppend(&sbErr1, "\033[35m\033[1m%c\033[0m", buf[i++]);
  }
  else {
    sAppend(&sbErr1, "%c", buf[i++]);
  }
  if (firstErrD == 0) {
    sAppend(&sbErr2, "%c", buf[i-1]);
  }
  for (; i < len; i++){
    sAppend(&sbErr1, "%c", buf[i]);
    if (firstErrD == 0) {
      sAppend(&sbErr2, "%c", buf[i]);
    }
  }
}

static inline int printErrorLineHighligt2afterCol(Parser *p, char *buf, char *after, int len, int col) {
  if (!col || col == len) return 0;
  for (int i = 0; i < col; i++){
    sAppend(&sbErr1, " ");
    if (firstErrD == 0) {
      sAppendN(&sbErr2, " ", 1);
    }
    if (i == len-2) { i++; break;}
  }
  len = p->user.loc.col - col;
  if (len > 0 && len < 40){
    for (int i = len; i--;) {
      sAppend(&sbErr1, "~");
      _rxode2_reallyHasAfter=1;
      if (firstErrD == 0) {
        sAppendN(&sbErr2, "~", 1);
      }
    }
  }
  if (isEsc) {
    sAppend(&sbErr1, "\033[35m\033[1m^\033[0m");
  }
  else {
    sAppend(&sbErr1, "^");
  }
  if (firstErrD == 0) {
    sAppendN(&sbErr2, "^", 1);
  }
  return 1;
}

static inline void printErrorLineHighligt2after(Parser *p, char *buf, char *after, int len) {
  int col = 0, lenv = strlen(after);
  while (col != len && strncmp(buf + col, after, lenv) != 0) col++;
  if (col == len) col = 0;
  if (!printErrorLineHighligt2afterCol(p, buf, after, len, col)) {
    for (int i = 0; i < p->user.loc.col; i++){
      sAppend(&sbErr1, " ");
      if (firstErrD == 0) {
        sAppendN(&sbErr2, " ", 1);
      }
      if (i == len-2) { i++; break;}
    }
    if (isEsc) {
      sAppend(&sbErr1, "\033[35m\033[1m^\033[0m");
    }
    else {
      sAppend(&sbErr1, "^");
    }
    if (firstErrD == 0) {
      sAppendN(&sbErr2, "^", 1);
    }
  }
}

static inline void printErrorLineHighlight2(Parser *p, char *buf, char *after, int len) {
  sAppend(&sbErr1, "\n      ");
  if (firstErrD == 0) {
    sAppendN(&sbErr2, "\n      ", 7);
  }
  if (_rxode2_reallyHasAfter == 1 && after){
    printErrorLineHighligt2after(p, buf, after, len);
  } else {
    for (int i = 0; i < p->user.loc.col; i++){
      sAppendN(&sbErr1, " ", 1);
      if (firstErrD == 0) {
        sAppendN(&sbErr2, " ", 1);
      }
      if (i == len-2) { i++; break;}
    }
    if (isEsc) {
      sAppendN(&sbErr1, "\033[35m\033[1m^\033[0m", 14);
    }
    else {
      sAppendN(&sbErr1, "^", 1);
    }
    if (firstErrD == 0) {
      sAppendN(&sbErr2, "^", 1);
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


/* An `if`/`while` with no body at all is the one construct this grammar cannot
   accept and cannot explain for itself: `statement` is deliberately not
   nullable (inst/tran.g), so the parse simply fails at whatever follows, and
   dparser's generic "syntax error" with a caret says nothing about why.  The
   message says what to write instead.

   Recognized by walking back from the failure: skip whitespace and comments,
   expect ')', match back to its '(', skip whitespace again, and look for the
   keyword.  Returns the keyword, or NULL when this is some other error. */
/* spelled out rather than isalnum(), which is locale dependent and would need
   another header for one comparison */
static inline int rxIsWordChar(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
    (c >= '0' && c <= '9') || c == '_' || c == '.';
}

static inline const char *rxBodilessIfWhile(Parser *p) {
  int line = 1, col = 0, depth;
  const char *s = gBuf, *at;
  if (gBuf == NULL) return NULL;
  for (; *s != '\0'; s++) {
    if (line == p->user.loc.line && col == p->user.loc.col) break;
    if (*s == '\n') { line++; col = 0; } else { col++; }
  }
  at = s;
  /* back over whitespace */
  while (at > gBuf) {
    const char *b = at - 1;
    if (*b == ' ' || *b == '\t' || *b == '\r' || *b == '\n') { at = b; continue; }
    break;
  }
  /* `if (c) else s` fails AT the else, so step back over it and keep looking;
     the missing body is the then-branch */
  if (at - gBuf >= 4 && !strncmp(at - 4, "else", 4) &&
      (at - gBuf == 4 || !rxIsWordChar(at[-5]))) {
    at -= 4;
    while (at > gBuf) {
      const char *b = at - 1;
      if (*b == ' ' || *b == '\t' || *b == '\r' || *b == '\n') { at = b; continue; }
      break;
    }
  }
  if (at == gBuf || at[-1] != ')') return NULL;
  /* match the condition's parentheses back to their opening '(' */
  at--;                       /* now on ')' */
  depth = 0;
  while (at > gBuf) {
    if (*at == ')') depth++;
    else if (*at == '(') { depth--; if (depth == 0) break; }
    at--;
  }
  if (depth != 0 || *at != '(') return NULL;
  while (at > gBuf && (at[-1] == ' ' || at[-1] == '\t')) at--;
  if (at - gBuf >= 2 && !strncmp(at - 2, "if", 2) &&
      (at - gBuf == 2 || !rxIsWordChar(at[-3]))) return "if";
  if (at - gBuf >= 5 && !strncmp(at - 5, "while", 5) &&
      (at - gBuf == 5 || !rxIsWordChar(at[-6]))) return "while";
  return NULL;
}

static void rxSyntaxError(struct D_Parser *ap) {
  if (!rx_suppress_syntax_info){
    Parser *pb = (Parser *)ap;
    const char *kw = rxBodilessIfWhile(pb);
    if (kw != NULL) {
      /* _gbuf is an sbuf, so the message is built on the heap the way every
         other reported syntax error in this parser builds its own */
      sPrint(&_gbuf,
             "'%s' needs a statement for its body; use '%s (...) {}' for an empty one",
             kw, kw);
      trans_syntax_error_report_fn(_gbuf.s);
      return;
    }
    printSyntaxErrorHeader();
    Parser *p = (Parser *)ap;
    printPriorLines(p);
    char *after = 0;
    ZNode *z = p->snode_hash.last_all ? p->snode_hash.last_all->zns.v[0] : 0;
    while (z && z->pn->parse_node.start_loc.s == z->pn->parse_node.end)
      z = (z->sns.v && z->sns.v[0]->zns.v) ? z->sns.v[0]->zns.v[0] : 0;
    if (_rxode2_reallyHasAfter==1 && z && z->pn->parse_node.start_loc.s != z->pn->parse_node.end)
      after = rc_dup_str(z->pn->parse_node.start_loc.s, z->pn->parse_node.end);
    sClear(&sbErr1);
    sClear(&sbErr2);
    _rxode2_reallyHasAfter = 0;
    printErrorLineHiglightRegion(p, after);
    printErrorInfo(p, 0, after, 0);
    RSprintf("%s", sbErr1.s);
    if (firstErrD == 0) {
      firstErrD = 1;
      sAppend(&firstErr, "\n%s", sbErr2.s);
      sAppendN(&firstErr, "\nmore errors could be listed above", 34);
    }
  }
  rx_syntax_error = 1;
}

void updateSyntaxCol(void) {
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
