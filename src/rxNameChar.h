/*
 * rxNameChar.h -- which characters make up a model name.
 *
 * The name rules of rxode2's model syntax (and of the symengine names that
 * stand for them): a letter, '.' or '_' to start, then those or digits.  The
 * text scanners in the translators and in the CSE pass all ask this.
 */
#ifndef __RX_NAME_CHAR_H__
#define __RX_NAME_CHAR_H__

static inline int rxNameStart(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '.' || c == '_';
}

static inline int rxNameChar(char c) {
  return rxNameStart(c) || (c >= '0' && c <= '9');
}

#endif /* __RX_NAME_CHAR_H__ */
