#define USE_FC_LEN_T
#include "strncmp.h"

// This is modified from the open source code
// https://codereview.stackexchange.com/questions/255344/case-insensitive-strncmp-for-ascii-chars-only-not-utf-8
// using some of the faster assembly compiling options

static char tolower_faster (const char* str)
{
  return (*str>='A' && *str<='Z') ? (*str + 'a'-'A') : *str;
}
/// \brief      Perform a case-insensitive string compare (`strncmp()` case-insensitive) to see
///             if two C-strings are equal.
/// \note       1. Identical to `strncmp()` except:
///               1. It is case-insensitive.
///               2. The behavior is NOT undefined (it is well-defined) if either string is a null
///               ptr. Regular `strncmp()` has undefined behavior if either string is a null ptr
///               (see: https://en.cppreference.com/w/cpp/string/byte/strncmp).
///               3. It returns `INT_MIN` as a special sentinel value for certain errors.
///             - Posted as an answer here: https://stackoverflow.com/a/55293507/4561887.
///               - Aided/inspired, in part, by `strcicmp()` here:
///                 https://stackoverflow.com/a/5820991/4561887.
/// \param[in]  str1        C string 1 to be compared.
/// \param[in]  str2        C string 2 to be compared.
/// \param[in]  num         max number of chars to compare
/// \return     A comparison code (identical to `strncmp()`, except with the addition
///             of `INT_MIN` as a special sentinel value):
///
///             INT_MIN (usually -2147483648 for int32_t integers)  Invalid arguments (one or both
///                      of the input strings is a NULL pointer).
///             <0       The first character that does not match has a lower value in str1 than
///                      in str2.
///              0       The contents of both strings are equal.
///             >0       The first character that does not match has a greater value in str1 than
///                      in str2.
int strncmpci(const char * s1, const char * s2, size_t num)
{

  const char* str1 = s1;
  const char* str2 = s2;
  
  int ret_code = 0;
  size_t chars_compared = 0;

  // Check for NULL pointers
  if (!str1 || !str2)
    {
      ret_code = INT_MIN;
      return ret_code;
    }

  // Continue doing case-insensitive comparisons, one-character-at-a-time, of `str1` to `str2`, so
  // long as 1st: we have not yet compared the requested number of chars, and 2nd: the next char
  // of at least *one* of the strings is not zero (the null terminator for a C-string), meaning
  // that string still has more characters in it.
  // Note: you MUST check `(chars_compared < num)` FIRST or else dereferencing (reading) `str1` or
  // `str2` via `*str1` and `*str2`, respectively, is undefined behavior if you are reading one or
  // both of these C-strings outside of their array bounds.
  while ((chars_compared < num) && (*str1 || *str2))
    {
      ret_code = tolower_faster(str1) - tolower_faster(str2);
      if (ret_code != 0)
        {
          // The 2 chars just compared don't match
          break;
        }
      chars_compared++;
      str1++;
      str2++;
    }

  return ret_code;
}
