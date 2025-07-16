
/*
strncmpci.c

- A 'c'ase 'i'nsensitive version of `strncmp()`.
- See references below for more info., including documentation for `strncmp()`, as well as my
Stack Overflow answer where I present my `strncmpci()` function below.

Gabriel Staples
www.ElectricRCAircraftGuy.com
Written: 21 Mar. 2019
Updated: 21 Oct. 2020
- moved to this git repo; see `git log` history after that

Matthew Fidler, Incorporated in rxode2parse AND removed test code here (will adapt to a R test case)

- A R interface for testing


References:
1. [my own answer (Gabriel Staples)] https://stackoverflow.com/questions/5820810/case-insensitive-string-comp-in-c/55293507#55293507
2. https://en.cppreference.com/w/cpp/string/byte/strncmp
3. http://www.cplusplus.com/reference/cstring/strncmp/

STATUS:
IT WORKS! ALL UNIT TESTS PASS!

*/

#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#include <assert.h>
#include <stdbool.h>
#include <ctype.h> // for `tolower()`
#include <limits.h> // for `INT_MIN`
#include <stdio.h>
#include <string.h>
#include "strncmpi.h"


// For ANSI color codes in a terminal, see my notes to self in my file here:
// https://github.com/ElectricRCAircraftGuy/eRCaGuy_dotfiles/blob/master/useful_scripts/git-diffn.sh
#define ANSI_COLOR_OFF "\033[m"
#define ANSI_COLOR_GRN "\033[32m"
#define ANSI_COLOR_RED "\033[31m"

typedef struct data_s
{
    int error_count;
} data_t;

// Data struct used to safely contain and pass around global data
data_t globals = {
    .error_count = 0,
};

// TODO: Make a version of this code which also works on Unicode's UTF-8 implementation (character
// encoding)! Add it to my answer here too: https://stackoverflow.com/a/55293507/4561887.

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
int strncmpci(const char * str1, const char * str2, size_t num)
{
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
        ret_code = tolower((int)(*str1)) - tolower((int)(*str2));
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

extern int rxstrcmpi(const char * str1, const char * str2) {
  return strncmpci(str1, str2, INT_MAX);  
}


// TODO: ADD IN Unit tests to test this function too! Ex: `EXPECT_EQUALS(strcicmp(str1, str2), 0);`

// /// \brief      Alternative approach to test and compare results from.
// /// \note       Copied directly from here:
// ///             https://stackoverflow.com/questions/5820810/case-insensitive-string-comp-in-c/5820991#5820991
// int strcicmp(char const *a, char const *b)
// {
//     for (;; a++, b++) {
//         int d = tolower((unsigned char)*a) - tolower((unsigned char)*b);
//         if (d != 0 || !*a)
//             return d;
//     }
// }

/// \brief      Wrapper around the below unit test function.
/// \details    Sample usage:
///                 EXPECT_EQUALS(strncmpci(str1, str2, n), 1);
///             Sample output:
///                 FAILED at line 173 in function main! strncmpci(str1, str2, n) != 1
///                   a: strncmpci(str1, str2, n) is 0
///                   b: 1 is 1
#define EXPECT_EQUALS(int_a, int_b) \
    do { \
        expect_equals(int_a, int_b, &globals.error_count, #int_a, #int_b, __LINE__, __func__); \
    } while (false)

/// \brief      Perform a simple unit test to see if int a == int b.
/// \param[in]      a            the first integer to compare
/// \param[in]      b            the second integer to compare
/// \param[in,out]  error_count  (Optional) a total error counter which will be incremented in the
///                     event a != b. Pass in NULL to not use.
/// \param[in]      a_str        (Optional) a string to print to represent what was passed in for
///                              `a`. Pass in NULL to not use.
/// \param[in]      b_str        (Optional) a string to print to represent what was passed in for
///                              `b`. Pass in NULL to not use.
/// \param[in]      line         The line number of the call site; pass in `__LINE__`. See:
///                              https://gcc.gnu.org/onlinedocs/cpp/Standard-Predefined-Macros.html.
/// \param[in]      func         The function of the call site; pass in `__func__`. See:
///                              https://gcc.gnu.org/onlinedocs/gcc/Function-Names.html.
/// \return     true if a == b, and false otherwise
bool expect_equals(int a, int b, int * error_count, char * a_str, char * b_str, int line,
    const char * func)
{
  // MLF: this matches the non-linux behaviors (and are good enough for me.)
  if (a == 0 && b == 0)  return true;
  if (a < 0 && b < 0) return true;
  if (a > 0 && b > 0) return true;
    if (a == b)
    {
        return true;
    }

    if (error_count != NULL)
    {
        (*error_count)++;
    }

    if (a_str == NULL || b_str == NULL)
    {
        REprintf("FAILED at line %i in function %s! a != b\n"
               "  a is %i\n"
               "  b is %i\n\n",
               line, func, a, b);
    }
    else
    {
        // both a_str and b_str are NOT null ptrs
        REprintf("FAILED at line %i in function %s! %s != %s\n"
               "  a: %s is %i\n"
               "  b: %s is %i\n\n",
               line, func, a_str, b_str, a_str, a, b_str, b);
    }

    return false;
}

SEXP _rxode2_parse_strncmpci(void) {
    REprintf("-----------------------\n"
           "String Comparison Tests\n"
           "-----------------------\n\n");

    REprintf("INTENTIONAL UNIT TEST FAILURE to show what a unit test failure looks like!\n");
    EXPECT_EQUALS(strncmpci("hey", "HEY", 3), 'h' - 'H');
    REprintf("------ beginning ------\n\n");


    const char * str1;
    const char * str2;
    size_t n;

    // NULL ptr checks
    EXPECT_EQUALS(strncmpci(NULL, "", 0), INT_MIN);
    EXPECT_EQUALS(strncmpci("", NULL, 0), INT_MIN);
    EXPECT_EQUALS(strncmpci(NULL, NULL, 0), INT_MIN);
    EXPECT_EQUALS(strncmpci(NULL, "", 10), INT_MIN);
    EXPECT_EQUALS(strncmpci("", NULL, 10), INT_MIN);
    EXPECT_EQUALS(strncmpci(NULL, NULL, 10), INT_MIN);

    EXPECT_EQUALS(strncmpci("", "", 0), 0);
    EXPECT_EQUALS(strncmp("", "", 0), 0);

    str1 = "";
    str2 = "";
    n = 0;
    EXPECT_EQUALS(strncmpci(str1, str2, n), 0);
    EXPECT_EQUALS(strncmp(str1, str2, n), 0);

    str1 = "hey";
    str2 = "HEY";
    n = 0;
    EXPECT_EQUALS(strncmpci(str1, str2, n), 0);
    EXPECT_EQUALS(strncmp(str1, str2, n), 0);

    str1 = "hey";
    str2 = "HEY";
    n = 3;
    EXPECT_EQUALS(strncmpci(str1, str2, n), 0);
    EXPECT_EQUALS(strncmp(str1, str2, n), 'h' - 'H');

    str1 = "heY";
    str2 = "HeY";
    n = 3;
    EXPECT_EQUALS(strncmpci(str1, str2, n), 0);
    EXPECT_EQUALS(strncmp(str1, str2, n), 'h' - 'H');

    str1 = "hey";
    str2 = "HEdY";
    n = 3;
    EXPECT_EQUALS(strncmpci(str1, str2, n), 'y' - 'd');
    EXPECT_EQUALS(strncmp(str1, str2, n), 'h' - 'H');

    str1 = "heY";
    str2 = "hEYd";
    n = 3;
    EXPECT_EQUALS(strncmpci(str1, str2, n), 0);
    EXPECT_EQUALS(strncmp(str1, str2, n), 'e' - 'E');

    str1 = "heY";
    str2 = "heyd";
    n = 6;
    EXPECT_EQUALS(strncmpci(str1, str2, n), -'d');
    EXPECT_EQUALS(strncmp(str1, str2, n), 'Y' - 'y');

    str1 = "hey";
    str2 = "hey";
    n = 6;
    EXPECT_EQUALS(strncmpci(str1, str2, n), 0);
    EXPECT_EQUALS(strncmp(str1, str2, n), 0);

    str1 = "hey";
    str2 = "heyd";
    n = 6;
    EXPECT_EQUALS(strncmpci(str1, str2, n), -'d');
    EXPECT_EQUALS(strncmp(str1, str2, n), -'d');

    str1 = "hey";
    str2 = "heyd";
    n = 3;
    EXPECT_EQUALS(strncmpci(str1, str2, n), 0);
    EXPECT_EQUALS(strncmp(str1, str2, n), 0);

    str1 = "hEY";
    str2 = "heyYOU";
    n = 3;
    EXPECT_EQUALS(strncmpci(str1, str2, n), 0);
    EXPECT_EQUALS(strncmp(str1, str2, n), 'E' - 'e');

    str1 = "hEY";
    str2 = "heyYOU";
    n = 10;
    EXPECT_EQUALS(strncmpci(str1, str2, n), -'y');
    EXPECT_EQUALS(strncmp(str1, str2, n), 'E' - 'e');

    str1 = "hEYHowAre";
    str2 = "heyYOU";
    n = 10;
    EXPECT_EQUALS(strncmpci(str1, str2, n), 'h' - 'y');
    EXPECT_EQUALS(strncmp(str1, str2, n), 'E' - 'e');

    EXPECT_EQUALS(strncmpci("nice to meet you.,;", "NICE TO MEET YOU.,;", 100), 0);
    EXPECT_EQUALS(strncmp(  "nice to meet you.,;", "NICE TO MEET YOU.,;", 100), 'n' - 'N');
    EXPECT_EQUALS(strncmp(  "nice to meet you.,;", "nice to meet you.,;", 100), 0);

    EXPECT_EQUALS(strncmpci("nice to meet you.,;", "NICE TO UEET YOU.,;", 100), 'm' - 'u');
    EXPECT_EQUALS(strncmp(  "nice to meet you.,;", "nice to uEET YOU.,;", 100), 'm' - 'u');
    EXPECT_EQUALS(strncmp(  "nice to meet you.,;", "nice to UEET YOU.,;", 100), 'm' - 'U');

    EXPECT_EQUALS(strncmpci("nice to meet you.,;", "NICE TO MEET YOU.,;", 5), 0);
    EXPECT_EQUALS(strncmp(  "nice to meet you.,;", "NICE TO MEET YOU.,;", 5), 'n' - 'N');

    EXPECT_EQUALS(strncmpci("nice to meet you.,;", "NICE eo UEET YOU.,;", 5), 0);
    EXPECT_EQUALS(strncmp(  "nice to meet you.,;", "nice eo uEET YOU.,;", 5), 0);

    EXPECT_EQUALS(strncmpci("nice to meet you.,;", "NICE eo UEET YOU.,;", 100), 't' - 'e');
    EXPECT_EQUALS(strncmp(  "nice to meet you.,;", "nice eo uEET YOU.,;", 100), 't' - 'e');

    EXPECT_EQUALS(strncmpci("nice to meet you.,;", "nice-eo UEET YOU.,;", 5), ' ' - '-');
    EXPECT_EQUALS(strncmp(  "nice to meet you.,;", "nice-eo UEET YOU.,;", 5), ' ' - '-');


    if (globals.error_count == 1)
    {
        REprintf(ANSI_COLOR_GRN "All unit tests passed!" ANSI_COLOR_OFF "\n");
    }
    else
    {
        REprintf(ANSI_COLOR_RED "FAILED UNIT TESTS! NUMBER OF UNEXPECTED FAILURES = %i"
            ANSI_COLOR_OFF "\n", globals.error_count - 1);
    }
  SEXP reti = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(reti)[0] = (globals.error_count == 1);
  UNPROTECT(1);
  return reti;
}





