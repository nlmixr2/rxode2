#ifndef ODE_LINALG_H_
#define ODE_LINALG_H_

//! \file ode_linalg.h

#include <cmath>
#include <cstdio>
#include <cstdlib>

namespace ode {

//!Crout forward substitution
/*!
Performs forward substitution to solve a lower triangular matrix equation where the elements on the main diagonal of the lower triangular matrix are assumed to all be 1.
\param[in] L n by n lower triangular matrix
\param[in] b right hand side of matrix equation U*out = b
\param[in] p array of permutation indices from crout_LU
\param[in] n dimensions of A and length of b
\param[out] out array of length n, result of substitution
*/
void ode_crout_forw_sub (double **L, double *b, int *p, int n, double *out);

//!Backward substitution
/*!
Performs backward substitution to solve an upper triangular matrix equation
input:
\param[in] U n by n upper triangular matrix
\param[in] b right hand side of matrix equation U*out = b
\param[in] n dimensions of A and length of b
\param[out] out array of length n, result of substitution
*/
void ode_back_sub (double **U, double *b, int n, double *out);

//!Crout LU decomposition
/*!
performs LU decomposition with partial pivoting on the matrix A and sets permutation indices in the array p, following the outline in the book:
    + Demmel, James W. Applied numerical linear algebra. Vol. 56. Siam, 1997.
\param A matrix of size n to decompose (overwritten)
\param[in] n size of square matrix
\param[out] p integer array loaded with permutation indices
*/
void ode_crout_LU (double **A, int n, int *p);

//!Solves a matrix equation where the matrix has already be crout LU decomposed
/*!
\param[in] LU crout LU decomposed matrix
\param[in] p permutation indices
\param[in] b right-hand side of matrix equation
\param[in] n size of matrix and arrays
\param[out] out solution to linear system
*/
void ode_solve_LU (double **LU, int *p, double *b, int n, double *out);

//!Solves a matrix equation A x = b with crout LU decomposition
/*!
\param[in] A matrix, is overwritten
\param[in] b right-hand side of matrix equation
\param[in] n size of matrix and arrays
\param[out] out solution to linear system
*/
void ode_solve_A (double **A, double *b, int n, double *out);

//!Solves a tridiagonal matrix equation
/*!
The input matrix `T` must be in banded storage. This means that the diagonals are placed on rows so that the trigiagonal matrix `A`

    A = | a00  a01   0    0  |
        | a10  a11  a12   0  |
        |  0   a21  a22  a23 |
        |  0    0   a32  a33 |

is stored in banded form as the array `T`

    T = |  *   a01  a12  a23 |
        | a00  a11  a22  a33 |
        | a10  a21  a31   *  |

and the value of the asterisk elements doesn't matter.

\param[in] T tridiagonal matrix in banded storage form
\param[in] r right-hand side of matrix equation
\param[in] temp temporary array
\param[in] n size of matrix and arrays
\param[out] out solution to linear system
*/
void ode_solve_tridiag (double **T, double *r, double *temp, int n, double *out);

} // namespace ode

#endif
