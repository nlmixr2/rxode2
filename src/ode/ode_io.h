#ifndef ODE_IO_H_
#define ODE_IO_H_

//! \file ode_io.h

#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>
#include <sstream>

namespace ode {

//!check if a file can be written (opened in write mode)
/*!
\param[in] fn desired file name
*/
void ode_check_write (const char *fn);

//!write an array to a binary file
/*!
\param[in] fn file name or path
\param[in] a array of values to write
\param[in] n size of array or number of elements to write
*/
template<class T>
void ode_write (char const *fn, T *a, unsigned long n) {
    //check that the file can be written
    ode_check_write(fn);
    //open the file
    FILE *ofile = fopen(fn, "wb");
    //write values
    fwrite(a, sizeof(T), n, ofile);
    //close the file
    fclose(ofile);
}

//!print a message and exit with failure
/*!
\param[in] msg the message to print before exiting the program
*/
void ode_print_exit (const char *msg);

//!converts an integer into a string
std::string ode_int_to_string (long i);

} // namespace ode

#endif
