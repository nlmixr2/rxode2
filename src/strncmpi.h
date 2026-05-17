#ifndef __STRNCMPI_H__
#define __STRNCMPI_H__
#pragma once
#if defined(__cplusplus)
extern "C" {
#endif
#define strncmpci rxode2parse_strncmpci
  
  int strncmpci(const char * str1, const char * str2, size_t num);
  int rxstrcmpi(const char * str1, const char * str2);
  
#if defined(__cplusplus)
}
#endif
#endif
