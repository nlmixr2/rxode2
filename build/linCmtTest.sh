#!/bin/bash
export R_HOME=/usr/lib/R/
export R_HOME_DIR=/usr/lib/R
export R_SHARE_DIR=/usr/share/R/share
export R_INCLUDE_DIR=/usr/share/R/include
export R_DOC_DIR=/usr/share/R/doc
. /usr/lib/R/etc/ldpaths

LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libprofiler.so CPUPROFILE=sample.profile /usr/lib/R/bin/exec/R -f linCmtTest.R

#google-pprof --text /usr/bin/R ./sample.profile
google-pprof --web /usr/bin/R ./sample.profile
