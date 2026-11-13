#!/usr/bin/env bash
# Fail when hand-written C/C++ uses raw PROTECT()/UNPROTECT() instead of the
# rxode2 protect guards (src/rxProtect.h): `rxProtect rx_protect;` in C++,
# `rxProtectGuard;` + rxP()/rxUP()/rxUPAll() in C.  See CLAUDE.md.
#
# Scanned: src/ and inst/include/ C/C++ sources and headers, ignoring string
# literals (model code emitted by codegen) and comments.
# Skipped: generated RcppExports.cpp, rxProtect.h itself, vendored sundials.
# A line that must stay raw ends with the comment `check-protect: allow`.
# Run locally from the package root: .github/scripts/check-protect.sh
set -euo pipefail

pattern='\b(PROTECT|UNPROTECT|Rf_protect|Rf_unprotect)[[:space:]]*\('

bad=0
while IFS= read -r f; do
  case "$(basename "$f")" in
    RcppExports.cpp|rxProtect.h) continue ;;
  esac
  case "$f" in
    src/sundials_inc/*) continue ;;
  esac
  # drop allowed lines, string literals and // and one-line /* */ comments
  hits=$(grep -n '' "$f" | grep -v 'check-protect: allow' |
           sed -E 's/"([^"\\]|\\.)*"/""/g; s://.*$::; s:/\*.*\*/::g' |
           grep -E "$pattern" || true)
  if [ -n "$hits" ]; then
    while IFS= read -r h; do
      echo "::error file=$f,line=${h%%:*}::raw PROTECT/UNPROTECT; C++: 'rxProtect rx_protect;' + rx_protect.protect(); C: 'rxProtectGuard;' + rxP()/rxUPAll() (src/rxProtect.h)"
      echo "$f:$h"
    done <<<"$hits"
    bad=1
  fi
done < <(find src inst/include -type f \( -name '*.c' -o -name '*.cpp' -o -name '*.cc' -o -name '*.h' -o -name '*.hpp' \) | sort)

if [ "$bad" -ne 0 ]; then
  echo "Use the rxode2 protect guards instead of raw PROTECT/UNPROTECT:"
  echo "  C++: rxProtect rx_protect; SEXP x = rx_protect.protect(...);  (unprotects at scope exit)"
  echo "  C:   rxProtectGuard; SEXP x = rxP(...); ... rxUPAll(); before returning"
  echo "See CLAUDE.md, C/C++ Conventions."
  exit 1
fi
echo "No raw PROTECT/UNPROTECT in C/C++ sources."
