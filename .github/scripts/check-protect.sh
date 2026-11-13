#!/usr/bin/env bash
# Fail when hand-written C++ uses raw PROTECT()/UNPROTECT() instead of the
# rxProtect RAII guard (src/rxProtect.h).  See "C/C++ Conventions" in CLAUDE.md.
#
# Scanned: src/ and inst/include/ C++ sources and headers.
# Skipped: generated RcppExports.cpp, rxProtect.h itself, vendored sundials
# headers, and headers a .c file includes (C code uses rxP()/rxUP()).
# Run locally from the package root: .github/scripts/check-protect.sh
set -euo pipefail

pattern='\b(PROTECT|UNPROTECT|Rf_protect|Rf_unprotect)[[:space:]]*\('

# headers pulled into C translation units
cHeaders=$(grep -hoE '^[[:space:]]*#[[:space:]]*include[[:space:]]+"[^"]+"' src/*.c |
             sed -E 's/.*"([^"]+)"/\1/' | xargs -n1 basename | sort -u)

bad=0
while IFS= read -r f; do
  base=$(basename "$f")
  case "$base" in
    RcppExports.cpp|rxProtect.h) continue ;;
  esac
  case "$f" in
    src/sundials_inc/*) continue ;;
  esac
  if [[ "$f" == *.h ]] && grep -qxF "$base" <<<"$cHeaders"; then
    continue
  fi
  if hits=$(grep -nE "$pattern" "$f"); then
    while IFS= read -r h; do
      echo "::error file=$f,line=${h%%:*}::raw PROTECT/UNPROTECT in C++; use 'rxProtect rx_protect;' and rx_protect.protect() (src/rxProtect.h)"
      echo "$f:$h"
    done <<<"$hits"
    bad=1
  fi
done < <(find src inst/include -type f \( -name '*.cpp' -o -name '*.cc' -o -name '*.h' -o -name '*.hpp' \) | sort)

if [ "$bad" -ne 0 ]; then
  echo "C++ must use the rxProtect guard: 'rxProtect rx_protect;' then rx_protect.protect(x);"
  echo "it unprotects when it goes out of scope.  See CLAUDE.md, C/C++ Conventions."
  exit 1
fi
echo "No raw PROTECT/UNPROTECT in C++ sources."
