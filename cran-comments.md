# rxode2 4.0.2

- Fix odr rule that came from `rxode2` but was seen in
`nlmixr2est`. CRAN requested that this issue be fixed    (https://www.stats.ox.ac.uk/pub/bdr/M1-SAN/nlmixr2est).

- Be a bit more careful so that names are not
  duplicated.  Now include the md5 hash, a global counter and random 4
  digit and number combination. In addition add the name of the
  original function so it will be easier to debug in the future.

- Fall back to data.frame `rbind` when `rbind.rxSolve()` fails
