# Authors

- Matthew L. Fidler (core team/developer/manual)
- Melissa Hallow (tutorial writer)
- Wenping Wang (core team/developer)

# Contributors

- Zufar Mulyukov – Wrote initial version of
  [`rxShiny()`](https://nlmixr2.github.io/rxode2/reference/rxShiny.md)
  with modifications from Matthew Fidler
- Alan Hindmarsh – Lsoda author
- Awad H. Al-Mohy – Al-Mohy matrix exponential author
- Ernst Hairer – dop853 author
- Gerhard Wanner – dop853 author
- Goro Fuji – Timsort author
- Hadley Wickham – Author of original findLhs in RxODE, also original
  author of .s3register (used with permission to anyone, both changed by
  Matthew Fidler)
- Jack Dongarra – LApack author
- Linda Petzold – LSODA
- Martin Maechler – expm author, used routines from there for inductive
  linearization
- Morwenn – Timsort author
- Nicholas J. Higham – Author of Al-mohy matrix exponential
- Roger B. Sidje – expokit matrix exponential author
- Simon Frost – thread safe C implementation of liblsoda
- Kevin Ushey – Original author of fast factor, modified by Matthew
  Filder
- Yu Feng – thread safe liblsoda
- Cleve Moler – LApack author
- David Cooley – Author of fast_factor which was modified and now is
  used RxODE to quickly create factors for IDs without sorting them like
  R does
- Drew Schmidt – Drew Schmidt author of edits for exponential matrix
  utility taken from R package expm
- Matt Dowle & Arun Srinivasan – data.table authors on which the openMp
  framework is based upon

# RxODE acknowledgments:

- Sherwin Sy – Weight based dosing example
- Justin Wilkins – Documentation updates, logo and testing
- Emma Schwager – [R IJK
  distribution](https://github.com/biobakery/banocc/blob/master/R/rlkj.R)
  author
- J Coligne – dop853 fortran author
- Bill Denney – Documentation updates, manual and minor bug fixes
- Tim Waterhouse – Fixed one bug with mac working directories
- Richard Upton – Helped with solving the ADVAN linCmt() solutions
- Dirk Eddelbuettel – Made some fixes for the Rcpp changes require R
  strict headers
- Ross Ihaka – R author
- Robert Gentleman – R author
- R core team – R authors
