# rxode2 5.1.5

Vendored the SUNDIALS public headers and dropped `LinkingTo: sundialr`, so the
already-vendored SUNDIALS sources always compile against headers from the same
SUNDIALS release.  Also removed the `qs2` (and `stringfish`) dependency.

The remainder of the release is bug fixes; see NEWS.md.
