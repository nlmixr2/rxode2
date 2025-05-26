# rxode2 3.0.4

- Add stable hashes for rxUi objects (#838, #689)

- Fix for iov simulation (#842)

- Fix for `rxnbinom()` called directly from R (#847) and expand it to
  match more close with R's `rnbinom()` including allowing named `mu=`
  calls.  In rxode2 ui, these are also now allowed.
