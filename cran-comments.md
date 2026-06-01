# rxode2 5.1.2

* CRAN has asked us to fix `nlmixr2est` `m1-san` which seems to be
  transient (it was flagged to us and then has been removed from the
  CRAN issues).  We could reproduce it using the rhub runners, but it
  was an issue with rxode2 instead of nlmixr2est with loading and
  unloading the same model.  We have fixed rxode2 so this issue no longer occurs.

To validate we performed a `m1-san` check using `rxode2` found here:

- https://github.com/nlmixr2/rxode2/actions/runs/26737546575

And also validated with a `m1-san` check using `nlmixr2est` found here:

- https://github.com/nlmixr2/nlmixr2est/actions/runs/26737555793

Both were successful.  Hence we are submitting `rxode2` quickly after
our first submission because of this fix.
