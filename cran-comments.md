# rxode2 5.1.6

This is a bug-fix follow-up to 5.1.5.

This is submitted within a few days of the last update because RcppParallel 6.2.0 reversed their TBB decision.

The remaining changes are bug fixes

## Reverse dependencies

This resubmission fixes the reverse-dependency failures reported for the
previous 5.1.6 upload.

Those failures all came from a single change, which has been reverted.  rxode2
shares its C entry points with downstream packages through a positional
function-pointer table, so that a new rxode2 can be released without rebuilding
every reverse dependency.  The previous upload corrected two labels in that
table's name vector that did not describe the slots they sat on.  Nothing about
the ABI moved and the table is still read positionally, but the released
nlmixr2est records those names when it is compiled and compares them when it
loads, so it refused to start:

    nlmixr2est needs a different version of rxode2 api, cannot run nlmixr2est

which failed every package that depends on nlmixr2est.

The labels are restored, so the names are now identical to those in 5.1.5, and
they are documented as frozen: new entry points are only ever appended.  The
build-time check on that table, added in the same upload, has also been removed,
since every reverse dependency runs it when it loads.

The affected maintainers are the nlmixr2 team, who are aware of the change.
