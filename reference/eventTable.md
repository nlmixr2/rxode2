# Create an event table object

Initializes an object of class ‘EventTable’ with methods for adding and
querying dosing and observation records

## Usage

``` r
eventTable(amount.units = NA, time.units = NA)
```

## Arguments

- amount.units:

  string denoting the amount dosing units, e.g., “mg”, “ug”. Default to
  `NA` to denote unspecified units. It could also be a solved rxode2
  object. In that case, eventTable(obj) returns the eventTable that was
  used to solve the rxode2 object.

- time.units:

  string denoting the time units, e.g., “hours”, “days”. Default to
  `"hours"`.

  An `eventTable` is an object that consists of a data.frame storing
  ordered time-stamped events of an (unspecified) PK/PD dynamic system,
  units (strings) for dosing and time records, plus a list of functions
  to add and extract event records.

  Currently, events can be of two types: dosing events that represent
  inputs to the system and sampling time events that represent
  observations of the system with ‘amount.units’ and ‘time.units’,
  respectively.

## Value

A modified data.frame with the following accessible functions:

- `get.EventTable()` returns the current event table

- [`add.dosing()`](https://nlmixr2.github.io/rxode2/reference/add.dosing.md)
  adds dosing records to the event table.

- `get.dosing()` returns a data.frame of dosing records.

- `clear.dosing()` clears or deletes all dosing from event table

- \`[`add.sampling()`](https://nlmixr2.github.io/rxode2/reference/add.sampling.md)
  adds sampling time observation records to the event table.

- `get.sampling()`returns a data.frame of sampled observation records.

- `clear.sampling()` removes all sampling from event table.

- `get.obs.rec()` returns a logical vector indicating whether each event
  record represents an observation or not.

- `get.nobs()` returns the number of observation (not dosing) records.

- `get.units()` returns a two-element character vector with the dosing
  and time units, respectively

- `copy()` makes a copy of the current event table. To create a copy of
  an event table object use `qd2 <- qd$copy()`

- [`expand()`](https://rdrr.io/pkg/symengine/man/expand.html) Expands
  the event table for multi-subject solving. This is done by
  `qd$expand(400)` for a 400 subject data expansion

## See also

[`et()`](https://nlmixr2.github.io/rxode2/reference/et.md)

## Author

Matthew Fidler, Melissa Hallow and Wenping Wang

## Examples

``` r
# create dosing and observation (sampling) events
# QD 50mg dosing, 5 days followed by 25mg 5 days
#
qd <- eventTable(amount.units = "mg", time.units = "days")
#
qd$add.dosing(dose = 50, nbr.doses = 5, dosing.interval = 1, do.sampling = FALSE)
#
# sample the system's drug amounts hourly the first day, then every 12 hours
# for the next 4 days
qd$add.sampling(seq(from = 0, to = 1, by = 1 / 24))
qd$add.sampling(seq(from = 1, to = 5, by = 12 / 24))
#
# print(qd$get.dosing())     # table of dosing records
print(qd$get.nobs()) # number of observation (not dosing) records
#> [1] 34
#
# BID dosing, 5 days
bid <- eventTable("mg", "days") # only dosing
bid$add.dosing(
  dose = 10000, nbr.doses = 2 * 5,
  dosing.interval = 12, do.sampling = FALSE
)
#
# Use the copy() method to create a copy (clone) of an existing
# event table (simple assignments just create a new reference to
# the same event table object (closure)).
#
bid.ext <- bid$copy() # three-day extension for a 2nd cohort
bid.ext$add.dosing(
  dose = 5000, nbr.doses = 2 * 3,
  start.time = 120, dosing.interval = 12, do.sampling = FALSE
)

# You can also use the Piping operator to create a table

qd2 <- eventTable(amount.units = "mg", time.units = "days") %>%
  add.dosing(dose = 50, nbr.doses = 5, dosing.interval = 1, do.sampling = FALSE) %>%
  add.sampling(seq(from = 0, to = 1, by = 1 / 24)) %>%
  add.sampling(seq(from = 1, to = 5, by = 12 / 24))
# print(qd2$get.dosing())     # table of dosing records
print(qd2$get.nobs()) # number of observation (not dosing) records
#> [1] 34

# Note that piping with %>% will update the original table.

qd3 <- qd2 %>% add.sampling(seq(from = 5, to = 10, by = 6 / 24))
print(qd2$get.nobs())
#> [1] 34
print(qd3$get.nobs())
#> [1] 55
```
