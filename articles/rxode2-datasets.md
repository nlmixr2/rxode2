# rxode2 Event Types

## Datasets for rxode2 & nlmixr

Data for input into `nlmixr` is the same type of data input for
`rxode2`, and it is similar to data for NONMEM (most NONMEM-ready
datasets can be used directly in `nlmixr`).

## Columns Described by Type of Use

### Subject Identification Columns

The subject identification column separates subjects for identification
of random effects.

- `ID`: A subject identifier that may be an integer, character, or
  factor.

### Observation Columns

Observation columns are used to indicate the dependent variable and how
to use or measure it.

- `DV`: A numeric column with the measurement
- `CENS`: A numeric column for indication of censoring, such as below
  the limit of quantification for an assay.
- `LIMIT`: A numeric column for helping indicate the type of censoring,
  such as below the limit of quantification for an assay.
- `MDV`: An indicator for missing `DV` values
- `CMT`: The name or number of the compartment
- `DVID`: The dependent variable identifier
- `EVID` The event identifier

### Dosing Columns

- `AMT`: The amount of the dose
- `CMT`: The name or number of the compartment
- `EVID`: The event identifier
- `ADDL`: The number of additional doses
- `RATE` or `DUR`: The rate or duration of a dose

### Covariate Columns

## Details for Specific Dataset Columns

The details below are sorted alphabetically by column name. For grouping
by use, see the documentation above.

### `AMT` Column

The `AMT` column defines the amount of a dose.

For observation rows, it should be `0` or `NA`.

For dosing rows, it is the amount of the dose administered to the `CMT`.
If the dose has a zero-order rate (such as a constant infusion), the
infusion may be setup using the `RATE` or `DUR` column.

### `CENS`/`LIMIT` Columns

The `CENS` column is an indicator column indicating if censoring
occurred. For pharmacokinetic modeling, censoring is typically when a
sample is below the limit of quantification. Internally `rxode2` saves
these values so that `nlmixr` can use them in likelihood calculations.

`CENS = 0` indicates that the value in `DV` is measured without
censoring.

`CENS = 1` indicates that a value is left censored (or below the limit
of quantitation) and that the value in `DV` is censoring/quantitation
limit.

`CENS = -1` indicates that a value is right censored (or above limit of
quantitation) and that the value in `DV` is censoring/quantitation
limit.

The `LIMIT` is additional information about how censoring is handled
with `nlmixr` and is stored in `rxode2`’s data structure as well. When a
value is left censored, like below a limit of `1` you may also believe
that the value is above a certain threshold, like zero. In this case, a
limit of `0` indicates that the censored value is between `0` and `1`.

In short when:

`CENS = 0` a `LIMIT` is ignored because the observation is not censored

`CENS = 1` the value is censored between (`LIMIT`, `DV`)

`CENS = -1` the value is censored between (`DV`, `LIMIT`)

### `CMT` Column

The `CMT` column indicates the compartment where an event occurs. When
given as a character string or factor (the preferred method), it is
matched by name in the model. When given as an integer, it is matched by
the order that compartments appear in the model.

### `DUR` Column

The `DUR` column defines the duration of an infusion. It is used to set
the duration of a zero-order rate of infusion.

### `DV` Column

The `DV` column indicates the current measurement in the current
compartment (see `CMT`) with the current measurement identifier (see
`DVID`) which may be missing (see `MDV`) or censored (see `CENS`).

### `DVID` Column

TODO

### `EVID` Column

The `EVID` column is the event identifier for a row of data.

For observation records, it will be `0`. For normal dosing records, it
will be `1`. Many more `EVID` values are detailed in the [rxode2 Event
Types](https://nlmixr2.github.io/rxode2/articles/rxode2-event-types.html)
and [Classic rxode2
Events](https://nlmixr2.github.io/rxode2/articles/rxode2-events-classic.html)
vignettes.

### `ID` Column

The `ID` column is a subject identifier. This column is used to separate
one individual (usually a single person or animal) from another.

In the model, the `ID` column is used to separate individuals. The
numerical integrator re-initializes with each new individual, and new
values for all random effects are selected.

### `RATE` Column

TODO
