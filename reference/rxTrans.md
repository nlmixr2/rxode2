# Translate the model to C code if needed

This function translates the model to C code, if needed

## Usage

``` r
rxTrans(
  model,
  modelPrefix = "",
  md5 = "",
  modName = NULL,
  modVars = FALSE,
  ...
)

# Default S3 method
rxTrans(
  model,
  modelPrefix = "",
  md5 = "",
  modName = NULL,
  modVars = FALSE,
  ...
)

# S3 method for class 'character'
rxTrans(
  model,
  modelPrefix = "",
  md5 = "",
  modName = NULL,
  modVars = FALSE,
  ...
)
```

## Arguments

- model:

  This is the ODE model specification. It can be:

  - a string containing the set of ordinary differential equations (ODE)
    and other expressions defining the changes in the dynamic system.

  - a file name where the ODE system equation is contained

  An ODE expression enclosed in `\{\}`

  (see also the `filename` argument). For details, see the sections
  “Details” and `rxode2 Syntax` below.

- modelPrefix:

  Prefix of the model functions that will be compiled to make sure that
  multiple rxode2 objects can coexist in the same R session.

- md5:

  Is the md5 of the model before parsing, and is used to embed the md5
  into DLL, and then provide for functions like
  [`rxModelVars()`](https://nlmixr2.github.io/rxode2/reference/rxModelVars.md).

- modName:

  a string to be used as the model name. This string is used for naming
  various aspects of the computations, including generating C symbol
  names, dynamic libraries, etc. Therefore, it is necessary that
  `modName` consists of simple ASCII alphanumeric characters starting
  with a letter.

- modVars:

  returns the model variables instead of the named vector of translated
  properties.

- ...:

  Ignored parameters.

## Value

a named vector of translated model properties including what type of
jacobian is specified, the `C` function prefixes, as well as the `C`
functions names to be called through the compiled model.

## See also

[`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md),
[`rxCompile()`](https://nlmixr2.github.io/rxode2/reference/rxCompile.md).

## Author

Matthew L.Fidler
