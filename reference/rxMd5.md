# Return the md5 of an rxode2 object or file

This md5 is based on the model and possibly the extra c code supplied
for the model. In addition the md5 is based on syntax options, compiled
rxode2 library md5, and the rxode2 version/repository.

## Usage

``` r
rxMd5(model, ...)
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

- ...:

  ignored arguments

## Value

If this is a rxode2 object, return a named list:

- `file_md5` is the model's file's md5

- `parsed_md5` is the parsed model's file's md5.

Otherwise return the md5 based on the arguments provided

## Author

Matthew L.Fidler
