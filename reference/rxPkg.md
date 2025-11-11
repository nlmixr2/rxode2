# Creates a package from compiled rxode2 models

Creates a package from compiled rxode2 models

## Usage

``` r
rxPkg(
  ...,
  package,
  wd = getwd(),
  action = c("install", "build", "binary", "create"),
  license = c("gpl3", "lgpl", "mit", "agpl3"),
  name = "Firstname Lastname",
  fields = list()
)
```

## Arguments

- ...:

  Models to build a package from

- package:

  String of the package name to create

- wd:

  character string with a working directory where to create a
  subdirectory according to `modName`. When specified, a subdirectory
  named after the “`modName.d`” will be created and populated with a C
  file, a dynamic loading library, plus various other working files. If
  missing, the files are created (and removed) in the temporary
  directory, and the rxode2 DLL for the model is created in the current
  directory named `rx_????_platform`, for example
  `rx_129f8f97fb94a87ca49ca8dafe691e1e_i386.dll`

- action:

  Type of action to take after package is created

- license:

  is the type of license for the package.

- name:

  Full name of author

- fields:

  A named list of fields to add to `DESCRIPTION`, potentially overriding
  default values. See
  [`use_description()`](https://usethis.r-lib.org/reference/use_description.html)
  for how you can set personalized defaults using package options.

## Value

this function returns nothing and is used for its side effects

## Author

Matthew Fidler
