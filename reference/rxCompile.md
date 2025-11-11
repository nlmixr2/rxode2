# Compile a model if needed

This is the compilation workhorse creating the rxode2 model DLL files.

## Usage

``` r
rxCompile(
  model,
  dir,
  prefix,
  force = FALSE,
  modName = NULL,
  package = NULL,
  ...
)

# S3 method for class 'rxModelVars'
rxCompile(
  model,
  dir = NULL,
  prefix = NULL,
  force = FALSE,
  modName = NULL,
  package = NULL,
  ...
)

# S3 method for class 'character'
rxCompile(
  model,
  dir = NULL,
  prefix = NULL,
  force = FALSE,
  modName = NULL,
  package = NULL,
  ...
)

# S3 method for class 'rxDll'
rxCompile(model, ...)

# S3 method for class 'rxode2'
rxCompile(model, ...)
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

- dir:

  This is the model directory where the C file will be stored for
  compiling.

      If unspecified, the C code is stored in a temporary directory,
      then the model is compiled and moved to the current directory.
      Afterwards the C code is removed.

      If specified, the C code is stored in the specified directory
      and then compiled in that directory.  The C code is not removed
      after the DLL is created in the same directory.  This can be
      useful to debug the c-code outputs.

- prefix:

  is a string indicating the prefix to use in the C based functions. If
  missing, it is calculated based on file name, or md5 of parsed model.

- force:

  is a boolean stating if the (re)compile should be forced if rxode2
  detects that the models are the same as already generated.

- modName:

  a string to be used as the model name. This string is used for naming
  various aspects of the computations, including generating C symbol
  names, dynamic libraries, etc. Therefore, it is necessary that
  `modName` consists of simple ASCII alphanumeric characters starting
  with a letter.

- package:

  Package name for pre-compiled binaries.

- ...:

  Other arguments sent to the
  [`rxTrans()`](https://nlmixr2.github.io/rxode2/reference/rxTrans.md)
  function.

## Value

An rxDll object that has the following components

- `dll` DLL path

- `model` model specification

- `.c` A function to call C code in the correct context from the DLL
  using the [`.C()`](https://rdrr.io/r/base/Foreign.html) function.

- `.call` A function to call C code in the correct context from the DLL
  using the [`.Call()`](https://rdrr.io/r/base/CallExternal.html)
  function.

- `args` A list of the arguments used to create the rxDll object.

## See also

[`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)

## Author

Matthew L.Fidler
