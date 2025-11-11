# Register a method for a suggested dependency

Generally, the recommend way to register an S3 method is to use the
`S3Method()` namespace directive (often generated automatically by the
`@export` roxygen2 tag). However, this technique requires that the
generic be in an imported package, and sometimes you want to suggest a
package, and only provide a method when that package is loaded.
`s3_register()` can be called from your package's `.onLoad()` to
dynamically register a method only if the generic's package is loaded.
(To avoid taking a dependency on vctrs for this one function, please
feel free to copy and paste the function source into your own package.)

## Usage

``` r
.s3register(generic, class, method = NULL)
```

## Arguments

- generic:

  Name of the generic in the form `pkg::generic`.

- class:

  Name of the class

- method:

  Optionally, the implementation of the method. By default, this will be
  found by looking for a function called `generic.class` in the package
  environment.

  Note that providing `method` can be dangerous if you use devtools.
  When the namespace of the method is reloaded by
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html),
  the function will keep inheriting from the old namespace. This might
  cause crashes because of dangling
  [`.Call()`](https://rdrr.io/r/base/CallExternal.html) pointers.

## Value

nothing; called for side effects

## Details

For R 3.5.0 and later, `s3_register()` is also useful when demonstrating
class creation in a vignette, since method lookup no longer always
involves the lexical scope. For R 3.6.0 and later, you can achieve a
similar effect by using "delayed method registration", i.e. placing the
following in your `NAMESPACE` file:

    if (getRversion() >= "3.6.0") {
      S3method(package::generic, class)
    }

## Examples

``` r
# A typical use case is to dynamically register tibble/pillar methods
# for your class. That way you avoid creating a hard dependency on packages
# that are not essential, while still providing finer control over
# printing when they are used.

.onLoad <- function(...) {
  .s3Register("pillar::pillar_shaft", "vctrs_vctr")
  .s3Register("tibble::type_sum", "vctrs_vctr")
}
```
