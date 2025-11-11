# Check if a language object matches a template language object

- If `template == str2lang(".")`, it will match anything.

- If `template == str2lang(".name")`, it will match any name.

- If `template == str2lang(".call()")`, it will match any call.

## Usage

``` r
.matchesLangTemplate(x, template)
```

## Arguments

- x:

  The object to check

- template:

  The template object it should match

## Value

TRUE if it matches, FALSE, otherwise

## Examples

``` r
.matchesLangTemplate(str2lang("d/dt(foo)"), str2lang("d/dt(.name)"))
#> [1] TRUE
.matchesLangTemplate(str2lang("d/dt(foo)"), str2lang("d/foo(.name)"))
#> [1] FALSE
.matchesLangTemplate(str2lang("d/dt(foo)"), str2lang("d/."))
#> [1] TRUE
```
