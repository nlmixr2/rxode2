# Get the last compiled model information as alist

Get the last compiled model information as alist

## Usage

``` r
rxLastCompile()
```

## Value

A list contains the following elements:

- `msg` the message for a bad compilation, or NULL if successful.

- `stdout` the standard output from the compilation

- `stderr` the standard error from the compilation

- `c` the code code that was used

This list will be returned invisibly, but the function will also message
the contents to the console.

## Author

Matthew L. Fidler

## Examples

``` r
rxode2({
  a <- b
})
#>  
#>  
#> rxode2 5.0.0 model named rx_be6f8608c2fb582b7780b8a94e016289 model (✔ ready). 
#> value$params: b
#> value$lhs: a
rxLastCompile()
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
```
