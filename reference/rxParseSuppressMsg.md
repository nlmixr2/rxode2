# Respect suppress messages

This turns on the silent REprintf in C when
[`suppressMessages()`](https://rdrr.io/r/base/message.html) is turned
on. This makes the `REprintf` act like `messages` in R, they can be
suppressed with
[`suppressMessages()`](https://rdrr.io/r/base/message.html)

## Usage

``` r
rxParseSuppressMsg()
```

## Value

Nothing

## Author

Matthew Fidler

## Examples

``` r
# rxParseSuppressMsg() is called with rxode2()

# Note the errors are output to the console

try(rxode2parse("d/dt(matt)=/3"), silent = TRUE)
#>  

# When using suppressMessages, the output is suppressed

suppressMessages(try(rxode2parse("d/dt(matt)=/3"), silent = TRUE))

# In rxode2, we use REprintf so that interrupted threads do not crash R
# if there is a user interrupt. This isn't captured by R's messages, but
# This interface allows the `suppressMessages()` to suppress the C printing
# as well

# If you  want to suppress messages from rxode2 in other packages, you can use
# this function
```
