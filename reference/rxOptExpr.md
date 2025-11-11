# Optimize rxode2 for computer evaluation

This optimizes rxode2 code for computer evaluation by only calculating
redundant expressions once.

## Usage

``` r
rxOptExpr(x, msg = "model")
```

## Arguments

- x:

  rxode2 model that can be accessed by rxNorm

- msg:

  This is the name of type of object that rxode2 is optimizing that will
  in the message when optimizing. For example "model" will produce the
  following message while optimizing the model:

  finding duplicate expressions in model...

## Value

Optimized rxode2 model text. The order and type lhs and state variables
is maintained while the evaluation is sped up. While parameters names
are maintained, their order may be modified.

## Author

Matthew L. Fidler
