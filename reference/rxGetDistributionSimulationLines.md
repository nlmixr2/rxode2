# This is a S3 method for getting the distribution lines for a rxode2 simulation

This is a S3 method for getting the distribution lines for a rxode2
simulation

## Usage

``` r
rxGetDistributionSimulationLines(line)

# S3 method for class 'norm'
rxGetDistributionSimulationLines(line)

# S3 method for class 'dnorm'
rxGetDistributionSimulationLines(line)

# S3 method for class 't'
rxGetDistributionSimulationLines(line)

# S3 method for class 'cauchy'
rxGetDistributionSimulationLines(line)

# S3 method for class 'ordinal'
rxGetDistributionSimulationLines(line)

# Default S3 method
rxGetDistributionSimulationLines(line)

# S3 method for class 'rxUi'
rxGetDistributionSimulationLines(line)
```

## Arguments

- line:

  Parsed rxode2 model environment

## Value

Lines for the simulation of `ipred` and `dv`. This is based on the idea
that the focei parameters are defined

## Author

Matthew Fidler
