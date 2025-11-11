# Easy rxode2 Event Tables

## Creating rxode2’s event tables

An event table in rxode2 is a specialized data frame that acts as a
container for all of rxode2’s events and observation times.

To create an rxode2 event table you may use the code
[`eventTable()`](https://nlmixr2.github.io/rxode2/reference/eventTable.md),
[`et()`](https://nlmixr2.github.io/rxode2/reference/et.md), or even
create your own data frame with the right event information contained in
it. This is closely related to the [types of events that rxode2
supports](https://nlmixr2.github.io/rxode2/articles/rxode2-event-types.md).

``` r
library(rxode2)
```

    #> rxode2 4.1.1.9000 using 2 threads (see ?getRxThreads)
    #>   no cache: create with `rxCreateCache()`

``` r
library(units)
```

    #> udunits database from /usr/share/xml/udunits/udunits2.xml

``` r
(ev <- et())
```

    #> -- EventTable with 0 records --
    #> 0 dosing records (see x$get.dosing(); add with add.dosing or et)
    #> 0 observation times (see x$get.sampling(); add with add.sampling or et)

or

``` r
(ev <- et())
```

    #> -- EventTable with 0 records --
    #> 0 dosing records (see x$get.dosing(); add with add.dosing or et)
    #> 0 observation times (see x$get.sampling(); add with add.sampling or et)

With this event table you can add sampling/observations or doses by
piping or direct access.

This is a short table of the two main functions to create dosing

| add.dosing()    | et() | Description                         |
|-----------------|------|-------------------------------------|
| dose            | amt  | Dose/Rate/Duration amount           |
| nbr.doses       | addl | Additional doses or number of doses |
| dosing.interval | ii   | Dosing Interval                     |
| dosing.to       | cmt  | Dosing Compartment                  |
| rate            | rate | Infusion rate                       |
| start.time      | time | Dosing start time                   |
|                 | dur  | Infusion Duration                   |

Sampling times can be added with `add.sampling(` sampling times `)` or
`et(` sampling times `)`. [Dosing intervals and sampling
windows](#add-doses-and-samples-within-a-sampling-window) are also
supported.

With most of the documentation we will be using the
[`et()`](https://nlmixr2.github.io/rxode2/reference/et.md) syntax,
though both are supported.

For these models, we can illustrate by using the model shared in the
rxode2 tutorial:

``` r
## Model from rxode2 tutorial
m1 <-function() {
  ini({
    KA   <- 2.94E-01
    CL   <- 1.86E+01
    V2   <- 4.02E+01
    Q    <- 1.05E+01
    V3   <- 2.97E+02
    Kin  <- 1
    Kout <- 1
    EC50 <- 200
    ## Added modeled bioavaiblity, duration and rate
    fdepot <- 1
    durDepot <- 8
    rateDepot <- 1250
  })
  model({
    C2          <- centr/V2
    C3          <- peri/V3
    d/dt(depot) <- -KA*depot
    f(depot)    <- fdepot
    dur(depot)  <- durDepot
    rate(depot) <- rateDepot
    d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
    d/dt(peri)  <-                    Q*C2 - Q*C3
    d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff;
    eff(0)      <- 1
  })
}
```

### Adding doses to the event table

Once created you can add dosing to the event table by the
[`add.dosing()`](https://nlmixr2.github.io/rxode2/reference/add.dosing.md),
and [`et()`](https://nlmixr2.github.io/rxode2/reference/et.md)
functions.

Using the
[`add.dosing()`](https://nlmixr2.github.io/rxode2/reference/add.dosing.md)
function you have:

| argument        | meaning                                 |
|-----------------|-----------------------------------------|
| dose            | dose amount                             |
| nbr.doses       | Number of doses; Should be at least 1.  |
| dosing.interval | Dosing interval; By default this is 24. |
| dosing.to       | Compartment where dose is administered. |
| rate            | Infusion rate                           |
| start.time      | The start time of the dose              |

``` r
ev <- eventTable(amount.units="mg", time.units="hr")

## The methods are attached to the event table, so you can use 
## them directly
ev$add.dosing(dose=10000, nbr.doses = 3)# loading doses
## Starts at time 0; Default dosing interval is 24

## You can also pipe the event tables to these methods.
ev <- ev %>%
  add.dosing(dose=5000, nbr.doses=14,
             dosing.interval=12)# maintenance

ev
```

    #> -- EventTable with 2 records --
    #> 2 dosing records (see x$get.dosing(); add with add.dosing or et)
    #> 0 observation times (see x$get.sampling(); add with add.sampling or et)
    #> multiple doses in `addl` columns, expand with x$expand(); or etExpand(x)
    #> -- First part of x: --
    #> # A tibble: 2 x 5
    #>   time   amt  ii  addl evid        
    #>    [h]  [mg] [h] <int> <evid>      
    #> 1    0 10000  24     2 1:Dose (Add)
    #> 2    0  5000  12    13 1:Dose (Add)

Notice that the units were specified in the table. When specified, the
units use the `units` package to keep track of the units and convert
them if needed. Additionally, `ggforce` uses them to label the `ggplot`
axes. The `set_units` and `drop_units` are useful to set and drop the
rxode2 event table units.

In this example, you can see the time axes is labeled:

``` r
rxSolve(m1, ev) %>% plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

    #> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’

![](rxode2-event-table_files/figure-html/unnamed-chunk-9-1.png)

If you are more familiar with the NONMEM/rxode2 event records, you can
also specify dosing using `et` with the dose elements directly:

``` r
ev <- et(timeUnits="hr") %>%
  et(amt=10000, until = set_units(3, days),
     ii=12) # loading doses

ev
```

    #> -- EventTable with 1 records --
    #> 1 dosing records (see x$get.dosing(); add with add.dosing or et)
    #> 0 observation times (see x$get.sampling(); add with add.sampling or et)
    #> multiple doses in `addl` columns, expand with x$expand(); or etExpand(x)
    #> -- First part of x: --
    #> # A tibble: 1 x 5
    #>   time   amt  ii  addl evid        
    #>    [h] <dbl> [h] <int> <evid>      
    #> 1    0 10000  12     6 1:Dose (Add)

Which gives:

``` r
rxSolve(m1, ev) %>% plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

    #> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’

![](rxode2-event-table_files/figure-html/unnamed-chunk-11-1.png)

This shows how easy creating event tables can be.

### Adding sampling to an event table

If you notice in the above examples, rxode2 generated some default
sampling times since there was not any sampling times. If you wish more
control over the sampling time, you should add the samples to the rxode2
event table by `add.sampling` or `et`

``` r
ev <- eventTable(amount.units="mg", time.units="hr")

## The methods are attached to the event table, so you can use them
## directly
ev$add.dosing(dose=10000, nbr.doses = 3)# loading doses

ev$add.sampling(seq(0,24,by=4))

ev
```

    #> -- EventTable with 8 records --
    #> 1 dosing records (see x$get.dosing(); add with add.dosing or et)
    #> 7 observation times (see x$get.sampling(); add with add.sampling or et)
    #> multiple doses in `addl` columns, expand with x$expand(); or etExpand(x)
    #> -- First part of x: --
    #> # A tibble: 8 x 5
    #>   time   amt  ii  addl evid         
    #>    [h]  [mg] [h] <int> <evid>       
    #> 1    0    NA  NA    NA 0:Observation
    #> 2    0 10000  24     2 1:Dose (Add) 
    #> 3    4    NA  NA    NA 0:Observation
    #> 4    8    NA  NA    NA 0:Observation
    #> 5   12    NA  NA    NA 0:Observation
    #> 6   16    NA  NA    NA 0:Observation
    #> 7   20    NA  NA    NA 0:Observation
    #> 8   24    NA  NA    NA 0:Observation

Which gives:

``` r
solve(m1, ev) %>% plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-13-1.png)

Or if you use `et` you can simply add them in a similar way to
`add.sampling`:

``` r
ev <- et(timeUnits="hr") %>%
  et(amt=10000, until = set_units(3, days),
     ii=12) %>% # loading doses
  et(seq(0,24,by=4))

ev
```

    #> -- EventTable with 8 records --
    #> 1 dosing records (see x$get.dosing(); add with add.dosing or et)
    #> 7 observation times (see x$get.sampling(); add with add.sampling or et)
    #> multiple doses in `addl` columns, expand with x$expand(); or etExpand(x)
    #> -- First part of x: --
    #> # A tibble: 8 x 5
    #>   time   amt  ii  addl evid         
    #>    [h] <dbl> [h] <int> <evid>       
    #> 1    0    NA  NA    NA 0:Observation
    #> 2    0 10000  12     6 1:Dose (Add) 
    #> 3    4    NA  NA    NA 0:Observation
    #> 4    8    NA  NA    NA 0:Observation
    #> 5   12    NA  NA    NA 0:Observation
    #> 6   16    NA  NA    NA 0:Observation
    #> 7   20    NA  NA    NA 0:Observation
    #> 8   24    NA  NA    NA 0:Observation

which gives the following rxode2 solve:

``` r
solve(m1, ev) %>% plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-15-1.png)

Note the jagged nature of these plots since there was only a few sample
times.

### Expand the event table to a multi-subject event table.

The only thing that is needed to expand an event table is a list of IDs
that you want to expand;

``` r
ev <- et(timeUnits="hr") %>%
  et(amt=10000, until = set_units(3, days),
     ii=12) %>% # loading doses
  et(seq(0,48,length.out=200)) %>%
  et(id=1:4)

ev
```

    #> -- EventTable with 804 records --
    #> 4 dosing records (see x$get.dosing(); add with add.dosing or et)
    #> 800 observation times (see x$get.sampling(); add with add.sampling or et)
    #> multiple doses in `addl` columns, expand with x$expand(); or etExpand(x)
    #> -- First part of x: --
    #> # A tibble: 804 x 6
    #>       id  time   amt  ii  addl evid         
    #>    <int>   [h] <dbl> [h] <int> <evid>       
    #>  1     1 0        NA  NA    NA 0:Observation
    #>  2     1 0     10000  12     6 1:Dose (Add) 
    #>  3     1 0.241    NA  NA    NA 0:Observation
    #>  4     1 0.482    NA  NA    NA 0:Observation
    #>  5     1 0.724    NA  NA    NA 0:Observation
    #>  6     1 0.965    NA  NA    NA 0:Observation
    #>  7     1 1.21     NA  NA    NA 0:Observation
    #>  8     1 1.45     NA  NA    NA 0:Observation
    #>  9     1 1.69     NA  NA    NA 0:Observation
    #> 10     1 1.93     NA  NA    NA 0:Observation
    #> # i 794 more rows

You can see in the following simulation there are 4 individuals that are
solved for:

``` r
set.seed(42)
rxSetSeed(42)
solve(m1, ev,
      params=data.frame(KA=0.294*exp(rnorm(4)),
                        CL=18.6*exp(rnorm(4)))) %>%
    plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-17-1.png)

### Add doses and samples within a sampling window

In addition to adding fixed doses and fixed sampling times, you can have
windows where you sample and draw doses from. For dosing windows you
specify the time as an ordered numerical vector with the lowest dosing
time and the highest dosing time inside a list.

In this example, you start with a dosing time with a 6 hour dosing
window:

``` r
set.seed(42)
rxSetSeed(42)
ev <- et(timeUnits="hr") %>%
  et(time=list(c(0,6)), amt=10000, until = set_units(2, days),
     ii=12) %>% # loading doses
  et(id=1:4)

ev
```

    #> -- EventTable with 16 records --
    #> 0 dosing records (see x$get.dosing(); add with add.dosing or et)
    #> 16 observation times (see x$get.sampling(); add with add.sampling or et)
    #> -- First part of x: --
    #> # A tibble: 16 x 6
    #>       id low   time high   amt evid        
    #>    <int> [h]    [h]  [h] <dbl> <evid>      
    #>  1     1   0  5.49     6 10000 1:Dose (Add)
    #>  2     1  12 17.0     18 10000 1:Dose (Add)
    #>  3     1  24 25.7     30 10000 1:Dose (Add)
    #>  4     1  36 41.6     42 10000 1:Dose (Add)
    #>  5     2   0  4.31     6 10000 1:Dose (Add)
    #>  6     2  12 14.7     18 10000 1:Dose (Add)
    #>  7     2  24 28.2     30 10000 1:Dose (Add)
    #>  8     2  36 39.9     42 10000 1:Dose (Add)
    #>  9     3   0  0.808    6 10000 1:Dose (Add)
    #> 10     3  12 16.4     18 10000 1:Dose (Add)
    #> 11     3  24 27.1     30 10000 1:Dose (Add)
    #> 12     3  36 39.9     42 10000 1:Dose (Add)
    #> 13     4   0  4.98     6 10000 1:Dose (Add)
    #> 14     4  12 13.7     18 10000 1:Dose (Add)
    #> 15     4  24 29.6     30 10000 1:Dose (Add)
    #> 16     4  36 41.5     42 10000 1:Dose (Add)

You can clearly see different dosing times in the following simulation:

``` r
ev <- ev %>% et(seq(0,48,length.out=200))

solve(m1, ev,
      params=data.frame(KA=0.294*exp(rnorm(4)),
                        CL=18.6*exp(rnorm(4)))) %>%
  plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-19-1.png)

Of course in reality the dosing interval may only be 2 hours:

``` r
set.seed(42)
rxSetSeed(42)
ev <- et(timeUnits="hr") %>%
  et(time=list(c(0,2)), amt=10000, until = set_units(2, days),
     ii=12) %>% # loading doses
  et(id=1:4) %>%
  et(seq(0,48,length.out=200))

solve(m1, ev,
      params=data.frame(KA=0.294*exp(rnorm(4)),
                        CL=18.6*exp(rnorm(4)))) %>%
  plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-20-1.png)

The same sort of thing can be specified with sampling times. To specify
the sampling times in terms of a sampling window, you can create a list
of the sampling times. Each sampling time will be a two element ordered
numeric vector.

``` r
rxSetSeed(42)
set.seed(42)
ev <- et(timeUnits="hr") %>%
  et(time=list(c(0,2)), amt=10000, until = set_units(2, days),
     ii=12) %>% # loading doses
  et(id=1:4)

## Create 20 samples in the first 24 hours and 20 samples in the
## second 24 hours
samples <- c(lapply(1:20, function(...){c(0,24)}),
             lapply(1:20, function(...){c(20,48)}))

## Add the random collection to the event table
ev <- ev %>% et(samples)

library(ggplot2)
solve(m1, ev, params=data.frame(KA=0.294*exp(rnorm(4)),
                                CL=18.6*exp(rnorm(4)))) %>%
  plot(C2) + geom_point()
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-21-1.png)

This shows the flexibility in dosing and sampling that the rxode2 event
tables allow.

### Combining event tables

Since you can create dosing records and sampling records, you can create
any complex dosing regimen you wish. In addition, rxode2 allows you to
combine event tables by `c`, `seq`, `rep`, and `rbind`.

### Sequencing event tables

One way to combine event table is to sequence them by `c`, `seq` or
`etSeq`. This takes the two dosing groups and adds at least one
inter-dose interval between them:

``` r
## bid for 5 days
bid <- et(timeUnits="hr") %>%
       et(amt=10000,ii=12,until=set_units(5, "days"))

## qd for 5 days
qd <- et(timeUnits="hr") %>%
      et(amt=20000,ii=24,until=set_units(5, "days"))

## bid for 5 days followed by qd for 5 days
et <- seq(bid,qd) %>% et(seq(0,11*24,length.out=100));

rxSolve(m1, et) %>% plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-22-1.png)

When sequencing events, you can also separate this sequence by a period
of time; For example if you wanted to separate this by a week, you could
easily do that with the following sequence of event tables:

``` r
## bid for 5 days followed by qd for 5 days
et <- seq(bid,set_units(1, "week"), qd) %>%
    et(seq(0,18*24,length.out=100));

rxSolve(m1, et) %>% plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-23-1.png)

Note that in this example the time between the bid and the qd event
tables is exactly one week, not 1 week plus 24 hours because of the
inter-dose interval. If you want that behavior, you can sequence it
using the `wait="+ii"`.

``` r
## bid for 5 days followed by qd for 5 days
et <- seq(bid,set_units(1, "week"), qd,wait="+ii") %>%
    et(seq(0,18*24,length.out=100));

rxSolve(m1, et) %>% plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-24-1.png)

Also note, that rxode2 assumes that the dosing is what you want to space
the event tables by, and clears out any sampling records when you
combine the event tables. If that is not true, you can also use the
option `samples="use"`

### Repeating event tables

You can have an event table that you can repeat with `etRep` or `rep`.
For example 4 rounds of 2 weeks on QD therapy and 1 week off of therapy
can be simply specified:

``` r
qd <-et(timeUnits = "hr") %>%
  et(amt=10000, ii=24, until=set_units(2, "weeks"), cmt="depot")

et <- rep(qd, times=4, wait=set_units(1,"weeks")) %>%
      add.sampling(set_units(seq(0, 12.5,by=0.005),weeks))

rxSolve(m1, et)  %>% plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-25-1.png)

This is a simplified way to use a sequence of event tables. Therefore,
many of the same options still apply; That is `samples` are cleared
unless you use `samples="use"`, and the time between event tables is at
least the inter-dose interval. You can adjust the timing by the `wait`
option.

### Combining event tables with rbind

You may combine event tables with `rbind`. This does not consider the
event times when combining the event tables, but keeps them the same
times. If you space the event tables by a waiting period, it also does
not consider the inter-dose interval.

Using the previous `seq` you can clearly see the difference. Here was
the sequence:

``` r
## bid for 5 days
bid <- et(timeUnits="hr") %>%
       et(amt=10000,ii=12,until=set_units(5, "days"))

## qd for 5 days
qd <- et(timeUnits="hr") %>%
      et(amt=20000,ii=24,until=set_units(5, "days"))

et <- seq(bid,qd) %>%
    et(seq(0,18*24,length.out=500));

rxSolve(m1, et) %>% plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-26-1.png)

But if you bind them together with `rbind`

``` r
## bid for 5 days
et <- rbind(bid,qd) %>%
    et(seq(0,18*24,length.out=500));

rxSolve(m1, et) %>% plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-27-1.png)

Still the waiting period applies (but does not consider the inter-dose
interval)

``` r
et <- rbind(bid,wait=set_units(10,days),qd) %>%
    et(seq(0,18*24,length.out=500));

rxSolve(m1, et) %>% plot(C2)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-28-1.png)

You can also bind the tables together and make each ID in the event
table unique; This can be good to combine cohorts with different
expected dosing and sampling times. This requires the `id="unique"`
option; Using the first example shows how this is different in this
case:

``` r
## bid for 5 days
et <- etRbind(bid,qd, id="unique") %>%
    et(seq(0,150,length.out=500));

library(ggplot2)
rxSolve(m1, et) %>% plot(C2) + facet_wrap( ~ id)
```

    #> i parameter labels from comments are typically ignored in non-interactive mode

    #> i Need to run with the source intact to parse comments

![](rxode2-event-table_files/figure-html/unnamed-chunk-29-1.png)

### Event tables in Rstudio Notebooks

In addition to the output in the console which has been shown in the
above examples, Rstudio notebook output is different and can be seen in
the following screenshots;

The first screenshot shows how the event table looks after evaluating it
in the Rstduio notebook

![](eventTable-output-notebook-02.png)

This is a simple dataframe that allows you to page through the contents.
If you click on the first box in the Rstudio notebook output, it will
have the notes about the event table:

![](eventTable-output-notebook-01.png) \## Expanding events

Event tables can be expanded so they contain an `addl` data item, like
the following example:

``` r
ev <- et() %>%
  et(dose=50, ii=8, until=48)

ev
```

    #> -- EventTable with 1 records --
    #> 1 dosing records (see x$get.dosing(); add with add.dosing or et)
    #> 0 observation times (see x$get.sampling(); add with add.sampling or et)
    #> multiple doses in `addl` columns, expand with x$expand(); or etExpand(x)
    #> -- First part of x: --
    #> # A tibble: 1 x 5
    #>    time   amt    ii  addl evid        
    #>   <dbl> <dbl> <dbl> <int> <evid>      
    #> 1     0    50     8     6 1:Dose (Add)

You can expand the events so they do not have the `addl` items by
`$expand()` or `etExpand(ev)`:

The first, `etExpand(ev)` expands the event table without modifying the
original data frame:

``` r
etExpand(ev)
```

    #> -- EventTable with 7 records --
    #> 7 dosing records (see x$get.dosing(); add with add.dosing or et)
    #> 0 observation times (see x$get.sampling(); add with add.sampling or et)
    #> -- First part of x: --
    #> # A tibble: 7 x 4
    #>    time   amt    ii evid        
    #>   <dbl> <dbl> <dbl> <evid>      
    #> 1     0    50     0 1:Dose (Add)
    #> 2     8    50     0 1:Dose (Add)
    #> 3    16    50     0 1:Dose (Add)
    #> 4    24    50     0 1:Dose (Add)
    #> 5    32    50     0 1:Dose (Add)
    #> 6    40    50     0 1:Dose (Add)
    #> 7    48    50     0 1:Dose (Add)

You can see the `addl` events were expanded, however the original data
frame remained intact:

``` r
print(ev)
```

    #> -- EventTable with 1 records --
    #> 1 dosing records (see $get.dosing(); add with add.dosing or et)
    #> 0 observation times (see $get.sampling(); add with add.sampling or et)
    #> multiple doses in `addl` columns, expand with $expand(); or etExpand()
    #> -- First part of : --
    #> # A tibble: 1 x 5
    #>    time   amt    ii  addl evid        
    #>   <dbl> <dbl> <dbl> <int> <evid>      
    #> 1     0    50     8     6 1:Dose (Add)

If you use `ev$expand()` it will modify the `ev` object. This is similar
to an object-oriented method:

``` r
ev$expand()
ev
```

    #> -- EventTable with 7 records --
    #> 7 dosing records (see x$get.dosing(); add with add.dosing or et)
    #> 0 observation times (see x$get.sampling(); add with add.sampling or et)
    #> -- First part of x: --
    #> # A tibble: 7 x 4
    #>    time   amt    ii evid        
    #>   <dbl> <dbl> <dbl> <evid>      
    #> 1     0    50     0 1:Dose (Add)
    #> 2     8    50     0 1:Dose (Add)
    #> 3    16    50     0 1:Dose (Add)
    #> 4    24    50     0 1:Dose (Add)
    #> 5    32    50     0 1:Dose (Add)
    #> 6    40    50     0 1:Dose (Add)
    #> 7    48    50     0 1:Dose (Add)
