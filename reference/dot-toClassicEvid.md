# This converts NONMEM-style EVIDs to classic RxODE events

This converts NONMEM-style EVIDs to classic RxODE events

## Usage

``` r
.toClassicEvid(cmt = 1L, amt = 0, rate = 0, dur = 0, ii = 0, evid = 0L, ss = 0)
```

## Arguments

- cmt:

  compartment flag

- amt:

  dose amount

- rate:

  dose rate

- dur:

  dose duration

- ii:

  inter-dose interval

- evid:

  event id

- ss:

  steady state

## Value

classic evids, excluding evids that are added (you need to add them
manually) or simply use etTran. This is mostly for testing and really
shouldn't be used directly.

## Author

Matthew L. Fidler

## Examples

``` r
.toClassicEvid(cmt=10, amt=3, evid=1)
#> [1] 1001
.toClassicEvid(cmt=10, amt=3, rate=2, evid=1)
#> [1] 11001
.toClassicEvid(cmt=10, amt=3, rate=-1, evid=1)
#> [1] 91001
.toClassicEvid(cmt=10, amt=3, rate=-2, evid=1)
#> [1] 81001
.toClassicEvid(cmt=10, amt=3, dur=2, evid=1)
#> [1] 21001
.toClassicEvid(cmt=304, amt=3, dur=2, evid=1)
#> [1] 320401
.toClassicEvid(cmt=7, amt=0, rate=2, evid=1, ss=1)
#> [1] 10740
.toClassicEvid(cmt=-10, amt=3, evid=1)
#> [1] 1030
.toClassicEvid(cmt=10, amt=3, evid=5)
#> [1] 41001
.toClassicEvid(cmt=6, amt=3, evid=6)
#> [1] 50601
.toClassicEvid(cmt=6, amt=3, evid=7)
#> [1] 650
.toClassicEvid(evid=2)
#> [1] 2
.toClassicEvid(evid=4)
#> [1] 101
```
