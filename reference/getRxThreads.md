# Get/Set the number of threads that rxode2 uses

Get/Set the number of threads that rxode2 uses

## Usage

``` r
getRxThreads(verbose = FALSE)

setRxThreads(threads = NULL, percent = NULL, throttle = NULL)

rxCores(verbose = FALSE)
```

## Arguments

- verbose:

  Display the value of relevant OpenMP settings

- threads:

  NULL (default) rereads environment variables. 0 means to use all
  logical CPUs available. Otherwise a number \>= 1

- percent:

  If provided it should be a number between 2 and 100; the percentage of
  logical CPUs to use. By default on startup, 50 percent.

- throttle:

  2 (default) means that, roughly speaking, a single thread will be used
  when number subjects solved for is \<=2, 2 threads when the number of
  all points is \<=4, etc. The throttle is to speed up small data tasks
  (especially when repeated many times) by not incurring the overhead of
  managing multiple threads.

  The throttle will also suppress sorting which ID will be solved first
  when there are (nsubject solved)\*throttle \<= nthreads. In `rxode2`
  this sorting occurs to minimize the time for waiting for another
  thread to finish. If the last item solved is has a long solving time,
  all the other solving have to wait for that last costly solving to
  occur. If the items which are likely to take more time are solved
  first, this wait is less likely to have an impact on the overall
  solving time.

  In rxode2 the IDs are sorted by the individual number of solving
  points (largest first). It also has a C interface that allows these
  IDs to be resorted by total time spent solving the equation. This
  allows packages like nlmixr to sort by solving time if needed.

  Overall the the number of threads is throttled (restricted) for small
  tasks and sorting for IDs are suppressed.

## Value

number of threads that rxode2 uses
