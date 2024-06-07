- This works with the new rxode2 parse
- This is more pendantic with the no-remap so that no-remap is everywhere :)
- This moves the ignored steady state indicators to a allocated
  integer so garbage collection will not cause gcc-USBAN alignment
  errors
