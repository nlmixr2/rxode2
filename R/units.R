# compatibility function for units

.setUnitsMode <- function() {
  if (requireNamespace("units", quietly = TRUE)) {
    return(units::units_options("set_units_mode"))
  } else {
    return(NULL)
  }
}

.unitless <- function() {
  if (requireNamespace("units", quietly = TRUE)) {
    return(units::unitless)
  } else {
    return(NULL)
  }
}
