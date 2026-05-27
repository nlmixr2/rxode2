.createCensBox <- function(data) {
  .dat <- data
  .df <- do.call(
    rbind,
    lapply(seq_along(.dat[, 1]), function(i) {
      .xlow <- .dat$x[i] - .dat$..width[i]
      .xhi <- .dat$x[i] + .dat$..width[i]
      .low <- with(.dat, ifelse(lower[i] == ..ni[i], -Inf, lower[i]))
      .hi <- with(.dat, ifelse(upper[i] == ..pi[i], Inf, upper[i]))
      .ret <- data.frame(
        x = c(.xlow, .xlow, .xhi, .xhi),
        y = c(.low, .hi, .hi, .low),
        group = i
      )
      .ret
    })
  )
  .addCols <- setdiff(colnames(data), c(colnames(.df), "..ni", "..pi", "..delta", "..width"))
  for (.col in .addCols) {
    .df[, .col] <- rep(data[, .col], each = 4)
  }
  .df
}

.setupGroupCensBox <- function(data, params) {
  .nm <- names(data)
  .hasCens <- "cens" %in% .nm
  .hasLimit <- "limit" %in% .nm
  .hasLower <- "lower" %in% .nm
  .hasUpper <- "upper" %in% .nm
  .censSet <- .hasCens || .hasLimit
  .lowerSet <- .hasLower || .hasUpper
  if (.censSet && .lowerSet) {
    stop("stat_cens cannot mix the (cens, limit) and (lower, upper) aesthetic pairs; supply only one pair",
      call. = FALSE
    )
  }
  if (!.censSet && !.lowerSet) {
    stop("stat_cens requires either the (lower, upper) or (cens, limit) aesthetic pair",
      call. = FALSE
    )
  }
  if (.hasLimit && !.hasCens) {
    stop("stat_cens requires `cens` when the `limit` aesthetic is supplied",
      call. = FALSE
    )
  }
  if (.lowerSet && !(.hasLower && .hasUpper)) {
    stop("stat_cens requires both `lower` and `upper` aesthetics when using the (lower, upper) pair",
      call. = FALSE
    )
  }
  .dat <- data
  if (.hasCens) {
    .dat$lower <- ifelse(.dat$cens == 0, NA_real_, -Inf)
    .dat$upper <- ifelse(.dat$cens == 0, NA_real_, Inf)
    if (.hasLimit) {
      .dat$upper <- ifelse(.dat$cens == 1, .dat$y, ifelse(is.na(.dat$limit), .dat$upper, .dat$limit))
      .dat$lower <- ifelse(.dat$cens == -1, .dat$y, ifelse(is.na(.dat$limit), .dat$lower, .dat$limit))
    } else {
      .dat$upper <- ifelse(.dat$cens == 1, .dat$y, .dat$upper)
      .dat$lower <- ifelse(.dat$cens == -1, .dat$y, .dat$lower)
    }
    data <- .dat
  }
  .dat <- data[!is.na(data$lower), , drop = FALSE]
  if (length(.dat[, 1]) > 0) {
    .r <- c(data$y, data$upper[is.finite(data$upper)], data$lower[is.finite(data$lower)])
    .r <- range(.r, na.rm = TRUE)
    .rw <- .r[2] - .r[1]
    .dat$..ni <- .r[1] - 0.01 * .rw
    .dat$..pi <- .r[2] + 0.01 * .rw
    .dat$..delta <- .rw
    .r <- range(data$x, na.rm = TRUE)
    .dat$..width <- (.r[2] - .r[1]) * params$width
    .dat$lower <- ifelse(is.finite(.dat$lower), .dat$lower, .dat$..ni)
    .dat$upper <- ifelse(is.finite(.dat$upper), .dat$upper, .dat$..pi)
  }
  .dat
}

.setupCensParams <- function(data, params) {
  if (is.null(params$width)) {
    params$width <- 0.01
  }
  return(params)
}

GeomPolygonCens <- ggplot2::ggproto("GeomPolygonCens", ggplot2::GeomPolygon,
  default_aes = aes(
    colour = NA, fill = "red", size = 0.5, linetype = 1,
    alpha = 0.5
  )
)

StatCens <- ggplot2::ggproto("StatCens", ggplot2::Stat,
  setup_params = function(data, params) {
    .setupCensParams(data, params)
  },
  setup_data = function(data, params) {
    .setupGroupCensBox(data, params)
  },
  compute_panel = function(data, scales, width) {
    .createCensBox(data)
  },
  compute_group = function(data, scales, width) {
    .createCensBox(data)
  },
  required_aes = c("x", "y"),
  optional_aes = c("cens", "limit", "lower", "upper")
)

#' Censoring geom/stat
#'
#' This is a censoring geom that shows the left or right censoring specified in the nlmixr input data-set or fit
#'
#' Requires the following aesthetics:
#'
#'  - `x` represents the independent variable, often the time scale
#'  - `y` represents the dependent variable
#'
#' Plus exactly one of these aesthetic forms (the two forms cannot be mixed):
#'
#'  - `lower` and `upper` give the lower and upper bounds of the censoring
#'    interval directly. Both must be supplied. Use `-Inf`/`Inf` for an
#'    unbounded side; use `NA` on both for observed (non-censored) rows.
#'  - `cens` (with optional `limit`) gives the censoring indicator
#'    (`-1` right censored, `0` not censored, `1` left censored) and, when
#'    `limit` is also supplied, the associated limit value. Internally
#'    converted to `lower`/`upper`.
#'
#' Will add boxes representing the areas of the fit that were censored.
#'
#' @param width represents the width (in \% of range) of the
#'   censoring box
#' @inheritParams ggplot2::stat_identity
#' @return This returns a ggplot2 stat
#' @export
stat_cens <- function(mapping = NULL, data = NULL,
                      position = "identity", show.legend = NA,
                      inherit.aes = TRUE, width = 0.01, ...) {
  ggplot2::layer(
    stat = StatCens, data = data, mapping = mapping, geom = GeomPolygonCens,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = TRUE, width = width, ...)
  )
}
#' @rdname stat_cens
#' @export
geom_cens <- stat_cens
