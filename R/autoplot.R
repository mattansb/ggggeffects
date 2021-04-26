#' Create a (blank) ggplot from ggeffects
#'
#' @param object A `ggeffects` object.
#' @param mapping List of aesthetic mappings to use for plot. Defaults (which
#'   can be (individually) overridden) are:
#' - `predicted` is mapped to `y`
#' - first term is mapped to `x`
#' - second term is mapped to `group`, `color` and `fill`
#' - `conf.high`/`conf.low` are mapped onto `ymax`/`ymin`
#' @param cols,rows Specs for the facetting of the plot (See `cols` and `rows`
#'   args in [facet_grid()]). Defaults to third and forth terms, respectively.
#'   Disable facetting by setting both to `NULL`.
#' @param ... Args passed to [facet_grid()], sush as `labeller`, etc.
#'
#' @export
autoplot.ggeffects <- function(object,
                               mapping = NULL,
                               rows = waiver(), cols = waiver(),
                               ...) {
  # Clean up data ----
  if (attr(object, "continuous.group")) {
    object$group <- as.numeric(as.character(object$group))
  }

  terms <- attr(object, "terms", exact = TRUE)
  xterms <- c("x", "group", "facet", "panel")
  for (trm in seq_along(terms)) {
    colnames(object)[colnames(object) == xterms[trm]] <- terms[trm]
  }

  # AES ----
  if (length(terms) >= 2L) {
    plot_aes <- ggplot2::aes(x = .data[[terms[1]]],
                             y = .data[["predicted"]],
                             ymin = .data[["conf.low"]],
                             ymax = .data[["conf.high"]],
                             group = .data[[terms[2]]],
                             color = .data[[terms[2]]],
                             fill = .data[[terms[2]]])
  } else {
    plot_aes <- ggplot2::aes(x = .data[[terms[1]]],
                             y = .data[["predicted"]],
                             ymin = .data[["conf.low"]],
                             ymax = .data[["conf.high"]])
  }

  # Facets ----
  if (inherits(cols, "waiver")) {
    cols <- if (length(terms) >= 3L) ggplot2::vars(.data[[terms[3]]])
  }

  if (inherits(rows, "waiver")) {
    rows <- if (length(terms) >= 4L) ggplot2::vars(.data[[terms[4]]])
  }

  # Assemble ----
  ggplot2::ggplot(data = object) +
    plot_aes +
    ggplot2::facet_grid(rows = rows, cols = cols, ...) +
    mapping
}



