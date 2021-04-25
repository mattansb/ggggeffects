#' Create a (blank) ggplot from ggeffects
#'
#' @param object A `ggeffects` object.
#' @param mapping Default list of aesthetic mappings to use for plot. If not
#'   specified, default is to map `predicted` to `y`, the first term to `x` and
#'   the second term to `group`, `color` and `fill`. `conf.high`/`conf.low` are
#'   mapped onto `ymax`/`ymin`.
#' @param rows,cols Specs for the facetting of the plot. See `rows` and
#'   `cols` args in `facet_grid()` Defaults to `facet` and `panel` columns.
#'   Disable facetting by setting both to `NULL`.
#'
#'
#' @export
autoplot.ggeffects <- function(object,
                               mapping = NULL,
                               rows = waiver(), cols = waiver()) {

  if (attr(object, "continuous.group")) {
    object$group <- as.numeric(as.character(object$group))
  }

  # Clean up names
  terms <- attr(object, "terms", exact = TRUE)
  xterms <- c("x", "group", "facet", "panel")
  for (trm in seq_along(terms)) {
    colnames(object)[colnames(object) == xterms[trm]] <- terms[trm]
  }

  # AES
  if (length(terms) >= 2L) {
    plot_aes <- aes(x = .data[[terms[1]]],
                    y = .data[["predicted"]],
                    ymin = .data[["conf.low"]],
                    ymax = .data[["conf.high"]],
                    group = .data[[terms[2]]],
                    color = .data[[terms[2]]],
                    fill = .data[[terms[2]]])
  } else {
    plot_aes <- aes(x = .data[[terms[1]]],
                    y = .data[["predicted"]],
                    ymin = .data[["conf.low"]],
                    ymax = .data[["conf.high"]])
  }

  # Facets
  if (inherits(cols, "waiver")) {
    if (length(terms) >= 3L) {
      cols <- vars(.data[[terms[3]]])
    } else {
      cols <- NULL
    }
  }

  if (inherits(rows, "waiver")) {
    if (length(terms) >= 4L) {
      rows <- vars(.data[[terms[4]]])
    } else {
      rows <- NULL
    }
  }

  # Assemble
  ggplot(object) +
    plot_aes +
    facet_grid(rows, cols) +
    mapping
}



