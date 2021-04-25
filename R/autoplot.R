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
  terms <- attr(object, "terms", exact = TRUE)

  plot_aes <- aes(x = .data[[terms[1]]],
                  y = .data[["predicted"]],
                  ymin = .data[["conf.low"]],
                  ymax = .data[["conf.high"]])
  colnames(object)[colnames(object) == "x"] <- terms[1]

  if (length(terms) > 1) {
    colnames(object)[colnames(object) == "group"] <- terms[2]
    plot_aes <- aes(x = .data[[terms[1]]],
                    y = .data[["predicted"]],
                    ymin = .data[["conf.low"]],
                    ymax = .data[["conf.high"]],
                    group = .data[[terms[2]]],
                    color = .data[[terms[2]]],
                    fill = .data[[terms[2]]])
  }

  if (length(terms) > 2) {
    if (inherits(cols, "waiver")) cols <- vars(.data[[terms[3]]])
    colnames(object)[colnames(object) == "facet"] <- terms[3]
  }

  if (length(terms) > 3) {
    if (inherits(rows, "waiver")) rows <- vars(.data[[terms[4]]])
    colnames(object)[colnames(object) == "panel"] <- terms[4]
  }

  if (inherits(rows, "waiver")) rows <- NULL
  if (inherits(cols, "waiver")) cols <- NULL

  ggplot(object) +
    plot_aes +
    facet_grid(rows, cols) +
    mapping
}



