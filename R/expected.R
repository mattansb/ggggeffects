#' Basic mean / CI building blocks
#'
#' For now, these are just exported and renamed:
#' - `geom_expected_point` is [geom_point()]
#' - `geom_CI_bar` is [geom_errorbar()] / [geom_linerange()] / [geom_pointrange()] / [geom_crossbar()]
#' - `geom_expected_line` is [geom_line()]
#' - `geom_CI_ribbon` is [geom_ribbon()] with `alpha = 0.25` and no outline (`color = NA`) - both can be overriden.
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_errorbar
#' @inheritParams ggplot2::geom_linerange
#' @inheritParams ggplot2::geom_pointrange
#' @inheritParams ggplot2::geom_crossbar
#' @inheritParams ggplot2::geom_line
#' @inheritParams ggplot2::geom_ribbon
#' @param type Which type of interval geom to use.
#'
#' @export
geom_expected_point <- ggplot2::geom_point

#' @rdname geom_expected_point
#' @export
geom_CI_bar <- function (mapping = NULL, data = NULL, stat = "identity",
                         type = c("errorbar", "linerange", "pointrange", "crossbar"),
                         position = "identity",
                         ..., na.rm = FALSE, orientation = NA, show.legend = NA,
                         inherit.aes = TRUE) {
  type <- match.arg(type)

  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = type,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, orientation = orientation, ...))
}

#' @rdname geom_expected_point
#' @export
geom_expected_line <- ggplot2::geom_line

#' @rdname geom_expected_point
#' @export
geom_CI_ribbon <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                            ..., na.rm = FALSE, orientation = NA, show.legend = NA,
                            inherit.aes = TRUE, outline.type = "both")
{
  outline.type <- match.arg(outline.type, c("both", "upper",
                                            "lower", "full"))

  params <- list(na.rm = na.rm, orientation = orientation, outline.type = outline.type, ...)

  if (!any(c("color", "colour") %in% names(mapping)) &&
      !any(c("color", "colour") %in% names(params))) {
    params$color <- NA
  }

  if (!any("alpha" %in% names(mapping)) &&
      !any("alpha" %in% names(params))) {
    params$alpha <- 0.25
  }

  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomRibbon,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = params)
}
