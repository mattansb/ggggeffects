#' Basic mean / CI building blocks
#'
#' For now, these are just exported and renamed:
#' - `geom_expected_point` is [geom_point()]
#' - `geom_CI_bar` is [geom_errorbar()] / [geom_linerange()] / [geom_pointrange()] / [geom_crossbar()]
#' - `geom_expected_line` is [geom_line()]
#' - `geom_CI_ribbon` is [geom_ribbon()]
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
geom_CI_ribbon <- ggplot2::geom_ribbon
