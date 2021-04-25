#' Basic mean / CI building blocks
#'
#' For now, these are just exported and renamed:
#' - `geom_expected_point` is [geom_point()]
#' - `geom_CI_bar` is [geom_errorbar()]
#' - `geom_expected_line` is [geom_line()]
#' - `geom_CI_ribbon` is [geom_ribbon()]
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_errorbar
#' @inheritParams ggplot2::geom_line
#' @inheritParams ggplot2::geom_ribbon
#'
#' @export
geom_expected_point <- ggplot2::geom_point

#' @rdname geom_expected_point
#' @export
geom_CI_bar <- ggplot2::geom_errorbar

#' @rdname geom_expected_point
#' @export
geom_expected_line <- ggplot2::geom_line

#' @rdname geom_expected_point
#' @export
geom_CI_ribbon <- ggplot2::geom_ribbon
