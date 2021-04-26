#' Add data to plot
#'
#' Add the data used to fit the model on which the `ggeffects` grid is based.
#' Data can be residualized (with [residualize_over_grid()]) for *partial
#' residual plots* and/or collapsed across a grouping variable. See
#' [extract_fit_data()].
#'
#' @seealso [extract_fit_data()]
#'
#' @inheritParams ggplot2::layer
#' @inheritParams extract_fit_data
#'
#' @section Computed variables:
#' - The 1-to-4 terms (by their name)
#' - `predicted` - The response / predicted outcome + residuals
#'
#' @export
layer_fit_data <- function(mapping = NULL,
                           geom = "point", stat = "identity", position = "identity",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           residualize = FALSE, collapse.group = NULL, ...){

  trans_data <- function(data) {
    residualize <- force(residualize)
    collapse.group <- force(collapse.group)

    dataR <- extract_fit_data(
      object = data,
      residualize = residualize,
      collapse.group = collapse.group
    )

    dataR$conf.low <- dataR$conf.high <- NA_real_

    dataR
  }

  ggplot2::layer(
    geom = geom, stat = stat, data = trans_data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
