#' Add data to plot
#'
#' Add the data used to fit the model on which the `ggeffects` grid is based.
#' Data can be residualized (with [residualize_over_grid()]) for *partial
#' residual plots* and/or collapsed across a grouping variable (with
#' [collapse_by_group()]).
#'
#' @inheritParams ggplot2::layer
#' @param collapse.by For multilevel models, the name of the random grouping
#'   variable over which data should be aggregated prior to plotting. If `TRUE`,
#'   the first grouping variable will be used.
#' @param residuals Return partially residualized data instead of the raw
#'   data?
#'
#' @section Computed variables:
#' - The 1-to-4 terms (by their name)
#' - `predicted` - The response / predicted outcome + residuals
#'
#' @export
layer_fit_data <- function(mapping = NULL,
                           geom = "point", stat = "identity", position = "identity",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           residuals = FALSE, collapse.by = NULL, ...){

  trans_data <- function(object) {
    residuals <- force(residuals)
    collapse.by <- force(collapse.by)

    stopifnot("object must be a ggeffects grid" = inherits(object, "ggeffects"))

    if (!is.null(collapse.by)) {
      obj_name <- attr(object, "model.name", exact = TRUE)
      model <- get(obj_name, envir = parent.frame()) # globalenv()

      grid <- object
      grid$std.error <- grid$conf.low <- grid$conf.high <- NULL
      terms <- attr(object, "terms", exact = TRUE)
      xterms <- c("x", "group", "facet", "panel")
      for (trm in seq_along(terms)) {
        colnames(grid)[colnames(grid) == terms[trm]] <- xterms[trm]
      }

      dataR <- ggeffects::collapse_by_group(
        grid, model,
        collapse.by = if (is.character(collapse.by)) collapse.by,
        residuals = residuals
      )

      # Clean up
      if ("group_col" %in% colnames(dataR))
        colnames(dataR)[colnames(dataR) == "group_col"] <- "group"
      if ("response" %in% colnames(dataR))
        colnames(dataR)[colnames(dataR) == "response"] <- "predicted"
    } else if (residuals) {
      obj_name <- attr(object, "model.name", exact = TRUE)
      model <- get(obj_name, envir = parent.frame()) # globalenv()

      data2 <- as.data.frame(object)
      data2$std.error <- data2$conf.low <- data2$conf.high <- NULL

      dataR <- ggeffects::residualize_over_grid(data2, model, pred_name = "predicted")

      terms <- attr(object, "terms", exact = TRUE)
      xterms <- c("x", "group", "facet", "panel")
      for (trm in seq_along(terms)) {
        colnames(dataR)[colnames(dataR) == terms[trm]] <- xterms[trm]
      }
    } else {
      dataR <- attr(object, "rawdata", exact = TRUE)

      # Clean up
      colnames(dataR)[colnames(dataR) == "response"] <- "predicted"
    }

    # More cleanup
    dataR <- restore_names(dataR, object)

    dataR

    dataR$conf.low <- dataR$conf.high <- NA_real_

    dataR
  }

  ggplot2::layer(
    geom = geom, stat = stat, data = trans_data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# Helpers -----------------------------------------------------------------

#' @keywords internal
restore_names <- function(data, grid) {
  terms <- attr(grid, "terms", exact = TRUE)

  if (as.logical(as.numeric(attr(grid, "x.is.factor"))) &&
      !is.factor(data$x)) {
    data$x <- factor(data$x, labels = levels(grid[[terms[1]]]))
  } else if (!as.logical(as.numeric(attr(grid, "x.is.factor"))) &&
             !is.numeric(data$x)) {
    data$x <- as.numeric(as.character(data$x))
  }

  if ("group" %in% colnames(data) && attr(grid, "continuous.group")) {
    data$group <- as.numeric(as.character(data$group))
  } else if ("group" %in% colnames(data) && !is.factor(data$group)) {
    data$group <- factor(data$group, levels = levels(grid[[terms[2]]]))
  }

  if ("facet" %in% colnames(data) && !is.factor(data$facet))
    data$facet <- factor(data$facet, levels = levels(grid[[terms[3]]]))


  xterms <- c("x", "group", "facet", "panel")
  for (trm in seq_along(terms)) {
    colnames(data)[colnames(data) == xterms[trm]] <- terms[trm]
  }

  data
}
