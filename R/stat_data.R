#' Add data to plot
#'
#' Add original data (`stat_raw_data()`) or residualized data
#' (`stat_residualized_data()` for partial residual plots) to the plot.
#'
#' @inheritParams ggplot2::layer
#' @param collapse.group For multilevel models, the name of the random grouping
#'   variable over which data should be aggregated prior to plotting. If `TRUE`,
#'   the first grouping variable will be used.
#' @param data Ignored.
#'
#' @section Computed variables:
#' - The 1-to-4 terms (by their name)
#' - `predicted` - The response / predicted outcome + residuals
#'
#' @export
stat_raw_data <- function(mapping = NULL, data = NULL, geom = "point",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, collapse.group = NULL, ...) {
  # data must be null
  if (!is.null(data)) warning("Ignoring 'data' arg.")

  trans_data_raw <- function(data) {
    collapse.group <- force(collapse.group)
    dataR <- attr(data, "rawdata", exact = TRUE)

    # Clean up
    dataR$predicted <- dataR$response
    dataR$response <- NULL

    # collapse ?
    if (!is.null(collapse.group)) {
      obj_name <- attr(data, "model.name", exact = TRUE)
      model <- tryCatch(get(obj_name, envir = globalenv()),
                        error = function(e) NULL)
      dataR <- collapse_re_data(dataR, model, collapse_re = collapse.group)
    }

    # More cleanup
    dataR$conf.low <- dataR$conf.high <- NA_real_
    dataR <- restore_names(dataR, data)

    dataR
  }

  layer(
    stat = StatIdentity, data = trans_data_raw, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname stat_raw_data
#' @export
stat_residualized_data <- function(mapping = NULL, data = NULL, geom = "point",
                                   position = "identity", na.rm = FALSE, show.legend = NA,
                                   inherit.aes = TRUE, collapse.group = NULL, ...) {
  # data must be null
  if (!is.null(data)) warning("Ignoring 'data' arg.")

  trans_data_residuals <- function(data) {

    collapse.group <- force(collapse.group)
    obj_name <- attr(data, "model.name", exact = TRUE)
    model <- tryCatch(get(obj_name, envir = globalenv()),
                      error = function(e) NULL)
    data2 <- as.data.frame(data)
    data2$std.error <- data2$conf.low <- data2$conf.high <- NULL
    dataR <- residualize_over_grid(data2, model, pred_name = "predicted")
    terms <- attr(data, "terms", exact = TRUE)
    xterms <- c("x", "group", "facet", "panel")
    for (trm in seq_along(terms)) {
      colnames(dataR)[colnames(dataR) == terms[trm]] <- xterms[trm]
    }

    # collapse ?
    if (!is.null(collapse.group)) {
      dataR <- collapse_re_data(dataR, model, collapse_re = collapse.group)
    }

    # More cleanup
    dataR$conf.low <- dataR$conf.high <- NA_real_
    dataR <- restore_names(dataR, data)

    dataR
  }

  layer(
    stat = StatIdentity, data = trans_data_residuals, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



# Helpers -----------------------------------------------------------------

#' @keywords internal
collapse_re_data <- function(data, model = NULL, collapse_re = TRUE) {
  if (!requireNamespace("insight")) {
    stop("This function requires the `insight` package to work. Please install it.")
  }

  # Original data
  Xdata <- insight::get_data(model)

  # Find collapse_re
  if (isTRUE(collapse_re)) {
    collapse_re <- insight::find_random(model, flatten = TRUE)
  }

  if (length(collapse_re) > 1) {
    collapse_re <- collapse_re[1]
    warning("More than one random grouping variable found.",
            "\nUsing `", collapse_re, "`.", call. = FALSE)
  }

  if (!collapse_re %in% colnames(Xdata)) {
    stop("Could not find `", collapse_re, "` column.", call. = FALSE)
  }

  # Add collapse_re to data
  data$random <- factor(Xdata[[collapse_re]])

  # collapse
  agg_data <- aggregate(data[["predicted"]],
                        by = data[colnames(data) != "predicted"],
                        FUN = mean)
  colnames(agg_data)[ncol(agg_data)] <- "predicted"


  return(agg_data)
}



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
