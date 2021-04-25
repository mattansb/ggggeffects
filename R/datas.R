#' Add data to plot
#'
#' @inheritParams ggplot2::layer
#' @param collaps.group For multilevel models, the name of the grouping variable
#'   over which data should be aggragated prior to plotting. Can also be `TRUE`,
#'   in which case the first grouping variable will be used.
#' @param data Should only be `NULL`.
#' @export
stat_raw_data <- function(mapping = NULL, data = NULL, geom = "point",
                                   position = "identity", na.rm = FALSE, show.legend = NA,
                                   inherit.aes = TRUE, collaps.group = NULL, ...) {
  # data must be null
  if (!is.null(data)) warning("Ignoring 'data' arg.")

  trans_data_raw <- function(data) {
    collaps.group <- force(collaps.group)
    dataR <- attr(data, "rawdata", exact = TRUE)

    # Clean up
    dataR$predicted <- dataR$response
    dataR$response <- NULL

    # Collapse ?
    if (!is.null(collaps.group)) {
      obj_name <- attr(data, "model.name", exact = TRUE)
      model <- tryCatch(get(obj_name, envir = globalenv()),
                        error = function(e) NULL)
      dataR <- collaps_re_data(data, dataR, model, collaps_re = collaps.group)
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
                                   inherit.aes = TRUE, collaps.group = NULL, ...) {
  # data must be null
  if (!is.null(data)) warning("Ignoring 'data' arg.")

  trans_data_residuals <- function(data) {

    collaps.group <- force(collaps.group)
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

    # Collapse ?
    if (!is.null(collaps.group)) {
      dataR <- collaps_re_data(data, dataR, model, collaps_re = collaps.group)
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
collaps_re_data <- function(gge, data, model = NULL, collaps_re = TRUE) {
  Xdata <- insight::get_data(model)

  if (isTRUE(collaps_re)) {
    collaps_re <- insight::find_random(model, flatten = TRUE)
  }

  if (length(collaps_re) > 1) {
    collaps_re <- collaps_re[1]
    warning("More than one random grouping variable found.",
            "\nUsing `", collaps_re, "`.", call. = FALSE)
  }

  if (!collaps_re %in% colnames(Xdata)) {
    stop("Could not find `", collaps_re, "` column.", call. = FALSE)
  }

  data$random <- factor(Xdata[[collaps_re]])

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
