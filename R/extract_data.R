#' Extract data from a `ggeffects` object
#'
#' Extract the data used to fit the model on which the `ggeffects` grid is
#' based. Data can be residualized (with [residualize_over_grid()]) and/or
#' collapsed across a grouping variable.
#'
#' @inheritParams autoplot.ggeffects
#' @param collapse.group For multilevel models, the name of the random grouping
#'   variable over which data should be aggregated prior to plotting. If `TRUE`,
#'   the first grouping variable will be used.
#' @param residualize Return partially residualized data instead of the raw
#'   data?
#'
#' @return A data frame
#'
#' @seealso [layer_fit_data()]
#'
#' @export
extract_fit_data <- function(object, residualize = FALSE, collapse.group = NULL) {
  stopifnot("object must be a ggeffects grid" = inherits(object, "ggeffects"))

  if (isFALSE(residualize)) {
    dataR <- attr(object, "rawdata", exact = TRUE)

    # Clean up
    colnames(dataR)[colnames(dataR) == "response"] <- "predicted"
  } else {
    # Get model
    obj_name <- attr(object, "model.name", exact = TRUE)
    model <- get(obj_name, envir = parent.frame()) # globalenv()

    # Redisualize
    data2 <- as.data.frame(object)
    data2$std.error <- data2$conf.low <- data2$conf.high <- NULL
    dataR <- ggeffects::residualize_over_grid(data2, model, pred_name = "predicted")

    # Clean up
    terms <- attr(object, "terms", exact = TRUE)
    xterms <- c("x", "group", "facet", "panel")
    for (trm in seq_along(terms)) {
      colnames(dataR)[colnames(dataR) == terms[trm]] <- xterms[trm]
    }
  }

  # collapse ?
  if (!is.null(collapse.group)) {
    obj_name <- attr(object, "model.name", exact = TRUE)
    model <- get(obj_name, envir = parent.frame()) # globalenv()
    dataR <- collapse_re_data(dataR, model, collapse_re = collapse.group)
  }

  # More cleanup
  dataR <- restore_names(dataR, object)

  dataR
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

  if (length(collapse_re) > 1L) {
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
