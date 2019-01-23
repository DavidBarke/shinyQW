#' Plot a histogram
#'
#' Use this function to create a simple ggplot2 histogram.
#'
#' @param data A data frame / tibble produced for example by
#' \code{\link{table_frequency_distribution}}.
#' @param frequency_density Either "absolute" or "relative".
#' @param breaks A numeric vector specifying the breaks between the histogram's
#' groups.
#' @param col,fill,alpha Graphical parameters
#'
#' @export
histogram <- function(data, frequency_density = c("absolute", "relative"),
                      breaks,
                      col, fill, alpha) {
    if (frequency_density == "relative") {
      density <- "f_j"
      y_label <- expression(tilde(f)[j])
    } else if (frequency_density == "absolute") {
      density <- "h_j"
      y_label <- expression(tilde(h)[j])
    }
    breaks_x <- breaks
    breaks_y <- data[[density]]
    plot <- ggplot(data = data) +
      geom_tile(mapping = aes(x = m_j,
                              y = data[[density]]/2,
                              width = b_j,
                              height = data[[density]]),
                col = col,
                fill = fill,
                alpha = alpha) +
      scale_x_continuous(breaks = breaks_x, labels = breaks_x) +
      scale_y_continuous(breaks = breaks_y, minor_breaks = NULL, labels = breaks_y) +
      labs(x = "", y = y_label) +
      theme_bw()
  return(plot)
}

#' Table frequency distribution
#'
#' Produce the table frequency distribution for a numeric vector.
#'
#' @param x A numeric vector.
#' @param b The group width.
#' @param k The number of groups.
#'
#' @details
#' Passing a value to each \code{b} and \code{k} implies grouping of the data.
#' @export
table_frequency_distribution <- function(x, b = NULL, k = NULL) {
  stopifnot(is.numeric(x))
  if (sum(c(is.null(b), is.null(k))) == 1) stop("Either none or both of b and
                                                k have to differ from NULL.")
  if (!is.null(b) && !is.null(k)) {
    breaks <- min(x) + 0:k * b
    groups <- cut(x, breaks = breaks, right = FALSE, ordered_result = TRUE,
                  dig.lab = 4, include.lowest = TRUE)
    data <- tibble(Klasse = levels(groups))
    data$h_j <- absolute_density_grouped_data(groups, levels(groups))
    data <- data %>%
      mutate(f_j = h_j / sum(h_j), H_x = cumsum(h_j), F_x = cumsum(f_j),
             b_j = b, m_j = ((1:k) - 0.5) * b + min(x),
             h_dichte = h_j / b_j, f_dichte = f_j / b_j)
    return(data)
  } else {

  }
}

#' Compute the absolute density for grouped data
#'
#' @param x A numeric vector.
#' @param levels Levels of a factor, usually created by \code{\link[base]{cut}}.
absolute_density_grouped_data <- function(x, levels) {
  h_j <- vector("numeric", length = length(levels))
  for (i in 1:length(levels)) {
    h_j[i] = sum(x == levels[i])
  }
  return(h_j)
}

# TODO: sortierte Daten einarbeiten, breaks aus THverteikung berechnen
#' Plot the cumulative distribution function
#'
#' Plot the cumulative distribution function using \code{ggplot2}.
#'
#'
cumulative_distribution_function <- function(tfd, breaks, col, fill, alpha, size) {
  data <- tibble(x = breaks, y = c(0, tfd$F_x))
  plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
    geom_line(col = col,
              alpha = alpha,
              size = size) +
    geom_point(col = col,
               alpha = alpha,
               size = 2 * size) +
    scale_x_continuous(breaks = c(0, breaks), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(x = "", y = expression(F(x))) +
    theme_bw()
  return(plot)
}
