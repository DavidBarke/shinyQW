doe_histogramm <- function() {
  toleranzbreite <- as.numeric(input$toleranzbreite)
  flugdauer <- data()$Flugdauer
  k <- floor(sqrt(length(flugdauer)))
  mean_flugdauer <- mean(flugdauer)
  sd_flugdauer <- sd(flugdauer)
  min <- min(flugdauer, na.rm = TRUE)
  max <- max(flugdauer, na.rm = TRUE)
  R <- max - min
  b <- R / k
  breaks <- min + 0:k * b
  data_norm <- data.frame(x = seq(min, max, length.out = 100)) %>%
    mutate(y = dnorm(x = x, mean = mean_flugdauer, sd = sd_flugdauer))
  plot <- ggplot(data = data.frame(Flugdauer = flugdauer)) +
    geom_histogram(mapping = aes(x = Flugdauer, y = ..density..), breaks = breaks, closed = "left", col = "mediumblue", fill = "lightblue") +
    geom_vline(xintercept = mean_flugdauer - toleranzbreite/2, col = "red", linetype = "dashed") +
    geom_vline(xintercept = mean_flugdauer + toleranzbreite/2, col = "red", linetype = "dashed") +
    geom_line(data = data_norm, mapping = aes(x = x, y = y)) +
    labs(x = "Flugdauer", y = "Relative Häufigkeitsdichte") +
    theme_bw()
  plot <- ggplotly(plot)
  return(plot)
}

doe_effect_plot <- function(data) {
  # The first column of data contains the target variable
  factors <- names(data)[-1]
  means_high <- numeric(length(factors))
  means_low <- numeric(length(factors))
  for (i in seq_along(factors)) {
    high <- data[[factors[i]]] == 1
    means_high[i] <- mean(data[high,][[1]])
    means_low[i] <- mean(data[!high,][[1]])
  }
  df <- tibble(factors = factors, high = means_high, low = means_low)
  plot <- ggplot(data = df) +
    facet_grid(facets = . ~ factors) +
    geom_segment(mapping = aes(x = -1, xend = 1, y = low, yend = high)) +
    scale_x_continuous(name = "", breaks = c(-1, 1), minor_breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous(name = label_lang(
      de = "Zielgröße",
      en = "Target variable"
    )) +
    theme_bw()
  plot <- ggplotly(plot)
  return(plot)
}