c4 <- function(n) {
  ((2 / (n - 1)) ^ 0.5) * (gamma(n / 2) / gamma((n - 1) / 2))
}

A3 <- function(n) {
  3 / (c4(n) * sqrt(n))
}

plotly_xbar_chart <- function(x, trial_names) {
  stopifnot(length(x) == 3, is.data.frame(x))
  trial_names <- as.character(trial_names)
  names(x) <- c("value", "sample", "phase")
  
  x_split <- split(x, x$phase)
  
  x_trial_df <- bind_rows(x_split[trial_names])
  x_not_trial <- x_split[-which(names(x_split) %in% trial_names)]
  
  x_trial_summary_df <- x_trial_df %>%
    group_by(sample) %>%
    summarise(x_bar = mean(value), sd = sd(value))
  xbar_sd_summary <- function(x) {
    x_summary <- x %>%
      group_by(sample) %>%
      summarise(x_bar = mean(value), sd = sd(value))
  }
  x_not_trial_summary <- map(x_not_trial, xbar_sd_summary)
  
  control_lines <- numeric(3)
  control_lines_color <- character(3)
  control_lines[1] <- mean(x_trial_summary_df$x_bar)
  control_lines_color[1] <- "00FF00"
  control_lines[2:3] <- control_lines[1] + 
    c(-1, 1) * A3(n = nrow(x_trial_summary_df)) * mean(x_trial_summary_df$sd)
  control_lines_color[2:3] <- "FF0000"
  
  p <- plot_ly(
    data = x_trial_summary_df,
    x = ~sample,
    y = ~x_bar,
    type = "scatter", 
    mode = "lines+markers",
    name = label_lang(
      de = "Vorlaufphase",
      en = "Trial phase"
    )
  )
  
  for (i in seq_along(x_not_trial_summary)) {
    data <- x_not_trial_summary[[i]]
    p <- add_trace(
      p = p,
      data = data,
      inherit = FALSE,
      x = ~sample,
      y = ~x_bar,
      type = "scatter",
      mode = "lines+markers",
      name = label_lang(
        de = paste0("Operationsphase ", i),
        en = paste0("Operation phase ", i)
      )
    )
  }
  
  for (i in seq_along(control_lines)) {
    p <- add_trace(
      p = p,
      type = "scatter",
      mode = "lines",
      x = c(min(x$sample), max(x$sample)),
      y = control_lines[i],
      line = list(color = control_lines_color[i]),
      showlegend = FALSE,
      inherit = FALSE
    )
  }
  
  return(p)
}
