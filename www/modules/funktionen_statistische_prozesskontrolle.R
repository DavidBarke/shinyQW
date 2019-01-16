A1 <- function(n) {
  # n = 5
  1.596
}

A2 <- function(n) {
  # n = 5
  .577
}

A3 <- function(n) {
  3 / (c4(n) * sqrt(n))
}

B3 <- function(n) {
  val <- 1 - ((3 / c4(n)) * sqrt(1 - c4(n) ^ 2))
  max(val, 0)
}

B4 <- function(n) {
  1 + ((3 / c4(n)) * sqrt(1 - c4(n) ^ 2)) 
}

c4 <- function(n) {
  ((2 / (n - 1)) ^ 0.5) * (gamma(n / 2) / gamma((n - 1) / 2))
}

d2 <- function(n) {
  # n = 5
  2.326
}

D3 <- function(n) {
  # n = 5
  0
}

D4 <- function(n) {
  # n = 5
  2.115
}

spc_summarise_x <- function(x, trial_names) {
  stopifnot(length(x) == 3, is.data.frame(x))
  trial_names <- as.character(trial_names)
  names(x) <- c("value", "sample", "phase")
  
  x_split <- split(x, x$phase)
  
  x_trial_df <- bind_rows(x_split[trial_names])
  x_not_trial <- x_split[-which(names(x_split) %in% trial_names)]
  
  trial_df <- x_trial_df %>%
    group_by(sample) %>%
    summarise(x_bar = mean(value), sd = sd(value))
  xbar_sd_summary <- function(x) {
    x_summary <- x %>%
      group_by(sample) %>%
      summarise(x_bar = mean(value), sd = sd(value))
  }
  not_trial_list <- map(x_not_trial, xbar_sd_summary)
  list(
    trial_df = trial_df,
    not_trial_list = not_trial_list
  )
}

spc_plot_chart <- function(
  characteristic, x, trial_df, not_trial_list, control_lines, control_lines_color
) {
  p <- plot_ly(
    data = trial_df,
    x = ~sample,
    y = trial_df[[characteristic]],
    type = "scatter", 
    mode = "lines+markers",
    name = label_lang(
      de = "Vorlaufphase",
      en = "Trial phase"
    )
  )
  
  for (i in seq_along(not_trial_list)) {
    data <- not_trial_list[[i]]
    p <- add_trace(
      p = p,
      data = data,
      inherit = FALSE,
      x = ~sample,
      y = data[[characteristic]],
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

spc_s_chart <- function(x, trial_names) {
  x_summarised <- spc_summarise_x(x, trial_names)
  trial_df <- x_summarised$trial_df
  not_trial_list <- x_summarised$not_trial_list
  
  n <- nrow(x[x$sample == x$sample[1],])
  
  control_lines <- numeric(3)
  control_lines_color <- character(3)
  # First control line is always the center line
  control_lines[1] <- mean(trial_df$sd)
  control_lines_color[1] <- "00FF00"
  control_lines[2:3] <- c(B4(n), B3(n)) * control_lines[1]
  control_lines_color[2:3] <- "FF0000"
  
  p <- spc_plot_chart(
    characteristic = "sd",
    x = x,
    trial_df = trial_df,
    not_trial_list = not_trial_list,
    control_lines = control_lines,
    control_lines_color = control_lines_color
  )
  
  return(p)
}

spc_xbar_chart <- function(x, trial_names) {
  x_summarised <- spc_summarise_x(x, trial_names)
  trial_df <- x_summarised$trial_df
  not_trial_list <- x_summarised$not_trial_list
  
  n <- nrow(x[x$sample == x$sample[1],])
  
  control_lines <- numeric(3)
  control_lines_color <- character(3)
  # First control line is always the center line
  control_lines[1] <- mean(trial_df$x_bar)
  control_lines_color[1] <- "00FF00"
  control_lines[2:3] <- control_lines[1] + 
    c(-1, 1) * A3(n) * mean(trial_df$sd)
  control_lines_color[2:3] <- "FF0000"
  
  p <- spc_plot_chart(
    characteristic = "x_bar",
    x = x,
    trial_df = trial_df,
    not_trial_list = not_trial_list,
    control_lines = control_lines,
    control_lines_color = control_lines_color
  )
  
  return(p)
}
