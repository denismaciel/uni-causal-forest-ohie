export_entropy_figure <- function(output_dir = "figs") {
  p_c <- seq(0, 1, length.out = 100)
  p_d <- 1 - p_c

  entropy <- -p_c * log(p_c) - p_d * log(p_d)

  df <- tibble::tibble(entropy, p_c) |>
    dplyr::mutate(
      entropy = ifelse(is.nan(.data$entropy), 0, .data$entropy),
      single_node_entropy = -p_c * log(p_c),
      single_node_entropy = ifelse(
        is.nan(.data$single_node_entropy),
        0,
        .data$single_node_entropy
      )
    )

  node_entropy <- df |>
    ggplot2::ggplot(ggplot2::aes(.data$p_c, .data$entropy)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      y = "Node Entropy",
      x = "Share of Creditworthy Applicants"
    ) +
    ggplot2::expand_limits(y = 0:1) +
    ggplot2::theme_light()

  partial_entropy <- df |>
    ggplot2::ggplot(ggplot2::aes(x = .data$p_c, .data$single_node_entropy)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      y = "Partial Entropy",
      x = "Share of Creditworthy Applicants"
    ) +
    ggplot2::expand_limits(y = 0:1) +
    ggplot2::theme_light()

  ggplot2::ggsave(
    filename = file.path(output_dir, "entropy.png"),
    plot = patchwork::wrap_plots(node_entropy, partial_entropy),
    width = 12,
    height = 6,
    dpi = 300
  )
}

export_rank_figure <- function(predictions, output_dir = "figs") {
  plot <- predictions |>
    dplyr::arrange(.data$predictions) |>
    dplyr::mutate(idx = dplyr::row_number()) |>
    ggplot2::ggplot(ggplot2::aes(.data$idx, .data$predictions)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "red") +
    ggplot2::labs(x = "Rank", y = "Estimated Treatment Effect") +
    ggplot2::theme_light()

  ggplot2::ggsave(
    filename = file.path(output_dir, "ete_p_rank.png"),
    plot = plot,
    width = 7,
    height = 5,
    dpi = 300
  )
}

export_interval_figure <- function(predictions, output_dir = "figs", seed = 123, sample_size = 200, z = 1.96) {
  set.seed(seed)
  sampled <- dplyr::slice_sample(predictions, n = sample_size)

  plot <- sampled |>
    ggplot2::ggplot(ggplot2::aes(rank(.data$predictions), .data$predictions)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$predictions + z * sqrt(.data$variance.estimates),
        ymax = .data$predictions - z * sqrt(.data$variance.estimates)
      )
    ) +
    ggplot2::labs(x = "Rank", y = "Estimated Treatment Effect") +
    ggplot2::theme_light()

  ggplot2::ggsave(
    filename = file.path(output_dir, "ete_p_rank_interval.png"),
    plot = plot,
    width = 7,
    height = 5,
    dpi = 300
  )
}

export_histogram_figure <- function(predictions, model_data, output_dir = "figs") {
  histogram_data <- predictions |>
    dplyr::bind_cols(model_data$test) |>
    dplyr::mutate(bins = cut(.data$predictions, breaks = seq(-10, 10, 0.2))) |>
    dplyr::count(.data$bins)

  zero_point <- which(histogram_data$bins == "(0,0.2]") - 0.5

  plot <- histogram_data |>
    ggplot2::ggplot(ggplot2::aes(.data$bins, .data$n)) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = zero_point, color = "red") +
    ggplot2::labs(x = "Bins", y = "") +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

  ggplot2::ggsave(
    filename = file.path(output_dir, "ete_histogram.png"),
    plot = plot,
    width = 7,
    height = 5,
    dpi = 300
  )
}

export_analysis_figures <- function(predictions, model_data, output_dir = "figs", seed = 123) {
  fs::dir_create(output_dir)

  export_entropy_figure(output_dir)
  export_rank_figure(predictions, output_dir)
  export_interval_figure(predictions, output_dir, seed)
  export_histogram_figure(predictions, model_data, output_dir)
}
