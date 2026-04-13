ranking_plot <- function(rank_data, rank_value, x_label,
                         label_color = "black",
                         decreasing = TRUE,
                         image_col = NULL) {
  
  rank_value <- rlang::ensym(rank_value)
  
  xmin <- min(dplyr::pull(rank_data, !!rank_value), na.rm = TRUE)
  xmax <- max(dplyr::pull(rank_data, !!rank_value), na.rm = TRUE)
  
  label_position <- 0.05 * xmax
  
  df <- rank_data %>% 
    dplyr::filter(!grepl("Left the league", submitter)) %>% 
    dplyr::mutate(
      podium = if (decreasing) {
        as.factor(dplyr::dense_rank(-!!rank_value))
      } else {
        as.factor(dplyr::dense_rank(!!rank_value))
      },
      submitter = forcats::fct_reorder(
        submitter,
        if (decreasing) !!rank_value else -!!rank_value
      ),
      y_pos = as.numeric(submitter),   # <-- critical
      podium_label = dplyr::if_else(
        as.numeric(podium) < 4,
        paste0(numform::f_ordinal(as.numeric(podium)), "🏆"),
        NA_character_
      ),
      hjust = dplyr::if_else(!!rank_value < 0, 1.25, -.25)
    )
  
  p <- ggplot(df, aes(x = !!rank_value, y = submitter, fill = podium)) +
    geom_col(aes(alpha = as.numeric(podium) < 4)) +
    
    # 🏆 podium labels
    geom_text(
      aes(label = podium_label),
      x = label_position,
      color = label_color,
      hjust = 0
    ) +
    
    # values inside bars
    geom_text(
      aes(label = round(!!rank_value, 2), hjust = hjust),
      color = "white"
    )
  
  # 👉 Add images if provided
  if (!is.null(image_col)) {
    p <- p +
      ggimage::geom_image(
        aes(
          x = -xmax * 0.08,
          y = y_pos,
          image = .data[[image_col]]
        ),
        size = 0.05
      ) +
      geom_text(
        aes(
          x = -xmax * 0.03,
          y = y_pos,
          label = submitter
        ),
        hjust = 0,
        color = "white"
      ) +
      scale_y_discrete(labels = NULL)
  }
  
  p <- p +
    scale_fill_manual(
      values = c("#D4AF37", "#C0C0C0", "#B08D57", rep("#6C5CE7", 8))
    ) +
    scale_alpha_manual(values = c(.3, 1)) +
    scale_x_continuous(
      limits = c(-xmax * 0.15, xmax + xmax * .10)  # <-- space for images
    ) +
    coord_cartesian(clip = "off") +  # <-- allow drawing outside
    guides(fill = "none", alpha = "none") +
    labs(x = x_label, y = "") +
    theme_vapor() +
    theme(
      text = element_text(size = 14),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(l = 80)   # <-- avoid clipping
    )
  
  return(p)
}

score %>% 
  mutate(img = paste0("images/face/", word(submitter), ".webp")) %>% 
  ranking_plot(score, "test")