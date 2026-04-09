library(tidyverse)
all_votes <- readRDS("all_votes.rds")
# all_votes <- all_votes %>% filter(!grepl("Left the league", submitter))

ranking_plot <- function(rank_data, rank_value, x_label, decreasing = TRUE) {
  
  rank_value <- rlang::ensym(rank_value)
  
  xmin <- min(pull(rank_data, !!rank_value), na.rm = TRUE)
  xmax <- max(pull(rank_data, !!rank_value), na.rm = TRUE)
  x_range <- xmax - xmin
  
  label_position <- 0.05 * xmax
  label_color <- if (any(rank_data[[rank_value]] < 0)) "white" else "black"
  
  p <- rank_data %>% 
    filter(!grepl("Left the league", submitter)) %>% 
    mutate(
      podium = if (decreasing) {
        as.factor(dense_rank(-!!rank_value))
      } else {
        as.factor(dense_rank(!!rank_value))
      },
      submitter = fct_reorder(
        submitter,
        if (decreasing) !!rank_value else -!!rank_value
      ),
      podium_label = if_else(
        as.numeric(podium) < 4,
        paste0(numform::f_ordinal(as.numeric(podium)), "🏆"),
        NA_character_
      ),
      hjust = if_else(!!rank_value < 0, 1.25, -.25),
      color = if_else(any(!!rank_value < 0), "white", "black")
    ) %>% 
    
    ggplot(aes(x = !!rank_value, y = submitter, fill = podium)) +
    geom_col(aes(alpha = as.numeric(podium) < 4)) +
    # 🏆 podium label (left, fixed but safe)
    geom_text(
      aes(label = podium_label),
      x = label_position,
      color = label_color,
      hjust = 0
    ) +
    geom_text(
      aes(label = round(!!rank_value, 2), hjust = hjust),
      color = "white"
    ) +

    scale_fill_manual(
      values = c("#D4AF37", "#C0C0C0", "#B08D57", rep("#6C5CE7", 8))
    ) +
    scale_alpha_manual(values = c(.3, 1)) +
    scale_x_continuous(limits = c(0, xmax + xmax*.10)) +
    coord_cartesian(clip = "on") +
    guides(fill = "none", alpha = "none") +
    labs(x = x_label, y = "") +
    theme_vapor() +
    theme(text = element_text(size = 14))
  
  return(p)
}

# Sum of points 
# Winner: Captnmeaty
score <- 
all_votes %>% 
  distinct(round_number, submitter, score) %>% 
  summarise(score = sum(score), .by = c(submitter)) %>% 
  drop_na()

# Sum of rank (1-point = 1st)
# Winner: Captnmeaty
mean_rank <- 
all_votes %>% 
  distinct(round_number, submitter, score) %>% 
  mutate(rank = floor(rank(-score)), .by = round_number) %>% 
  summarise(mean_rank = mean(rank), .by = submitter)

# Sum of 1st place
# Winner: Captnmeaty
sum_1st <- 
all_votes %>% 
  distinct(round_number, submitter, score) %>% 
  mutate(rank = floor(rank(-score)), .by = round_number) %>% 
  summarise(sum_1st = sum(rank == 1), .by = submitter)
  
# Sum of Last place
# Winner: Christian
sum_last <- 
all_votes %>% 
  distinct(round_number, submitter, score) %>% 
  mutate(rank = floor(rank(-score)), .by = round_number) %>% 
  summarise(sum_last = sum(last(rank)), .by = submitter)

# Sum of voting text
# Winner: Captnmeaty
vote_text <- 
all_votes %>% 
  distinct(round_number, submitter, comment) %>% 
  mutate(comment_lenght = str_length(comment)) %>% 
  summarise(vote_text = mean(comment_lenght, na.rm = T), .by = submitter)

# Sum of submission text
sub_text <- 
all_votes %>% 
  distinct(round_number, submitter, submission_text) %>% 
  mutate(comment_lenght = str_length(submission_text)) %>% 
  summarise(sub_text = mean(comment_lenght, na.rm = T), .by = submitter)
  
# Sum of distinct voter
# Winner: Francois
mean_distinct <-
all_votes %>%
  mutate(n_voter = n(), .by = c(round_number, submitter)) %>%
  distinct(round_number, submitter, n_voter) %>% 
  summarise(mean_distinct = mean(n_voter), .by = c(submitter))

# Biggest point giver
# Winner: Guillaume
mean_giver <-
all_votes %>%
  # filter(!grepl("Left the league", voter)) %>% 
  summarise(avg_points = max(vote, na.rm = T), .by = c(round_number, voter)) %>% 
  summarise(avg_points = mean(avg_points), .by = voter) %>% 
  rename(submitter = 1) %>% 
  arrange(-avg_points)

# Correlation with consensus
# the contrarian
contrarian <-
  all_votes %>% 
  mutate(
    consensus = (sum(vote, na.rm = T) - vote) / (n() - 1),
    .by = c(round_number, submitter)
  ) %>% 
  summarise(
    corr = cor(vote, consensus, use = "complete.obs"),
    .by = voter
  ) %>% 
  rename(submitter = 1)

# coup de coeur pourri
# Antoine
polarizing <- 
all_votes %>%
  group_by(submitter) %>%
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    sd_score   = sd(score, na.rm = TRUE),
    polarizing_index = mean_score * sd_score
  ) %>%
  arrange(desc(polarizing_index))


# Prix c'est beau continue comme ça
improved <- 
all_votes %>%
  distinct(round_number, submitter, score) %>%
  arrange(submitter, round_number) %>% 
  mutate(improvement = score - lag(score), .by = submitter) %>% 
  summarise(improv_mean = mean(improvement, na.rm = T), .by = submitter)
