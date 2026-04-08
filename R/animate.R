library(gganimate)
library(plotly)

## gganimate ----
# p <-
all_votes %>% 
  distinct(round_number, submitter, score) %>% 
  arrange(submitter, round_number) %>% 
  mutate(cum = cumsum(score), .by = submitter) %>% 
  
  ggplot(aes(round_number, cum, color = submitter)) +
  geom_line() +
  geom_point() +
  theme_vapor()

  transition_reveal(round_number) +
  shadow_mark(alpha = 0.2, size = 0.5)
  view_follow(fixed_x = TRUE)

animate(p, fps = 10, width = 800, height = 500)

## plotly ----
base_df <- all_votes %>% 
  distinct(round_number, submitter, score) %>% 
  arrange(submitter, round_number) %>% 
  mutate(cum = cumsum(score), .by = submitter)

df_anim <- map_dfr(unique(base_df$round_number), function(r) {
  base_df %>%
    filter(round_number <= r) %>%
    mutate(frame = r)
})


plot_ly(
  df_anim,
  x = ~round_number,
  y = ~cum,
  color = ~submitter,
  split = ~submitter,   # <-- VERY important for lines
  frame = ~frame,
  type = "scatter",
  mode = "lines+markers"
) %>%
  animation_opts(frame = 800, easing = "linear")
## rank movement ----
p <- 
all_votes %>% 
  distinct(round_number, submitter, score) %>% 
  arrange(submitter, round_number) %>% 
  mutate(cum = cumsum(score), .by = submitter) %>% 
  mutate(rank_pos = dense_rank(-cum), .by = round_number) %>%
  
  ggplot(aes(round_number, -rank_pos, color = submitter)) +
  annotate("rect", xmin = 1, xmax = 23, ymin = -.5, ymax = -1.5, fill ="#D4AF37", alpha = .8) +
  annotate("rect", xmin = 1, xmax = 23, ymin = -1.5, ymax = -2.5, fill ="#C0C0C0", alpha = .8) +
  annotate("rect", xmin = 1, xmax = 23, ymin = -2.5, ymax = -3.5, fill ="#B08D57", alpha = .8) +
  geom_line() +
  scale_y_continuous(
    label = ~numform::f_ordinal(.x),
    n.breaks = 11
  ) +
  theme_vapor()
  
ggplotly(p)