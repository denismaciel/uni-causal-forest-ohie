library(tidyverse)

theme_set(theme_light())

p_c <- seq(0, 1, length.out = 100)
p_d <- 1 - p_c

entropy <- - p_c * log(p_c) - p_d * log(p_d)

df <- tibble(entropy, p_c) %>% 
  mutate(entropy = ifelse(is.nan(entropy), 0, entropy)) %>% 
  mutate(single_node_entropy = -p_c * log(p_c),
         single_node_entropy = ifelse(is.nan(single_node_entropy), 0, single_node_entropy))

p <- df %>% 
  ggplot(aes(p_c, entropy)) +
  geom_line() + 
  labs(
    y = "Node Entropy",
    x = "Share of Creditworthy Applicants"
  ) + 
  expand_limits(y = 0:1)

p2 <- df %>% 
  ggplot(aes(x = p_c, single_node_entropy)) +
  geom_line() + 
  labs(
    y = "Partial Entropy",
    x = "Share of Creditworthy Applicants"
   ) +
  expand_limits(y = 0:1)

output_plot <- p + p2

ggsave(output_plot, filename = here::here("figs/entropy.png"))
