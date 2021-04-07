library(tidyverse)
library(googlesheets4)
library(latex2exp)
library(ggimage)
library(here)

set.seed(20210406)

# public sheet, don't prompt the user to log in
gs4_deauth()

d20s <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yUeR0Y8s5bcdX12vhVQynnktxIaJ3zTQLHWgUbvT9cI/edit?usp=sharing") %>%
  mutate(trial = row_number(),
         sim = sample(1:20, nrow(.), replace = TRUE)) %>%
  pivot_longer(cols = -trial, names_to = "die", values_to = "roll")

final_chisq <- d20s %>%
  count(die, roll) %>%
  group_by(die) %>%
  summarize(chisq = chisq.test(n)$statistic,
            pval = chisq.test(n)$p.value)

# test2 <- d20s %>%
#   filter(trial <= 150) %>%
#   count(die, roll) %>%
#   group_by(die) %>%
#   mutate(exp = nrow(d20s) / length(unique(d20s$die)) / 20,
#          case_stat = (n - exp)^2 / exp) %>%
#   group_by(die) %>%
#   summarize(chisq = sum(case_stat), .groups = "drop") %>%
#   mutate(pval = pchisq(chisq, df = 20 - 1, lower.tail = FALSE))
# 
# blue_gold <- filter(d20s, die == "blue_gold", trial <= 150) %>% 
#   count(roll) %>% 
#   chisq.test(x = .$n, y = NULL)

out <- lapply(seq(max(d20s$trial)), function(x) {
  # we don't want less than 100 bc the expected cell count is less than 5
  # if (x < 100) return(NULL)
  
  sliced <- filter(d20s, trial <= x)
  
  sliced %>%
    count(die, roll) %>%
    mutate(exp = nrow(sliced) / 6 / 20,
           case_stat = (n - exp)^2 / exp) %>%
    group_by(die) %>%
    summarize(chisq = sum(case_stat), .groups = "drop") %>%
    mutate(pval = pchisq(chisq, df = 20 - 1, lower.tail = FALSE),
           trial = x)
}) %>%
  bind_rows() %>%
  arrange(die, trial)

cols <- c("red" = "red4",
          "blue_green" = "seagreen3",
          "blue_gold" = "steelblue4",
          "blue_purple" = "orchid4",
          "blue_yellow" = "orange3",
          "sim" = "black")

d20img <- "https://openclipart.org/download/94501/twenty-sided-dice.svg"

pval_plot <- out %>%
  filter(trial %% 5 == 0) %>%
  ggplot(aes(x = trial, y = pval, col = die, group = die)) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  geom_line() +
  scale_color_manual(values = cols) +
  labs(x = "Trial",
       y = 'p-value',
       title = "Testing My Five D20s - Plus One Simulated Die",
       subtitle = TeX(r'(Evolution of cumulative p-values from successive $\;\Chi^2\;$ tests)')) +
  theme_bw() +
  theme(legend.position = "none")

ggbackground(pval_plot, d20img, alpha = .1, color = "gray3")

ggsave(here('img', '06-experimental.svg'), 
       width = 6, height = 6)
ggsave(here('img', '06-experimental.png'), 
       width = 6, height = 6)

d20s %>%
  group_by(die, roll) %>%
  summarize(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  filter(die == "red") %>%
  ggplot(aes(x = roll, y = prop, fill = die)) +
  geom_col() +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = cols) +
  facet_wrap(~die) +
  theme_bw() +
  theme(legend.position = "none")
