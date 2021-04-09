# devtools::install_github("EnriquePH/OEIS.R")
library(rvest)
library(tidyverse)
library(ggrepel)
library(patchwork)
library(here)

# scrape page to get list of core sequences

core_seqs <- read_html("https://oeis.org/wiki/Index_to_OEIS:_Section_Cor") %>%
  html_node(xpath = '/html/body/div[1]/div[1]/div/div[3]/div[4]/div') %>%
  html_text() %>%
  str_extract_all("A\\d{6}") %>%
  unlist()

oeis_sites <- data.frame(title = core_seqs) %>%
  mutate(page = str_replace(title, "^A", "b"),
         site = paste0("https://oeis.org/", title, "/", page, ".txt"))

# scrape all the sequence pages and count how many times each integer appears

integers <- lapply(oeis_sites$site, read_lines)

integers_count <- integers %>%
  unlist() %>%
  trimws() %>%
  str_replace_all("\\s+", " ") %>%
  data.frame() %>%
  separate(col = ".", into = c('index', 'integer', 'extra'), 
           sep = " ", extra = "merge", fil = "right") %>%
  select(-extra) %>%
  filter(!str_detect(index, "^#")) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(is.finite(integer), integer <= 100) %>%
  count(integer)

# for a legible graphic, only keep integers <= 100 and more than 10 occurrences
# on the engative side, there's a big drop after -1 to only 5 or 8 occurrences
integers_count %>%
  filter (n > 10) %>%
  ggplot(aes(x = integer, y = n)) +
  geom_point() +
  geom_segment(aes(yend = 1, xend = integer))


plot_10minus <- integers_count %>%
  filter (n > 10, integer <= 10) %>%
  ggplot(aes(x = integer, y = n)) +
  geom_point() +
  geom_segment(aes(yend = 1, xend = integer)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = -1:10, minor_breaks = NULL) +
  labs(y = "Frequency", x = "Integer (-1 to 10)") +
  theme_bw()

plot_10plus <- integers_count %>%
  filter (n > 10, integer >= 10) %>%
  mutate(label_col = ifelse(n >= 3328 & !(integer %in% c(41,47,61,73,10,63,71,59,43,37,31,29,23,20,17,13,11)), 
                            as.character(integer), NA)) %>%
  ggplot(aes(x = integer, y = n)) +
  geom_point() +
  geom_segment(aes(yend = 1, xend = integer)) +
  geom_text_repel(aes(label = label_col), nudge_y = 500, segment.size = 0.5) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 14000)) +
  scale_x_continuous(breaks = 10*(1:10)) +
  labs(y = "", x = "Integer (10 to 100)") +
  theme_bw() +
  theme(axis.title.y = element_blank())

plot_10minus + plot_10plus +
  plot_layout(widths = c(1,2)) +
  plot_annotation(title = "Frequency of Integers in OEIS Core Sequences",
                  caption = "Data from the Online Encyclopedia of Integer Sequences")

# save images
ggsave(here('img', '09-statistics.svg'), 
       width = 8, height = 4)
ggsave(here('img', '09-statistics.png'), 
       width = 8, height = 4)
