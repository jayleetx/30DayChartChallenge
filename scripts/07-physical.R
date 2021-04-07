library(tidyverse)
library(googlesheets4)
library(ggimage)
library(here)

gs4_deauth()

# read data from drive
book_heights <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/16i6u0SgjEuy3kdu4HxY89a8JEJKvptkcLqg7GdH4OhM/edit?usp=sharing") %>%
  mutate(height = round(height_in + height_eighths / 8, 0),
         type = factor(type, levels = c('fiction', 'nonfiction', 'cookbook', 'other'),
                       labels = c("Fiction", "Nonfiction", "Cookbook", "Other")))

# machinations to set the y-value for each book to stack correctly
heights_tab <- book_heights %>%
  arrange(height_in, type) %>%
  group_by(height_in) %>%
  mutate(stack = row_number()) %>%
  ungroup() %>%
  # I edited these SVGs manually to set the colors, but the source is
  # https://openclipart.org/download/211627/book_lying_generic.svg
  mutate(image = case_when(
    type == "Fiction" ~ "data/book_purple.svg",
    type == "Nonfiction" ~ "data/book_orange.svg",
    type == "Cookbook" ~ "data/book_green.svg",
    type == "Other" ~ "data/book_pink.svg"
  ))

cols <- c("Fiction" = "#7570b3",
          "Nonfiction" = "#d95f02",
          "Cookbook" = "#1b9e77",
          "Other" = "#e7298a")

# this looks like a histogram/bar plot but the frame is actually a dot plot
ggplot(heights_tab, aes(x = height_in, y = stack, image = image)) +
  geom_image(size = .1) +
  geom_point(aes(x = height_in + 100, y = stack + 100, col = fct_rev(type)), alpha = 1, size = 3, shape = 15) +
  scale_x_continuous(breaks = 5:11, limits = c(4.75, 11.25)) +
  scale_y_continuous(breaks = seq(0, 24, 4), limits = c(1,21)) +
  scale_colour_manual(values = cols) +
  labs(x = "Height, Rounded to the Nearest Inch",
       y = "Count",
       title = "Heights of the Books on My Bookshelf",
       subtitle = "(I organize my bookshelf by height)",
       col = "") +
  theme_bw()

# save images
ggsave(here('img', '07-physical.svg'), 
       width = 6, height = 6)
ggsave(here('img', '07-physical.png'), 
       width = 6, height = 6)
