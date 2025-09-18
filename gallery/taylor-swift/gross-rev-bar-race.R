library(tidyverse)
library(ggpattern)
library(gganimate)
library(rvest)
library(glue)
library(scales)
library(shadowtext)
library(showtext)

sysfonts::font_add_google("Fira Sans", "fira-sans")
showtext_auto()

df <- readr::read_csv("https://raw.githubusercontent.com/posit-marketing/taylor-swift-tour/main/data/rank_concert_tours.csv") %>% 
  mutate(
    artist = case_when(
      artist == "Tim McGraw/Faith Hill"  ~ "Tim Mcgraw et. al.",
      artist == "Tim McGraw / Faith Hill"  ~ "Tim Mcgraw et. al.",
      artist == "Kenny Chesney & Tim Mcgraw"  ~ "Tim Mcgraw et. al.",
      artist == "Michael Jackson The Immortal World Tour By Cirque Du Soleil"  ~ "Michael Jackson",
      artist == "Bruce Springsteen & The E Street Band"  ~ "Bruce Springsteen & the E Street Band",
      artist == "Jay-Z / Beyoncé"   ~ "Beyoncé & Jay Z",
      artist == "Beyoncé and Jay Z" ~ "Beyoncé & Jay Z",
      artist == "Billy Joel/Elton John"  ~ "Billy Joel & Elton John"  ,
      artist == "“Summer Sanitarium Tour”/Metallica"  ~ "Metallica",
      artist == "'N Sync"  ~ "Nsync",
      .default = artist
    ),
    img = case_when(
      artist == "Taylor Swift" ~ "https://pbs.twimg.com/profile_images/1733149153490812928/5o_Nl1_Y_400x400.png",
      artist == "Bruce Springsteen & the E Street Band" ~ "https://pbs.twimg.com/profile_images/1575485570444185601/akmHjbf2_400x400.jpg",
      artist == "The Rolling Stones" ~ "https://seeklogo.com/images/R/Rolling_Stones-logo-B455DE3C86-seeklogo.com.png",
      artist == "U2" ~ "https://pbs.twimg.com/media/F2934i1XkAAxuHU.png",
      artist == "Elton John" ~ "https://ongpng.com/wp-content/uploads/2023/08/Elton-John-pointing.png"
    )
  )

df %>% 
  group_by(year) %>% 
  ggplot(aes(xmin = 0,
             xmax = gross,
             ymin = rank - 0.45, 
             ymax = rank + 0.45, 
             y = rank,
             group = artist)) + 
  geom_text(
    aes(label = stringr::str_wrap(artist, 25)),
    size = 3,
    x = -500) + 
  geom_rect(aes(fill = artist)) + 
  facet_wrap(~year) + 
  scale_y_reverse(labels = NULL, breaks = NULL) +
  scale_x_continuous(limits = c(-1000, 2500),
                     breaks = c(0, 500, 1000, 1500, 2000, 2500)) +
  labs(y = NULL) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_null() +
  geom_text(x = 1500,
            y = -9,
            size = 30,
            family = "Times",
            col = "grey30",
            aes(label = as.character(year))) +
  transition_time(year) -> g

animate(g, renderer = gifski_renderer(), nframes = 200)


df %>% 
  ggplot(aes(x = year, y = rank, group = artist)) + 
  geom_path(aes(color = artist), linewidth = 2, alpha = 0.3) +  
  scale_y_reverse(
    breaks = 1:10
  ) + 
  scale_color_manual(
    values = c("Taylor Swift" = "blue", 
               "Bruce Springsteen & the E Street Band" = "black",
               "The Rolling Stones" = "red",
               "U2" = "purple",
               "Elton John" = "green",
               "grey")) + 
  scale_alpha_manual(
    values = c("Taylor Swift" = 1, 
               "Bruce Springsteen & the E Street Band" = 1,
               "The Rolling Stones" = 1,
               "U2" = 1)) + 
  theme_minimal(base_family = "fira-sans") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  ggimage::geom_image(aes(image = img))




# Bar chart race
url <- "https://en.wikipedia.org/wiki/List_of_highest-grossing_concert_tours"
bow <- polite::bow(url)

tbls <- polite::scrape(bow) %>% 
  html_nodes("table.wikitable") %>% 
  html_table(fill = TRUE) 

t <- tibble()
for (YEAR in tbls[[9]]$Year) {
  year_tbl <- tbls[[9]] %>% 
    filter(Year <= YEAR) %>% 
    mutate(adj_gross = parse_number(`Adjusted gross (in 2022 dollar)`))
  top <- year_tbl %>%
    slice_max(adj_gross, n = 5) %>% 
    mutate(rank = row_number(), t = YEAR)
  t <- t %>% bind_rows(top)
}

tour_df <- t %>% 
  mutate(
    img = case_when(
      Artist == 'Taylor Swift' ~ '~/Desktop/tswift.jpg',
      Artist == 'Ed Sheeran' ~ '~/Desktop/ed-sheeran.jpeg',
      Artist == 'U2' ~ '~/Desktop/u2.jpeg',
      Artist == 'The Rolling Stones' ~ '~/Desktop/rolling-stones.jpeg',
      Artist == 'Grateful Dead' ~ '~/Desktop/grateful-dead.png',
      Artist == 'Madonna' ~ '~/Desktop/madonna.jpeg',
      Artist == 'Tina Turner' ~ '~/Desktop/tina-turner.png',
      Artist == 'New Kids on the Block' ~ '~/Desktop/nkotb.jpeg',
      Artist == 'Eagles' ~ '~/Desktop/eagles.jpeg',
      Artist == 'The Police' ~ '~/Desktop/the-police.jpg'
    )
  )

tour_df %>% 
  filter(t >= 1993) %>% 
  ggplot(aes(x = rank, y = adj_gross)) +
  geom_col_pattern(
    aes(pattern_filename = I(img)),
    pattern = "image",
    pattern_type = "expand",
    pattern_alpha = 0.5
  ) +
  coord_flip() +
  geom_shadowtext(
    aes(y = adj_gross*0.99,
        label = glue("{Artist}\n{`Tour title`}\n{Year}")),
    size = 10,
    color = "white",
    fontface = "bold",
    bg.color = "black",
    nudge_y = -1e7,
    hjust = 1) + 
  geom_text(
    aes(y = adj_gross*1.01,
        label = dollar(adj_gross, scale_cut = cut_short_scale())),
    size = 10,
    color = "grey20",
    fontface = "bold", 
    hjust = 0) + 
  facet_wrap(~t) + 
  scale_x_reverse(
    breaks = NULL,
    labels = NULL
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0.1, 0.1)),
    labels = NULL,
    breaks = NULL) +
  labs(title = "Highest-Grossing Tours by {closest_state}", 
       subtitle = "Single year revenue in 2022 dollar",
       x = NULL, y = NULL) +
  theme_minimal(base_family = "fira-sans") +
  theme(legend.position = "none",
        plot.title = element_text(size = 32, 
                                  color = "black",
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(
          size = 24,
          hjust = 0.5, 
          margin = margin(t = 20)),
        plot.margin = margin(t = 60,
                             b = 40,
                             r = 40)
        ) +
  facet_null() +
  transition_states(t, 
                    transition_length = 3, 
                    state_length = 1, 
                    wrap = FALSE) +
  enter_recolor() +
  # enter_drift(x_mod = 1) +
  enter_grow() +
  exit_recolor() +
  exit_fade() +
  view_follow() -> g

animate(g, 
        width = 1800, 
        height = 1200,
        renderer = av_renderer("~/Desktop/top-5-annual-gross-revenue-1993-2023-1800x1200.mp4"), 
        fps = 30,
        duration = 30,
        start_pause = 30,
        end_pause = 60)

anim_save("top-5-annual-gross-revenue-1993-2023-1800x1200.mp4", 
          animation = av,  
          path = "~/Desktop")

# gif <- animate(g, 
#               width = 1800, 
#               height = 1200,
#               renderer = gifski_renderer(), 
#               fps = 30,
#               duration = 30,
#               start_pause = 30,
#               end_pause = 60)
# 
# anim_save("top-5-annual-gross-revenue-1993-2023-1800x1200.gif", 
#           animation = gif,  
#           path = "~/Desktop")
