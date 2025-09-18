library(tidyverse)
library(lubridate)
library(rvest)
library(httr2)
library(polite)
library(glue)
library(fuzzyjoin)
library(scales)
library(gt)
library(gtExtras)

gg_bp_d_bow <- bow(
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Motion_Picture_%E2%80%93_Drama"
)
gg_bp_d_df <- gg_bp_d_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}'),
         Nominee = coalesce(`Director[4]`, Director)) %>% 
  select(Year, Film, Nominee) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
gg_bp_d_df %>% write_csv("blog_posts/oscars/clean-data/gg-bp-drama.csv")

# Gold Globes, Best Picture (Musical/Comedy)
gg_bp_c_bow <- bow(
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Motion_Picture_%E2%80%93_Musical_or_Comedy"
)
gg_bp_c_df <- gg_bp_c_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  .[4:9] %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Director) %>% 
  filter(Year >= 1970) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
gg_bp_c_df %>% write_csv("blog_posts/oscars/clean-data/gg-bp-comedy.csv")

# Golden Globes, Best Director
gg_bd_bow <- bow(
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Director"
)
gg_bd_df <- gg_bd_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Name) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
gg_bd_df %>% write_csv("blog_posts/oscars/clean-data/gg-bd.csv")

# Screenplay
gg_bsp_bow <- bow(
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Screenplay"
)
gg_bsp_df <- gg_bsp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Nominees) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
gg_bsp_df %>% write_csv("blog_posts/oscars/clean-data/gg-bsp.csv")

# Actress, comedy
gg_fa_c_bow <- bow(
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Actress_%E2%80%93_Motion_Picture_Comedy_or_Musical"
)
gg_fa_c_df <- gg_fa_c_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actress) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) 
gg_fa_c_df %>% write_csv("blog_posts/oscars/clean-data/gg-fa-c.csv")

# Actress, drama
gg_fa_d_bow <- bow(
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Actress_in_a_Motion_Picture_%E2%80%93_Drama"
)
gg_fa_d_df <- gg_fa_d_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actress) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
gg_fa_d_df %>% write_csv("blog_posts/oscars/clean-data/gg-fa-d.csv")

# Actor, comedy
gg_ma_c_bow <- bow(
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Actor_%E2%80%93_Motion_Picture_Musical_or_Comedy"
)
gg_ma_c_df <- gg_ma_c_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
gg_ma_c_df %>% write_csv("blog_posts/oscars/clean-data/gg-ma-c.csv")

# Actor, drama
gg_ma_d_bow <- bow(
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Actor_%E2%80%93_Motion_Picture_Drama"
)
gg_ma_d_df <- gg_ma_d_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
gg_ma_d_df %>% write_csv("blog_posts/oscars/clean-data/gg-ma-d.csv")

# Supporting Actor
gg_sma_bow <- bow(
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Supporting_Actor_%E2%80%93_Motion_Picture"
)
gg_sma_df <- gg_sma_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
gg_sma_df %>% write_csv("blog_posts/oscars/clean-data/gg-sma.csv")

# Supporting Actress
gg_sfa_bow <- bow(
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Supporting_Actress_%E2%80%93_Motion_Picture"
)
gg_sfa_df <- gg_sfa_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actress) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
gg_sfa_df %>% write_csv("blog_posts/oscars/clean-data/gg-sfa.csv")

# Screenplay
gg_bsp_bow <- bow(
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Screenplay"
)
gg_bsp_df <- gg_bsp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
gg_bsp_df %>% write_csv("blog_posts/oscars/clean-data/gg-bsp.csv")

cc_bp_bow <- bow(
  "https://en.wikipedia.org/wiki/Critics%27_Choice_Movie_Award_for_Best_Picture"
)
cc_bp_df <- cc_bp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}'),
         Nominee = coalesce(`Director(s)`, Director)) %>% 
  select(Year, Film, Nominee) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
cc_bp_df %>% write_csv("blog_posts/oscars/clean-data/cc-bp.csv")

# Critics Choice, Best Direction
cc_bd_bow <- bow(
  "https://en.wikipedia.org/wiki/Critics%27_Choice_Movie_Award_for_Best_Director"
)
cc_bd_df <- cc_bd_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}'),
         Film = coalesce(`Film (s)`)) %>% 
  select(Year, Film, Nominee=Director) %>% 
  group_by(Year) %>% 
  mutate(Winner = (row_number() == 1 | str_detect(Nominee, "TIE"))) %>% 
  separate_longer_delim(Film, " / ")
cc_bd_df %>% write_csv("blog_posts/oscars/clean-data/cc-bd.csv")

# Adapted Screenplay
cc_basp_bow <- bow(
  "https://en.wikipedia.org/wiki/Critics%27_Choice_Movie_Award_for_Best_Adapted_Screenplay"
)
cc_basp_df <- cc_basp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}'),
         Film = coalesce(`Film(s)`, Film),
         Nominee = coalesce(`Writer(s)`, Writer)) %>% 
  select(Year, Film, Nominee) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1 | str_detect(Nominee, "TIE"))
cc_basp_df %>% write_csv("blog_posts/oscars/clean-data/cc-basp.csv")

# Original Screenplay
cc_bosp_bow <- bow(
  "https://en.wikipedia.org/wiki/Critics%27_Choice_Movie_Award_for_Best_Original_Screenplay"
)
cc_bosp_df <- cc_bosp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}'),
         Nominee = `Writer (s)`) %>% 
  filter(!is.na(Film)) %>% 
  select(Year, Film, Nominee) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1 | str_detect(Nominee, "TIE"))
cc_bosp_df %>% write_csv("blog_posts/oscars/clean-data/cc-bosp.csv")

# Male actor
cc_ma_bow <- bow(
  "https://en.wikipedia.org/wiki/Critics%27_Choice_Movie_Award_for_Best_Actor"
)
cc_ma_df <- cc_ma_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  filter(!is.na(Film)) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1 | str_detect(Nominee, "TIE"))
cc_ma_df %>% write_csv("blog_posts/oscars/clean-data/cc-ma.csv")

# Female actor
cc_fa_bow <- bow(
  "https://en.wikipedia.org/wiki/Critics%27_Choice_Movie_Award_for_Best_Actress"
)
cc_fa_df <- cc_fa_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  filter(!is.na(Work)) %>% 
  select(Year, Film=Work, Nominee=Actress) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1 | str_detect(Nominee, "TIE"))
cc_fa_df %>% write_csv("blog_posts/oscars/clean-data/cc-fa.csv")

# Supporting actor
cc_sma_bow <- bow(
  "https://en.wikipedia.org/wiki/Critics%27_Choice_Movie_Award_for_Best_Supporting_Actor"
)
cc_sma_df <- cc_sma_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  filter(!is.na(Film)) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1 | str_detect(Nominee, "TIE"))
cc_sma_df %>% write_csv("blog_posts/oscars/clean-data/cc-sma.csv")

# Supporting actress
cc_sfa_bow <- bow(
  "https://en.wikipedia.org/wiki/Critics%27_Choice_Movie_Award_for_Best_Supporting_Actress"
)
cc_sfa_df <- cc_sfa_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  filter(!is.na(Film)) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1 | str_detect(Nominee, "TIE"))
cc_sfa_df %>% write_csv("blog_posts/oscars/clean-data/cc-sfa.csv")

dga_bd_bow <- bow(
  force = T,
  "https://en.wikipedia.org/wiki/Directors_Guild_of_America_Award_for_Outstanding_Directing_%E2%80%93_Feature_Film"
)
dga_bd_df <- dga_bd_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee = `Winners and nominees`) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  ungroup()
dga_bd_df %>% write_csv("blog_posts/oscars/clean-data/dga-bd.csv")

bafta_bp_bow <- bow(
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Film"
)
bafta_bp_df <- bafta_bp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=`Director(s)`) %>% 
  filter(!is.na(Year), !is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
bafta_bp_df %>% write_csv("blog_posts/oscars/clean-data/bafta-bp.csv")

# BAFTA, Best Director
bafta_bd_bow <- bow(
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Direction"
)
bafta_bd_df <- bafta_bd_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Director) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
bafta_bp_df %>% write_csv("blog_posts/oscars/clean-data/bafta-bp.csv")

# Original Screenplay
bafta_bosp_bow <- bow(
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Original_Screenplay"
)
bafta_bosp_df <- bafta_bosp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=`Screenwriter(s)`) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
bafta_bosp_df %>% write_csv("blog_posts/oscars/clean-data/bafta-bosp.csv")

# Adapted Screenplay
bafta_basp_bow <- bow(
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Adapted_Screenplay"
)
bafta_basp_df <- bafta_basp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=`Screenwriter(s)`) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
bafta_basp_df %>% write_csv("blog_posts/oscars/clean-data/bafta-basp.csv")

# Lead actor
bafta_ma_bow <- bow(
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Actor_in_a_Leading_Role"
)
bafta_ma_df <- bafta_ma_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  .[2:9] %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  filter(!is.na(Film), Year >= 1968, Nominee != "Best Actor") %>% # Before '68, award was split
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  group_by(Year, Nominee) %>% 
  mutate(Winner = any(Winner))
bafta_ma_df %>% write_csv("blog_posts/oscars/clean-data/bafta-ma.csv")

# Lead actress
bafta_fa_bow <- bow(
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Actress_in_a_Leading_Role"
)
bafta_fa_df <- bafta_fa_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  .[1:8] %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actress) %>% 
  filter(!is.na(Film), Year >= 1968) %>% # Before '68, award was split
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  group_by(Year, Nominee) %>% 
  mutate(Winner = any(Winner))
bafta_fa_df %>% write_csv("blog_posts/oscars/clean-data/bafta-fa.csv")

# Supporting actor
bafta_sma_bow <- bow(
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Actor_in_a_Supporting_Role"
)
bafta_sma_df <- bafta_sma_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  filter(!is.na(Film), Film != "Not Awarded") %>%
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  group_by(Year, Nominee) %>% 
  mutate(Winner = any(Winner))
bafta_sma_df %>% write_csv("blog_posts/oscars/clean-data/bafta-sma.csv")

# Supporting actress
bafta_sfa_bow <- bow(
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Actress_in_a_Supporting_Role"
)
bafta_sfa_df <- bafta_sfa_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  .[1:7] %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actress) %>% 
  filter(!is.na(Film)) %>% # Before '68, award was split
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  group_by(Year, Nominee) %>% 
  mutate(Winner = any(Winner))
bafta_sfa_df %>% write_csv("blog_posts/oscars/clean-data/bafta-sfa.csv")

sag_bp_bow <- bow(
  "https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Cast_in_a_Motion_Picture"
)
sag_bp_df <- sag_bp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=`Cast members`) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
sag_bp_df %>% write_csv("blog_posts/oscars/clean-data/sag-bp.csv")

# Leading actor
sag_ma_bow <- bow(
  "https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Male_Actor_in_a_Leading_Role"
)
sag_ma_df <- sag_ma_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  .[1:5] %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
sag_ma_df %>% write_csv("blog_posts/oscars/clean-data/sag-ma.csv")

# Leading actress
sag_fa_bow <- bow(
  "https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Female_Actor_in_a_Leading_Role"
)
sag_fa_df <- sag_fa_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  .[1:5] %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  filter(!is.na(Film)) %>% 
  select(Year, Film, Nominee=Actress) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
sag_fa_df %>% write_csv("blog_posts/oscars/clean-data/sag-fa.csv")

# Supporting actor
sag_sma_bow <- bow(
  "https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Male_Actor_in_a_Supporting_Role"
)
sag_sma_df <- sag_sma_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  .[1:4] %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
sag_sma_df %>% write_csv("blog_posts/oscars/clean-data/sag-sma.csv")

# Supporting actress
sag_sfa_bow <- bow(
  "https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Female_Actor_in_a_Supporting_Role"
)
sag_sfa_df <- sag_sfa_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  .[1:4] %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  filter(!is.na(Film)) %>% 
  select(Year, Film, Nominee=Actress) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1 | str_detect(Nominee, "TIE"))
sag_sfa_df %>% write_csv("blog_posts/oscars/clean-data/sag-sfa.csv")

pga_bp_bow <- bow(
  "https://en.wikipedia.org/wiki/Producers_Guild_of_America_Award_for_Best_Theatrical_Motion_Picture"
)
pga_bp_df <- pga_bp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=`Producer(s)`) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1)
pga_bp_df %>% write_csv("blog_posts/oscars/clean-data/pga-bp.csv")

oscars_bd_bow <- bow(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Director"
)
oscars_bd_df <- 
  oscars_bd_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=`Director(s)`) %>% 
  filter(!is.na(Nominee)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  ungroup()
oscars_bd_df %>% write_csv("blog_posts/oscars/clean-data/oscars-bd.csv")

# Oscars Best Picture
oscars_bp_bow <- bow(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture"
)
oscars_bp_df <- 
  oscars_bp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(`Year of Film Release`, '\\d{4}')) %>% 
  select(Year, Film, Nominee=`Producer(s)`) %>% 
  filter(!is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  ungroup()
oscars_bp_df %>% write_csv("blog_posts/oscars/clean-data/oscars-bp.csv")

# Oscars Original Screenplay
oscars_bosp_bow <- bow(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Original_Screenplay"
)
oscars_bosp_df <- 
  oscars_bosp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}'),
         Nominee = coalesce(Nominee, Nominees)) %>% 
  select(Year, Film, Nominee) %>% 
  filter(!is.na(Year), !is.na(Film)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  ungroup()
oscars_bosp_df %>% write_csv("blog_posts/oscars/clean-data/oscars-bosp.csv")

# Oscars Adapted Screenplay
oscars_basp_bow <- bow(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Adapted_Screenplay"
)
oscars_basp_df <- 
  oscars_basp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Nominees) %>% 
  filter(!is.na(Year)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  ungroup()
oscars_basp_df %>% write_csv("blog_posts/oscars/clean-data/oscars-basp.csv")

# Lead actor
oscars_ma_bow <- bow(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actor"
)
oscars_ma_df <- 
  oscars_ma_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  .[2:12] %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  filter(!is.na(Year)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  ungroup()
oscars_ma_df %>% write_csv("blog_posts/oscars/clean-data/oscars-ma.csv")

# Lead actress
oscars_fa_bow <- bow(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actress"
)
oscars_fa_df <- 
  oscars_fa_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  filter(!is.na(Year)) %>% 
  select(Year, Film, Nominee=Actress) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  ungroup()
oscars_fa_df %>% write_csv("blog_posts/oscars/clean-data/oscars-fa.csv")

# Supporting actor
oscars_sma_bow <- bow(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Supporting_Actor"
)
oscars_sma_df <- 
  oscars_sma_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  select(Year, Film, Nominee=Actor) %>% 
  filter(!is.na(Year)) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  ungroup()
oscars_sma_df %>% write_csv("blog_posts/oscars/clean-data/oscars-sma.csv")

# Supporting actress
oscars_sfa_bow <- bow(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Supporting_Actress"
)
oscars_sfa_df <- 
  oscars_sfa_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  mutate(Year = str_extract(Year, '\\d{4}')) %>% 
  filter(!is.na(Year)) %>% 
  select(Year, Film, Nominee=Actress) %>% 
  group_by(Year) %>% 
  mutate(Winner = row_number() == 1) %>% 
  ungroup()
oscars_sfa_df %>% write_csv("blog_posts/oscars/clean-data/oscars-sfa.csv")

wga_basp_bow <- bow(
  "https://en.wikipedia.org/wiki/Writers_Guild_of_America_Award_for_Best_Adapted_Screenplay"
)
wga_basp_df <- 
  wga_basp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  select(Year, Film, Nominee=`Recipient(s)`) %>% 
  mutate(Year = str_extract(Year, '\\d{4}'),
         cat = str_extract(Film, 'Best (Drama|Comedy)')) %>% 
  fill(cat) %>% 
  filter(!is.na(Film), !str_detect(Film, 'Best (Drama|Comedy)')) %>% 
  group_by(Year, cat) %>% 
  mutate(Winner = row_number() == 1) %>% 
  ungroup() %>% 
  select(-cat)
wga_basp_df %>% write_csv("blog_posts/oscars/clean-data/wga-basp.csv")

# WGA Original
wga_bosp_bow <- bow(
  "https://en.wikipedia.org/wiki/Writers_Guild_of_America_Award_for_Best_Original_Screenplay"
)
wga_bosp_df <- 
  wga_bosp_bow %>% 
  scrape() %>% 
  html_nodes("table.wikitable") %>% 
  html_table() %>% 
  map_df(~ .x %>% mutate(across(everything(), as.character))) %>% 
  select(Year, Film, Nominee=`Writer(s)`) %>% 
  mutate(Year = str_extract(Year, '\\d{4}'),
         cat = str_extract(Film, 'Best (Drama|Comedy)')) %>% 
  fill(cat) %>% 
  filter(!is.na(Film), !str_detect(Film, 'Best (Drama|Comedy)')) %>% 
  group_by(Year, cat) %>% 
  mutate(Winner = row_number() == 1) %>% 
  ungroup() %>% 
  select(-cat)
wga_bosp_df %>% write_csv("blog_posts/oscars/clean-data/wga-bosp.csv")


df <- list.files("./blog_posts/oscars/clean-data/") %>% 
  keep(.p = !str_detect(., "all")) %>% 
  map_dfr(~ read_csv(paste0("./blog_posts/oscars/clean-data/",.x)) 
          %>% mutate(id = .x)) %>% 
  mutate(Year = as.numeric(Year),
         Film = case_when(
           Film == "Everything Everywhere All At Once" ~ "Everything Everywhere All at Once",
           str_detect(Film, regex("(tick).*(boom)", ignore_case = T)) ~ "tick, tick... BOOM!",
           Film == "A Star is Born" ~ "A Star Is Born",
           Film == "Life is Beautiful" ~ "Life is Beautiful",
           str_detect(Film, "^Birdman") ~ "Birdman",
           str_detect(Film, "^Borat Subsequent Moviefilm") ~ "Borat Subsequent Moviefilm",
           Film == "Precious: Based on the Novel 'Push' by Sapphire" ~ "Precious",
           str_detect(Film, "Good Night.? and Good Luck.?") ~ "Good Night, and Good Luck",
           str_detect(Film, "The Postman") ~ "The Postman",
           Film == "Thelma and Louise" ~ "Thelma & Louise",
           Film == "Secrets and Lies" ~ "Secrets & Lies",
           Film == "Waking Ned Devine" ~ "Waking Ned",
           Film == "Sex, Lies and Videotape" ~ "Sex, Lies, and Videotape",
           Film == "Those Magnificent Men in their Flying Machines" ~ "Those Magnificent Men in Their Flying Machines",
           Film == "MASH" ~ "M*A*S*H",
           Film == "Harry Potter and the Philosopher's Stone" ~ "Harry Potter and the Sorcerer's Stone",
           str_detect(Film, "(Ford).*(Ferrari)") ~ "Ford v Ferrari",
           Film == "East Is East" ~ "East is East",
           Film == "Hiroshima, Mon Amour" ~ "Hiroshima mon amour",
           Film == "Lacombe Lucien" ~ "Lacombe, Lucien",
           Film == "Murder on the Orient Express / Serpico" ~ "Murder on the Orient Express",
           Film == "A Man For All Seasons" ~ "A Man for All Seasons",
           Film == "Victor Victoria" ~ "Victor/Victoria",
           Film == "...And Justice for All." ~ "...And Justice for All",
           str_detect(Film, "^Adaptation") & Year == 2002 ~ "Adaptation.",
           str_detect(Film, "^Dr. Strangelove") ~ "Dr. Strangelove",
           TRUE ~ Film
         ) %>% 
           str_remove("\\(.*\\)$") %>% 
           str_remove("\\[.*\\]$") %>% 
           str_replace_all("(’)|(‘)", "'") %>% 
           str_trim()) %>% 
  filter(!is.na(Year))

df %>% write_csv("./blog_posts/oscars/clean-data/all-awards.csv")


# Predictions -------------------------------------------------------------

df <- read_csv("./blog_posts/oscars/clean-data/all-awards.csv")
predict_category <- function(
    category = c("Picture", 
                 "Director", 
                 "Original Screenplay",
                 "Adapted Screenplay",
                 "Actor",
                 "Actress"),
    formula = ". - cc",
    ...) {
  
  category <- match.arg(category)
  str_filter <- case_when(
    category == "Picture" ~ "bp",
    category == "Director" ~ "bd",
    category == "Original Screenplay" ~ "(bosp)|(gg-bsp)",
    category == "Adapted Screenplay" ~ "(basp)|(gg-bsp)",
    category == "Actor" ~ "\\bma",
    category == "Actress" ~ "\\bfa"
  )
  noms <- df %>% 
    filter(str_detect(id, str_filter)) %>% 
    mutate(Nominee = str_remove_all(Nominee, "[†‡\\[].*") %>% str_trim, 
           id = str_extract(id, "(\\w+)-*", group =1)) %>% 
    pivot_wider(id_cols = c(Year, Film),
                names_from = id, 
                values_from = Winner, 
                values_fn = ~ max(if_else(.x, 'Won', 'Lost')),
                values_fill = 'Not Nominated'
    ) %>% 
    mutate(across(!c(Year, Film), 
                  ~ factor(.x, levels = c("Not Nominated", "Lost", "Won")))) %>% 
    filter(oscars %in% c("Won", "Lost"))
  
  train <-
    noms %>% 
    filter(Year < 2024, ...)
  
  test <-
    noms %>% 
    filter(Year == 2024)
  
  mod <- glm(as.formula(glue("oscars ~ {formula}")), 
             family = binomial(), 
             data = train %>% select(-Year, -Film))
  print(summary(mod))
  
  preds <-
    test %>% 
    modelr::add_predictions(mod, type = "response") %>% 
    mutate(award = category) %>% 
    select(-oscars, award, pred) %>% 
    arrange(desc(pred))
  
  print(preds)
  
}

# Default formula excludes CC
predict_category("Picture") 
predict_category("Picture", ".") # This will include CC
predict_category("Picture", ".", Year >= 1995) # This will include CC and limit to 95-present

predict_category("Director")

predict_category("Original Screenplay")

predict_category("Adapted Screenplay")

predict_category("Actor", ".", Year >= 1995)

predict_category("Actress", ".", Year >= 1995)
