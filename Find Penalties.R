library(tidyverse)
library(nflfastR)
library(gt)
library(gtExtras)

# Load NFL pbp for this season

pbp <- load_pbp()

penalties <- pbp %>%
  filter(str_detect(tolower(desc), "penalty"))

# extract player, type of penalty, yards, and accepted/declined
penalties_info <- penalties %>%
  filter(!(str_detect(desc, "Penalty") & str_detect(desc, "PENALTY"))) %>% 
  mutate(penalty_team = str_extract(desc, "(?i)(?<=penalty on\\s)\\w+"),
         penalty_yards = as.integer(str_extract(desc, "(?<=, )[0-9]+(?= yards)")),
         penalty_accepted = !str_detect(tolower(desc), "declined"),
         penalty_yards = ifelse(posteam == penalty_team, -penalty_yards, penalty_yards)) %>% 
  select(desc, down, ydstogo, yardline_100, penalty_yards, penalty_accepted, penalty_type)

write_csv(penalties_info, "2022 Penalties.csv")
