library(tidyverse)
library(nflfastR)
library(gt)
library(gtExtras)
library(plotly)
library(hrbrthemes)

# Load NFL pbp data from 2010 to present

pbp <- load_pbp(2010:2022)

scores <- pbp %>% 
  group_by(game_id) %>% 
  summarise(home_score = last(home_score),
            away_score = last(away_score),
            winner = case_when(home_score > away_score ~ last(home_team),
                               away_score > home_score ~ last(away_team),
                               TRUE ~ "Tie")) %>% 
  filter(winner != "Tie")

pbp_upd <- merge(pbp,scores %>% select(game_id,winner)) %>% 
  mutate(margin = home_score - away_score,
         home_has_ball = posteam == home_team,
         home_win = winner == home_team,
         score_differential = total_home_score - total_away_score)

model <- glm(home_win ~ score_differential + home_has_ball + game_seconds_remaining + down + ydstogo + yardline_100, data = pbp_upd, family= binomial(link = "logit"))

summary(model)

# Situation: home team up 7, away team has ball, 30 seconds left, 1st and goal from 10 yd line

predict(model, data.frame(score_differential = 7, home_has_ball = F, game_seconds_remaining = 30, 
                          down = 1, ydstogo = 10, yardline_100 = 10),
        type = "response")

# Home team has a 74% chance of winning

# Away team scores touchdown and ties game in 13 seconds, kickoff is a touchback

predict(model, data.frame(score_differential = 0, home_has_ball = T, game_seconds_remaining = 17, 
                          down = 1, ydstogo = 10, yardline_100 = 75),
        type = "response")

# Home team now has a 57.6% chance of winning

nfc_championship <- pbp_upd %>% 
  filter(game_id == "2022_21_SF_PHI") %>% 
  drop_na(score_differential, home_has_ball, game_seconds_remaining, down, ydstogo, yardline_100) %>% 
  mutate(play_number = row_number())

nfc_championship$win_prob <- predict(model, 
                                     data.frame(score_differential = nfc_championship$score_differential,
                                                home_has_ball = nfc_championship$home_has_ball,
                                                game_seconds_remaining = nfc_championship$game_seconds_remaining,
                                                down = nfc_championship$down,
                                                ydstogo = nfc_championship$ydstogo,
                                                yardline_100 = nfc_championship$yardline_100),
                                     type = "response")


qtr_ends <- nfc_championship %>% 
  filter(qtr != max(qtr)) %>% 
  group_by(qtr) %>% 
  summarise(play = last(play_number)) %>% 
  pull(play)

nfc_championship_plot <- ggplot(nfc_championship, aes(x = play_number, 
                                      y = win_prob,
                                      group = 1,
                                      text = paste0('Quarter: ', qtr,
                                                    '\nTime Left: ', time,
                                                    '\nHome Score: ', total_home_score,
                                                    '\nAway Score: ', total_away_score,
                                                    '\nHome Win Likelihood: ', paste0(round(win_prob*100,2),"%")))) +
  geom_line() +
  geom_vline(xintercept = qtr_ends, linetype = "dashed") +
  ylim(0,1) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = paste0("Win Probability Chart for 49ers @ Eagles (NFC Championship 2022)"),
       x = "Play #",
       y = "Home Win Probability")


nfc_championship_plotly <- ggplotly(nfc_championship_plot, tooltip = "text")

htmlwidgets::saveWidget(nfc_championship_plotly, "NFC Championship 2022 Win Prob.html")


comeback <- pbp_upd %>% 
  filter(game_id == "2022_15_IND_MIN") %>% 
  drop_na(score_differential, home_has_ball, game_seconds_remaining, down, ydstogo, yardline_100) %>% 
  mutate(play_number = row_number())

comeback$win_prob <- predict(model, 
                                     data.frame(score_differential = comeback$score_differential,
                                                home_has_ball = comeback$home_has_ball,
                                                game_seconds_remaining = comeback$game_seconds_remaining,
                                                down = comeback$down,
                                                ydstogo = comeback$ydstogo,
                                                yardline_100 = comeback$yardline_100),
                                     type = "response")

qtr_ends <- comeback %>% 
  filter(qtr != max(qtr)) %>% 
  group_by(qtr) %>% 
  summarise(play = last(play_number)) %>% 
  pull(play)

comeback_plot <- ggplot(comeback, aes(x = play_number, 
                                      y = win_prob,
                                      group = 1,
                                      text = paste0('Quarter: ', qtr,
                                                   '\nTime Left: ', time,
                                                   '\nHome Score: ', total_home_score,
                                                   '\nAway Score: ', total_away_score,
                                                   '\nHome Win Likelihood: ', paste0(round(win_prob*100,2),"%")))) +
  geom_line() +
  geom_vline(xintercept = qtr_ends, linetype = "dashed") +
  ylim(0,1) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = paste0("Win Probability Chart for Colts @ Vikings (Week 15, 2022)"),
       x = "Play #",
       y = "Home Win Probability")
  

comeback_plot <- ggplot(comeback %>% mutate(wp = wp * (posteam == home_team)), aes(x = play_number, 
                                      y = wp,
                                      group = 1,
                                      text = paste0('Quarter: ', qtr,
                                                    '\nTime Left: ', time,
                                                    '\nHome Score: ', total_home_score,
                                                    '\nAway Score: ', total_away_score,
                                                    '\nHome Win Likelihood: ', paste0(round(win_prob*100,2),"%")))) +
  geom_line() +
  geom_vline(xintercept = qtr_ends, linetype = "dashed") +
  ylim(0,1) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = paste0("Win Probability Chart for Colts @ Vikings (Week 15, 2022)"),
       x = "Play #",
       y = "Home Win Probability")

comeback_plotly <- ggplotly(comeback_plot, tooltip = "text")

htmlwidgets::saveWidget(comeback_plotly, "'The Comeback' Win Prob.html")
