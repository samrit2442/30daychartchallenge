library(tidyverse)
library(showtext)
library(ggtext)
library(magick)
library(plotly)

raw_data <- read.csv('~/R_Projects/30daychartchallenge/all_matches.csv')
ipl_1 <- raw_data |> 
  mutate_all(~replace_na(.,0)) |> 
  select(start_date, batting_team, bowling_team, 
         runs_off_bat, extras, wides, noballs,
         byes, legbyes, penalty) |> 
  mutate(total_runs = extras + runs_off_bat,
         isFour = if_else(runs_off_bat == 4, 1, 0),
         isSix = if_else(runs_off_bat == 6, 1, 0),
         start_date = ymd(start_date),
         year = year(start_date)) |> 
  filter(year <= 2023)
  
ipl_teams <- ipl_1 |> 
  distinct(batting_team) |> 
  pull()
ipl_teams <- ipl_teams[c(1:6, 8, 11, 15, 16, 17, 18)]
ipl_teams

ipl_teams2 <- ipl_teams[-c(4, 6)]
ipl_teams2

ipl_2 <- ipl_1 |> 
  mutate(batting_team = if_else(batting_team == ipl_teams[6],
                                ipl_teams[9],
                                batting_team),
         batting_team = if_else(batting_team == ipl_teams[4],
                                ipl_teams[10],
                                batting_team),
         bowling_team = if_else(bowling_team == ipl_teams[6],
                                ipl_teams[9],
                                bowling_team),
         bowling_team = if_else(bowling_team == ipl_teams[4],
                                ipl_teams[10],
                                bowling_team)) |> 
  filter(batting_team %in% ipl_teams2) |> 
  filter(bowling_team %in% ipl_teams2)

ipl2_sum <- ipl_2 |> 
  group_by(batting_team) |> 
  summarise(total_fours = sum(isFour),
            total_sixes = sum(isSix),
            total_score = sum(total_runs),
            fours = total_fours*4,
            sixes = total_sixes*6,
            non_boundaries = total_score - fours - sixes,
            fours_perc = round(fours/total_score*100, 2),
            sixes_perc = round(sixes/total_score*100, 2)) |> 
  arrange(desc(total_score)) |> 
  pivot_longer(cols = c("fours", "sixes", "non_boundaries"),
               names_to = "category",
               values_to = "value") |> 
  select(-total_fours, -total_sixes) |> 
  mutate(percent = round(value/total_score*100, 2))
# ipl2_sum

ipl2_sum$category <- factor(ipl2_sum$category, 
                            levels = c("non_boundaries", "fours", "sixes"))


font_add_google("Poppins", family = "poppins")
showtext_auto()

image_files <- list.files(pattern = "\\.jpg$|\\.jpeg$|\\.png$", 
                          full.names = TRUE)
imgs <- paste0("<img src='", image_files, "'/>")
# names(imgs) <- c("csk", "dc", "gt", "kkr", "lsg",
#                  "mi", "pbks", "rcb", "rr", "srh")

imgs
plt <- 
  ggplot(ipl2_sum, aes(y = reorder(batting_team, value),
                       x = value, fill = category)) +
  geom_col(position = "stack", color = "black") +
  # coord_flip() +
  scale_y_discrete(name = NULL, 
                   labels = c("<img src='./lsg.png' width='25' height='25'/>", 
                              "<img src='./gt.png' width='25' height='25'/>",
                              "<img src='./srh.png' width='25' height='25'/>",
                              "<img src='./rr.png' width='25' />",
                              "<img src='./dc.png' width='25' height='25'/>",
                              "<img src='./kkr.png' width='25' height='25'/>",
                              "<img src='./rcb.png' width='25' height='25'/>",
                              "<img src='./pbks.png' width='25' height='25'/>",
                              "<img src='./csk.png' width='25' height='25'/>",
                              "<img src='./mi.png' width='25' height='25'/>")) +
  theme(axis.text.y = element_markdown(),
        text = element_text(family = "poppins"),
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic"),
        plot.caption = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_markdown(),
        axis.text = element_text(size = 18, face = "bold"),
        axis.title = element_text(family = "poppins"),
        title = element_text(family = "poppins", face = "bold"),
        plot.background = element_rect(fill = "gray87"),
        panel.background = element_rect(fill = "gray80"),
        panel.grid.major = element_line(colour = "gray30")) +
  scale_fill_manual(labels = c("Non Boundaries", "4's", "6's"),
                    values = c("dodgerblue4", "darkgreen", "darkred")) +
  geom_text(aes(label = paste0(sprintf("%1.1f", percent),"%")),
            position = position_stack(vjust = 0.5), 
            colour = "white", size = 4.5, 
            family = "poppins", fontface = "bold") +
  labs(title = "Boundary Percentage of all teams in the history of IPL (2008 - 2023)",
       subtitle = "LSG and GT have participated in only two seasons of IPL",
       x = "Teams",
       y = "Total Runs",
       caption = "Data Source: www.cricsheet.com \n Made by: Samrit Pramanik")
plt
ggsave("30days_day1.png", plt, 
       dpi = 900,
       limitsize = T)




ipl_1$batting_team |> unique()
ipl_1$other_player_dismissed |> unique()
ipl_1$other_wicket_type |> unique()
ipl_1$penalty |> unique()
ipl_1$noballs |> unique()
ipl_1$start_date |> head(2)
ipl_1 |> filter(noballs == 0, runs_off_bat == 5) |> 
  select(batting_team, bowling_team, runs_off_bat, extras, noballs, total_runs) |> 
  head()





