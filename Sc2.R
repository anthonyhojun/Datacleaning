#libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(ggthemes)
library(gridExtra)

#load data
sc <- tbl_df(read.csv("/Users/antbae/OneDrive/R/Side_projects/SC2/korealadderstats.csv"))
sc <- sc[, c(1, 8, 15, 2:7, 9:14, 16:21)]

### Clean data ###

#create list to differentiate player's race and opponents race
pc <- c("PvP", "PvZ", "PvT")
zc <- c("ZvP", "ZvZ", "ZvT")
tc <- c("TvP", "TvZ", "TvT")

cp <- c("PvP", "TvP", "ZvP")
cz <- c("PvZ", "TvZ", "ZvZ")
ct <- c("PvT", "TvT", "ZvT")

#user race
sc <-
sc %>%
  gather(matchup, wins, -(1:3)) %>% 
  separate(matchup, c("Matchup", "Result"), sep = "\\.") %>%
  mutate(
    Race = ifelse(Matchup %in% pc, "Protoss",
           ifelse(Matchup %in% zc, "Zerg", "Terran"))
      )

#opponent race    
sc <-
sc %>%
  mutate(
    Opponent = ifelse(Matchup %in% cp, "Protoss",
               ifelse(Matchup %in% cz, "Zerg", "Terran"))
  )

#gather ranks
sc <-
sc %>%
  gather(Rank, Placement, 1:3) %>%
  select(7, 6, 1:5)

#to be able to quickly verify
sc <- 
sc %>%
  mutate(
    Rank = ifelse(Rank == "P.RANK", "Protoss",
           ifelse(Rank == "Z.RANK", "Zerg", "Terran"))
  ) 

#filter out to correct ranks
sc <-
sc %>%
  filter(Rank == Race) %>%
  arrange(Placement)

#clean up
sc <-
sc %>%
  select(-Race) %>%
  select(Placement, Rank, Opponent, Matchup, Result, wins) %>%
  rename(Race = Rank,
         Count = wins) %>%
  mutate_each(funs(as.factor), 
              c(Race,
                Opponent,
                Matchup,
                Result)
              )

### Summaries ### 
sc2 <-
sc 

#wins by player
by.player <-
sc2 %>%
  group_by(Placement, Race, Opponent) %>%
  mutate(
    Percentage = round(Count / sum(Count), 2)
  )

#wins by matchup
by.matchup <-
sc2 %>%
  group_by(Race, Opponent, Result) %>%
  summarise(
    Count = sum(Count)
  ) %>%
  mutate(
    percent = round(Count / sum(Count),2)
  ) %>%
  arrange(desc(Result))

wins.by.matchup <-
by.matchup %>%
  filter(Result == "Win")

#Wins by race
by.race <- 
sc2 %>%
  group_by(Race, Result) %>%
  summarise(
    Count = sum(Count)
  ) %>%
  mutate(
    percent = round(Count / sum(Count),2)
  ) %>%
  arrange(desc(Result))

wins.by.race <-
  by.race %>%
  filter(Result == "Win")

### Visualizations ###

#wins by race
ggplot(wins.by.race, aes(x = reorder(Race, percent), y = percent, 
                         label=percent, fill = as.factor(Race))) +
  geom_bar(stat = "identity") +
  theme_few() +
  scale_fill_few() + 
  expand_limits(y = c(0,1)) +
  coord_flip() +
  geom_text(nudge_y = .07) +
  ggtitle("Wins x Race") + 
  xlab("") +
  ylab("") +
  guides(fill = FALSE)

#wins by matchup
ggplot(wins.by.matchup, aes(x = Opponent, y = percent, label = percent,
                            fill = Opponent)) +
  geom_bar(stat = "identity") +
  facet_grid(~Race) +
  theme_calc() +
  scale_fill_few() + 
  expand_limits(y = c(0,1)) +
  geom_text(nudge_y = .07) +
  ggtitle("Wins x Matchup") + 
  xlab("") +
  ylab("") +
  guides(fill = FALSE) +
  theme(
    panel.grid.major = element_line(size = .1)
  )

#wins by placement

#by top 10
top10 <- 
  sc2 %>%
  distinct(Placement) %>%
  group_by(Race) %>%
  filter(Placement <= 10) %>%
  summarise(
    Count = n()
  )

ggplot(top10, aes(x = reorder(Race, -Count ), y = Count, label = Count)) + 
  geom_bar(aes(fill = Race), stat = "identity") +
  expand_limits(y = c(0,8)) +
  theme_few() +
  scale_fill_few() +
  guides(fill = FALSE) +
  xlab("") +
  ylab("") +
  ggtitle("Top 10 x Races") +
  geom_text(nudge_y = .3)

#by top 50
top50 <- 
  sc2 %>%
  distinct(Placement) %>%
  group_by(Race) %>%
  filter(Placement <= 50) %>%
  summarise(
    Count = n()
  )

top50.plot <-
  ggplot(top50, aes(x = reorder(Race, -Count ), y = Count, label = Count)) + 
  geom_bar(aes(fill = Race), stat = "identity") +
  expand_limits(y = c(0,8)) +
  theme_few() +
  scale_fill_few() +
  guides(fill = FALSE) +
  xlab("") +
  ylab("") +
  ggtitle("Top 50") +
  geom_text(nudge_y = .8) +
  theme(
    axis.ticks.y = element_blank(), 
    axis.text.y = element_blank()
  )

#51-100 
top51.100 <- 
  sc2 %>%
  distinct(Placement) %>%
  group_by(Race) %>%
  filter(Placement > 50 & Placement <= 100) %>%
  summarise(
    Count = n()
  )

top51.100.plot <- 
  ggplot(top51.100, aes(x = reorder(Race, -Count ), y = Count, label = Count)) + 
  geom_bar(aes(fill = Race), stat = "identity") +
  expand_limits(y = c(0,8)) +
  theme_few() +
  scale_fill_few() +
  guides(fill = FALSE) +
  xlab("") +
  ylab("") +
  ggtitle("51-100") +
  geom_text(nudge_y = .8) +
  theme(
    axis.ticks.y = element_blank(), 
    axis.text.y = element_blank()
  )

#top101.150
top101.150 <- 
  sc2 %>%
  distinct(Placement) %>%
  group_by(Race) %>%
  filter(Placement > 100 & Placement <= 150) %>%
  summarise(
    Count = n()
  )

top101.150.plot <- 
  ggplot(top101.150, aes(x = reorder(Race, -Count ), y = Count, label = Count)) + 
  geom_bar(aes(fill = Race), stat = "identity") +
  expand_limits(y = c(0,8)) +
  theme_few() +
  scale_fill_few() +
  guides(fill = FALSE) +
  xlab("") +
  ylab("") +
  ggtitle("101-150") +
  geom_text(nudge_y = .8) +
  theme(
    axis.ticks.y = element_blank(), 
    axis.text.y = element_blank()
  )

grid.arrange(top50.plot, top51.100.plot, top101.150.plot, nrow = 1)

#wins by placement

