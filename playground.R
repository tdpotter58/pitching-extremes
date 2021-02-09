library(dplyr)
library(tidyr)
library(knitr)
library(readr)
library(ggplot2)
library(ggrepel)
library(stringr)

pitchers <- read_csv("pitcher-data.csv")

pitchers <- pitchers %>%
  select(-Pit, -Str) %>%
  drop_na()

#kable(pitchers)
pitchers <- pitchers %>%
  mutate(Year_ch = as.character(pitchers$Year))
         
for (i in pitchers$Rk){
  name <- str_split(pitchers[i,2], "-")[[1]]
  
  pitchers[i,2] <- str_c(name[1],pitchers[i,48], sep = " ")
}

theme_set(theme_bw())
pitchers %>%
  filter(`W-L%` < 0.5) %>%
  ggplot(aes(x = ERA, y = `W-L%`, label = Player)) + 
  geom_point(size = 2) +
  geom_label_repel(data = subset(pitchers, `W-L%` < 0.34), fill = 'cyan') +
  labs(caption = "Data from Baseball-Reference")

pitchers <- pitchers %>%
  mutate(Player.f = as.factor(pitchers$Player))

pitchers$Player.f <- factor(pitchers$Player.f, levels = pitchers$Player.f[order(pitchers$`W-L%`)])

pitchers %>%
  filter(`W-L%` < 0.5) %>%
  ggplot(aes(x = Player.f,y = `W-L%`, fill = ERA)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Winning Percentage") +
  ylab("Player") +
  ggtitle("Pitchers with an ERA Below 3.00 
          and a Winning Percentage Below 0.5 in a Season") +
  scale_fill_gradient(low = 'red', high = 'black') +
  coord_flip(ylim=c(0.3,0.5)) +
  labs(caption = "Players who qualified for ERA Title, since 1968.
       Data from Baseball-Reference") +
  theme(plot.title = element_text(hjust = 0.5)) 
#coord_cartesian(ylim=c(0.3,0.5)) 
