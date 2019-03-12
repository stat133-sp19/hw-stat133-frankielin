# title: Make Shots Script
# description: A script to make and export charts made from the shots_data data_frame
# input(s): Please run make-shots-data-script.R, otherwise, make sure to have shots-data.csv and respective player CSVs
# output(s): 

# loading packages
library(dplyr)
library(readr)
library(grid)
library(ggplot2)
library(jpeg)

# loading in shots-made.csv
shots_data <-  as.tbl(read.csv("../data/shots-data.csv", row.names = 1))

curry <- filter(shots_data, name == "Stephen Curry")
durant <- filter(shots_data, name == "Kevin Durant")
green <- filter(shots_data, name == "Draymond Green")
iguodala <- filter(shots_data, name == "Andre Iguodala")
thompson <-filter(shots_data, name == "Klay Thompson")

# Preloading images
# court image (to be used as background of plot)
court_file <- "../images/nba-court.jpg"

# create raste object
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

# shot chart for players with court background
thompson_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag), size = .5) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()+ 
  scale_color_manual(values = c('#FDB927', '#006BB6'))

curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag), size = .5) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()+ 
  scale_color_manual(values = c('#FDB927', '#006BB6'))

durant_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag), size = .5) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()+ 
  scale_color_manual(values = c('#FDB927', '#006BB6'))

iguodala_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag), size = .5) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()+ 
  scale_color_manual(values = c('#FDB927', '#006BB6'))

green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag), size = .5) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()+ 
  scale_color_manual(values = c('#FDB927', '#006BB6'))

# Exporting shot charts as pdf to ../images
pdf('../images/klay-thompson-shot-chart.pdf',width=6.5,height=5) 
thompson_shot_chart
dev.off()

pdf('../images/draymond-green-shot-chart.pdf',width=6.5,height=5) 
green_shot_chart
dev.off()

pdf('../images/andre-iguodala-shot-chart.pdf',width=6.5,height=5) 
iguodala_shot_chart
dev.off()

pdf('../images/kevin-durant-shot-chart.pdf',width=6.5,height=5) 
durant_shot_chart
dev.off()

pdf('../images/stepen-curry-shot-chart.pdf',width=6.5,height=5) 
curry_shot_chart
dev.off()

# Faceted plot
all_player_shot_chart <- ggplot(data = shots_data) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag), size = .5) + 
  ylim(-50, 420) +
  facet_wrap(~name) +
  ggtitle('Shot Chart: Golden State Warriors (2016 season)') +
  theme_minimal() +
  theme(legend.position = "top") + 
  scale_color_manual(values = c('#FDB927', '#006BB6'), name = "")


pdf('../images/gsw-shot-chart.pdf',width=8, height = 7) 
all_player_shot_chart
dev.off()

png('../images/gsw-shot-chart.png', width =8, height = 7, units = "in", res = 800)
all_player_shot_chart
dev.off()

