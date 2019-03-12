#title: Make Shots Data Script
#description: A script to make a dataframe that we can produce NBA shotlines from
#input(s): andre-iguodala.csv, draymond-green.csv, kevin-durant.csv, klay-thompson.csv, stephen-curry.csv
#output(s): andre-iguodala-summary.txt, stephen-curry-summary.txt, kevin-durant-summary, klay-thompson-summary, draymond-green-summary, shots-data-summary

#resetting an existing files
rm(list = ls())

#Loading Packages
library(dplyr)
library(readr)

#Loading in Data
curry <- read_csv("../data/stephen-curry.csv")
iguodala <- read_csv("../data/andre-iguodala.csv")
durant <- read_csv("../data/kevin-durant.csv")
green <- read_csv("../data/draymond-green.csv")
thompson <- read_csv("../data/klay-thompson.csv")

#Mutating and manipulating data sets
curry <- 
  curry %>% 
  mutate(name = "Stephen Curry", #Adding vairale for player name
         shot_made_flag = case_when( #adding a variable for 
           shot_made_flag == "n" ~ "shot_no",
           shot_made_flag == "y" ~ "shot_yes"),
         minute = period*12 - minutes_remaining #adding variable for the time in minutes when the shot was made
         )

iguodala <- 
  iguodala %>% 
  mutate(name = "Andre Iguodala", #Adding vairale for player name
         shot_made_flag = case_when( #adding a variable for 
           shot_made_flag == "n" ~ "shot_no",
           shot_made_flag == "y" ~ "shot_yes"),
         minute = period*12 - minutes_remaining #adding variable for the time in minutes when the shot was made
  )

durant <- 
  durant %>% 
  mutate(name = "Kevin Durant", #Adding vairale for player name
         shot_made_flag = case_when( #adding a variable for 
           shot_made_flag == "n" ~ "shot_no",
           shot_made_flag == "y" ~ "shot_yes"),
         minute = period*12 - minutes_remaining #adding variable for the time in minutes when the shot was made
  )

green <- 
  green %>% 
  mutate(name = "Draymond Green", #Adding vairale for player name
         shot_made_flag = case_when( #adding a variable for 
           shot_made_flag == "n" ~ "shot_no",
           shot_made_flag == "y" ~ "shot_yes"),
         minute = period*12 - minutes_remaining #adding variable for the time in minutes when the shot was made
  )

thompson <- 
  thompson %>% 
  mutate(name = "Klay Thompson", #Adding vairale for player name
         shot_made_flag = case_when( #adding a variable for 
           shot_made_flag == "n" ~ "shot_no",
           shot_made_flag == "y" ~ "shot_yes"),
         minute = period*12 - minutes_remaining #adding variable for the time in minutes when the shot was made
  )


#exporting summaries to the output folder
sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()

sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
sink()

sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
sink()

sink(file = '../output/draymond-green-summary.txt')
summary(green)
sink()

#Creating shots_data df
shots_data <- rbind(iguodala, curry, durant, thompson, green)

#exporting shots-data.csv
write.csv(
  x = shots_data, # R object to be exported
  file = '../data/shots-data.csv'  # file path
)

#exporting shots-data summary
sink(file = '../output/shots-data-summary.txt')
summary(shots_data)
sink()


