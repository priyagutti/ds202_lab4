library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
defense <- read_excel('C:/Data Science/DS202//cyclonesFootball2020.xlsx', sheet = 'Defensive')
str(defense)
offense <- read_excel('C:/Data Science/DS202//cyclonesFootball2020.xlsx', sheet = 'Offensive')
bio <- read_excel('C:/Data Science/DS202//cyclonesFootball2020.xlsx', sheet = 'Biography')
defense$Name <- as.factor(defense$Name)
defense$Opponent_Opponent <- as.factor(defense$Opponent_Opponent)
offense$Name <- as.factor(offense$Name)
offense$Opponent_Opponent <- as.factor(offense$Opponent_Opponent)
bio$Name <- as.factor(bio$Name)
offClean <- offense %>%
  mutate(across(Receiving_REC:Receiving_TD, as.numeric))
defense1 <- defense_names %>% 
  mutate(across(Tackles_Solo:Pass_PB, as.numeric))

##Problem 3
bioClean <- bio1 %>% 
  separate(Height, c('feet', 'inches'), sep = '-', convert = TRUE, remove = FALSE) %>% 
  mutate(feet = 12*feet + inches) %>%
  select(-inches)
##Problem 4
defClean <- defense1 
str(defClean)
str(offClean)
str(bioClean)
##Part 2
offClean <- offClean %>%
pivot_longer(offClean, cols = Receiving_REC:Receiving_TD, names_to = "stat")
##Part 2 Question 2
df1<- offClean %>% 
  group_by(stat, Name) %>%
  summary(offClean$value)
## Problem 4
Oregon<- filter(offense, Opponent_Opponent == "Oregon")
Oklahoma <- filter(offense,Opponent_Opponent == "Oklahoma")
ggplot(offense,aes(x=Oregon$Receiving_YDS, y =Oklahoma$Receiving_YDS)) + geom_point()

##Biography 
bioClean <- bioClean %>%
  separate(Hometown, c('city', 'state'), sep = ',', convert = TRUE, remove = FALSE)

##Problem 5 of Biography
table(bioClean['state'])
##6 
##Brock Prody is an excellent player based on the statistics in offense and defense. 
##His receiving yards is the 47th rank and has a total of 1,648 yards.