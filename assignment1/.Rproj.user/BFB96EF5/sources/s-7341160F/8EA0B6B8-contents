library(tidyverse)
library(nnet)
library(caTools)
library(caret)
myData <- read.csv("games.csv")

###Converting the variables into the right types
myData <- myData %>% mutate_at(vars(1, 4:26, 32:51, 57:61), as.character)

###Replace 1 and 2 with team 1 and team 2
myData <- myData %>% mutate(winner = replace(winner, winner ==1,"team 1"),
                            firstBlood = replace(firstBlood, firstBlood ==1,"team 1"),
                            firstTower = replace(firstTower, firstTower ==1,"team 1"),
                            firstInhibitor = replace(firstInhibitor, firstInhibitor ==1,"team 1"),
                            firstBaron = replace(firstBaron, firstBaron ==1,"team 1"), 
                            firstDragon = replace(firstDragon, firstDragon ==1,"team 1"),
                            winner = replace(winner, winner ==2,"team 2"),
                            firstBlood = replace(firstBlood, firstBlood ==2,"team 2"),
                            firstTower = replace(firstTower, firstTower ==2,"team 2"),
                            firstInhibitor = replace(firstInhibitor, firstInhibitor ==2,"team 2"),
                            firstBaron = replace(firstBaron, firstBaron ==2,"team 2"), 
                            firstDragon = replace(firstDragon, firstDragon ==2,"team 2")) %>% 
  mutate_at(vars(1, 4:26, 32:51, 57:61), as.factor)

###Dropping unncessary column(s)
myData <- myData %>% select(-2)

head(myData)

###remove unncessary data###
myData <- myData %>% 
  select(-3)

###Visualization
ggplot(data = myData, aes(x = winner, fill = winner)) + 
  geom_bar() + 
  scale_fill_manual(values=c("blue", "red"))

winners <- myData %>%
  count(winner) %>%
  mutate(winPercentage = prop.table(n))


###Picks and bans###
picked <- myData %>% 
  select(t1_champ1id, t1_champ2id, t1_champ3id, t1_champ4id, t1_champ5id, t2_champ1id, t2_champ2id, t2_champ3id, t2_champ4id, t2_champ5id) %>% 
  gather(key, character) %>% 
  select(character) %>% 
  group_by(character) %>%
  summarise(totalNumbers = n())

ggplot(picked, aes(character, totalNumbers)) + geom_bar(stat = "identity", aes(fill = character)) + theme(legend.position = "none") + coord_flip()

bans <- myData %>% 
  select(t1_ban1, t1_ban2, t1_ban3, t1_ban4, t1_ban5, t2_ban1, t2_ban2, t2_ban3, t2_ban4, t2_ban5) %>% 
  gather(key, ban) %>% 
  select(ban) %>% 
  group_by(ban) %>%
  summarise(banNumbers = n()) %>%
  filter(ban>0)

ggplot(bans, aes(ban, banNumbers)) + geom_bar(stat = "identity", aes(fill = ban)) + theme(legend.position = "none") + coord_flip()


topPicked <- picked %>% 
  arrange(desc(totalNumbers)) %>% 
  top_n(15) %>% 
  arrange(character)

topBans <- bans %>% 
  arrange(desc(banNumbers)) %>% 
  top_n(15) %>% 
  arrange(ban) 

topTable <- cbind(topPicked, topBans)

topTable %>% 
  filter(character %in% ban) %>% 
  select(character)


###Win rate of the champions###
team1Win <- myData %>% 
  ###Get the players of the team and which team won for team 1
  select(winner, t1_champ1id, t1_champ2id, t1_champ3id, t1_champ4id, t1_champ5id) %>% 
  ###Change the variable to whether the team won or lost
  mutate(winner = case_when(winner == "team 1"~ 1, 
                            winner == "team 2" ~ 0, 
                            TRUE ~ 2)) %>% 
  ###Rename the variables so we can combine rows later
  rename(champ1id = t1_champ1id, champ2id = t1_champ2id, champ3id = t1_champ3id, champ4id = t1_champ4id, champ5id = t1_champ5id)


team2Win <- myData %>% 
  ###Get the players of the team and which team won for team 2
  select(winner, t2_champ1id, t2_champ2id, t2_champ3id, t2_champ4id, t2_champ5id) %>% 
  ###Change the variable to whether the team won or lost
  mutate(winner = case_when(winner == "team 1"~ 0, 
                            winner == "team 2" ~ 1, 
                            TRUE ~ 2)) %>% 
  ###Rename the variables so we can combine rows later
  rename(champ1id = t2_champ1id, champ2id = t2_champ2id, champ3id = t2_champ3id, champ4id = t2_champ4id, champ5id = t2_champ5id)

champWin <- bind_rows(team1Win, team2Win)

###Get frequency
champStats <- champWin %>% 
  group_by(champ1id, winner) %>%
  tally() %>%
  ###Get the total number of games played for each character
  group_by(champ1id) %>%
  mutate(total = sum(n))%>%
  ###Get the winning percentage
  mutate(percentage = n/total * 100) %>%                   
  filter(winner == 1) %>% 
  mutate(totalMean = case_when(total>746 ~1,
                               total<=746~0,
                               TRUE ~2)) %>% 
  mutate_at(6, as.character)

ggplot(data = champStats, aes(x = champ1id, y = percentage)) + geom_bar(stat = "identity", aes(color = total)) 



ggplot(myData, aes(x =gameDuration, y = t1_baronKills , colour = winner)) +geom_jitter() + 
  scale_color_manual(values=c("blue", "red"))

ggplot(myData, aes(x =gameDuration, y = t2_baronKills , colour = winner)) +geom_jitter() + 
  scale_color_manual(values=c("blue", "red"))


summoners <- myData %>% 
  select(t1_champ1_sum1, t1_champ1_sum2, t1_champ2_sum1, t1_champ1_sum2, t1_champ3_sum1, t1_champ3_sum2, t1_champ4_sum1, t1_champ4_sum2,
         t1_champ5_sum1, t1_champ5_sum2, t2_champ1_sum1, t2_champ1_sum2, t2_champ2_sum1, t2_champ2_sum2, t2_champ3_sum1, t2_champ3_sum2,
         t2_champ4_sum1, t2_champ4_sum2, t2_champ5_sum1, t2_champ5_sum2) %>% 
  gather(key, champ_sum) %>% 
  select(champ_sum) %>% 
  group_by(champ_sum) %>%
  summarise(totalNumbers = n())

ggplot(summoners, aes(champ_sum, totalNumbers)) + geom_bar(stat = "identity", aes(fill = champ_sum))


summoners %>% 
  mutate(percentage = totalNumbers / sum(totalNumbers))
