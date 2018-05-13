# Author: John Sokol
# Data Visualization
# 1 May 2018

library(nflscrapR)
library(ggplot2)
library(dplyr)

# import Super Bowl data
super_bowl52 <- game_play_by_play(GameID = tail(extracting_gameids(2017, playoffs = TRUE), n = 1))
super_bowl51 <- game_play_by_play(GameID = tail(extracting_gameids(2016, playoffs = TRUE), n = 1))
super_bowl50 <- game_play_by_play(GameID = tail(extracting_gameids(2015, playoffs = TRUE), n = 1))
super_bowl49 <- game_play_by_play(GameID = tail(extracting_gameids(2014, playoffs = TRUE), n = 1))
super_bowl48 <- game_play_by_play(GameID = tail(extracting_gameids(2013, playoffs = TRUE), n = 1))
super_bowl47 <- game_play_by_play(GameID = tail(extracting_gameids(2012, playoffs = TRUE), n = 1))
super_bowl46 <- game_play_by_play(GameID = tail(extracting_gameids(2011, playoffs = TRUE), n = 1))
super_bowl45 <- game_play_by_play(GameID = tail(extracting_gameids(2010, playoffs = TRUE), n = 1))
super_bowl44 <- game_play_by_play(GameID = tail(extracting_gameids(2009, playoffs = TRUE), n = 1))

# Team colors 
# reference this for choosing Tableau color scheme for each game
library(teamcolors)
nfl_colors <- teamcolors %>% filter(league == "nfl")
print(nfl_colors, n = 32)

# Super Bowl Win Probabilities

# Super Bowl 52
# Philly @ New England
# these comments are applicable for all win probability queries; to prevent redudancy, only comments are shown here

# queries time remaining after each play, home team win probability, away team win probability, and play description 
eagles_pats <- data.frame(super_bowl52$TimeSecs,super_bowl52$Home_WP_post,super_bowl52$Away_WP_post, super_bowl52$desc)

# omit erroneous instances where home team win probability == away team win probability
eagles_pats_final <- na.omit(eagles_pats[!(eagles_pats$super_bowl52.Home_WP_post == eagles_pats$super_bowl52.Away_WP_post),])

# rename columns
colnames(eagles_pats_final) = c("time_remaining", "Home", "Away", "Play Description")

# Super Bowl 51
# New England @ Atlanta
atl_ne <- data.frame(super_bowl51$TimeSecs,super_bowl51$Home_WP_post,super_bowl51$Away_WP_post, super_bowl51$desc, rep("Super Bowl 51", nrow(atl_ne)))
atl_ne_final <- na.omit(atl_ne[!(atl_ne$super_bowl51.Home_WP_post == atl_ne$super_bowl51.Away_WP),])
colnames(atl_ne_final) = c("time_remaining", "Home", "Away", "Play Description", "Super Bowl")

# Super Bowl 50
# Carolina @ Denver
car_den <- data.frame(super_bowl50$TimeSecs,super_bowl50$Home_WP_post,super_bowl50$Away_WP_post, super_bowl50$desc, rep("Super Bowl 50", nrow(car_den)))
car_den_final <- na.omit(car_den[!(car_den$super_bowl50.Home_WP_post == car_den$super_bowl50.Away_WP),])
colnames(car_den_final) = c("time_remaining", "Home", "Away", "Play Description", "Super Bowl")

# Super Bowl 49 
# New England @ Seattle
ne_sea <- data.frame(super_bowl49$TimeSecs ,super_bowl49$Home_WP_post, super_bowl49$Away_WP_post, super_bowl49$desc, rep("Super Bowl 49", nrow(ne_sea)))
ne_sea_final <- na.omit(ne_sea[!(ne_sea$super_bowl49.Home_WP_post == ne_sea$super_bowl49.Away_WP_post),])
colnames(ne_sea_final) = c("time_remaining", "Home", "Away", "Play Description", "Super Bowl")

# Super Bowl 48
# Seattle @ Denver
sea_den <- data.frame(super_bowl48$TimeSecs, super_bowl48$Home_WP_post, super_bowl48$Away_WP_post, super_bowl48$desc, rep("Super Bowl 48", nrow(sea_den)))
sea_den_final <- na.omit(sea_den[!(sea_den$super_bowl48.Home_WP_post == sea_den$super_bowl48.Away_WP_post),])
colnames(sea_den_final) = c("time_remaining", "Home", "Away", "Play Description", "Super Bowl")

# Super Bowl 47
# Baltimore @ San Francisco
balt_sanfran <- data.frame(super_bowl47$TimeSecs, super_bowl47$Home_WP_post, super_bowl47$Away_WP_post, super_bowl47$desc, rep("Super Bowl 47", nrow(balt_sanfran)))
balt_sanfran_final <- na.omit(balt_sanfran[!(balt_sanfran$super_bowl47.Home_WP_post == balt_sanfran$super_bowl47.Away_WP_post),])
colnames(balt_sanfran_final) = c("time_remaining", "Home", "Away", "Play Description", "Super Bowl")

# Super Bowl 46
# New York Giants @ New England
nyg_ne <- data.frame(super_bowl46$TimeSecs, super_bowl46$Home_WP_post, super_bowl46$Away_WP_post, super_bowl46$desc, rep("Super Bowl 46", nrow(nyg_ne)))
nyg_ne_final <- na.omit(nyg_ne[!(nyg_ne$super_bowl46.Home_WP_post == nyg_ne$super_bowl46.Away_WP_post),])
colnames(nyg_ne_final) = c("time_remaining", "Home", "Away", "Play Description", "Super Bowl")

# Super Bowl 45
# Pittsburgh @ Green Bay
pit_gb <- data.frame(super_bowl45$TimeSecs, super_bowl45$Home_WP_post, super_bowl45$Away_WP_post, super_bowl45$desc, rep("Super Bowl 45", nrow(pit_gb)))
pit_gb_final <- na.omit(pit_gb[!(pit_gb$super_bowl45.Home_WP_post == pit_gb$super_bowl45.Away_WP_post),])
colnames(pit_gb_final) = c("time_remaining", "Home", "Away", "Play Description", "Super Bowl")

# Super Bowl 44
# New Orleans @ Indianapolis 
no_ind <- data.frame(super_bowl44$TimeSecs, super_bowl44$Home_WP_post, super_bowl44$Away_WP_post, super_bowl44$desc, rep("Super Bowl 44", nrow(no_ind)))
no_ind_final <- na.omit(no_ind[!(no_ind$super_bowl44.Home_WP_post == no_ind$super_bowl44.Away_WP_post),])
colnames(no_ind_final) = c("time_remaining", "Home", "Away", "Play Description", "Super Bowl")


master_df <- rbind(eagles_pats_final, atl_ne_final, car_den_final, ne_sea_final, sea_den_final,
balt_sanfran_final, nyg_ne_final, pit_gb_final, no_ind_final)

write.table(master_df, file = "super_bowl46_52.csv", row.names = FALSE)




# Tabulating the Scores after EVERY play for Super Bowl 46 - 52
# tabluates data in dataframe, then writes dataframe to CSV file to import into Tableau

# Super Bowl 52
# PHI Score
# filters by possession team 
sb52_phi_pos <- super_bowl52 %>% filter(super_bowl52$posteam == "PHI")

# queries time remaining and scores when Philly possessed the ball 
sb52_phi_pos <- data.frame(sb52_phi_pos$TimeSecs, sb52_phi_pos$PosTeamScore)

# rename columns
colnames(sb52_phi_pos) = c("TimeRemaining", "Score")
    
# filters by defensive team 
sb52_phi_def <- super_bowl52 %>% filter(super_bowl52$DefensiveTeam == "PHI")

# queries time remaining and scores when Philly played defense
sb52_phi_def <- data.frame(sb52_phi_def$TimeSecs, sb52_phi_def$DefTeamScore) 

# rename columns
colnames(sb52_phi_def) = c("TimeRemaining", "Score")

# join both possession and defensive dataframes by common field TimeRemaining
sb52_phi_merge <- merge(sb52_phi_pos, sb52_phi_def, by = "TimeRemaining", all = TRUE)

# reverses the dataframe so TimeRemaining is organized in decreasing order, also removes na values 
sb52_phi_scores  <- cbind(sb52_phi_merge[1], mycol = apply(sb52_phi_merge[-1], 1, max, na.rm = TRUE))
sb52_phi_scores <- sb52_phi_scores[dim(sb52_phi_scores)[1]:1,]

# rename columns
colnames(sb52_phi_scores) = c("TimeRemaining", "Away")
    
# NE Score
sb52_ne_pos <- super_bowl52 %>% filter(super_bowl52$posteam == "NE")
sb52_ne_pos <- data.frame(sb52_ne_pos$TimeSecs, sb52_ne_pos$PosTeamScore)
colnames(sb52_ne_pos) = c("TimeRemaining", "Score")
    
sb52_ne_def <- super_bowl52 %>% filter(super_bowl52$DefensiveTeam == "NE")
sb52_ne_def <- data.frame(sb52_ne_def$TimeSecs, sb52_ne_def$DefTeamScore) 
colnames(sb52_ne_def) = c("TimeRemaining", "Score")
    
sb52_ne_merge <- merge(sb52_ne_pos, sb52_ne_def, by = "TimeRemaining", all = TRUE)
    
sb52_ne_scores <- cbind(sb52_ne_merge[1], mycol = apply(sb52_ne_merge[-1], 1, max, na.rm = TRUE))
sb52_ne_scores  <- sb52_ne_scores[dim(sb52_ne_scores)[1]:1,]
colnames(sb52_ne_scores) = c("TimeRemaining", "Home")
    
# merge both PHI and NE Scores in one dataframe
sb52_scores <- merge(sb52_phi_scores, sb52_ne_scores, by = "TimeRemaining")
sb52_scores <- data.frame(sb52_scores[dim(sb52_scores)[1]:1,], rep("Super Bowl 52", nrow(sb52_scores)))
colnames(sb52_scores) = c("TimeRemaining", "Home", "Away","Super Bowl")



# Super Bowl 51
# ATL Score
sb51_atl_pos <- super_bowl51 %>% filter(super_bowl51$posteam == "ATL")
sb51_atl_pos <- data.frame(sb51_atl_pos$TimeSecs, sb51_atl_pos$PosTeamScore)
colnames(sb51_atl_pos) = c("TimeRemaining", "Score")

sb51_atl_def <- super_bowl51 %>% filter(super_bowl51$DefensiveTeam == "ATL")
sb51_atl_def <- data.frame(sb51_atl_def$TimeSecs, sb51_atl_def$DefTeamScore) 
colnames(sb51_atl_def) = c("TimeRemaining", "Score")

sb51_atl_merge <- merge(sb51_atl_pos, sb51_atl_def, by = "TimeRemaining", all = TRUE)

sb51_atl_scores  <- cbind(sb51_atl_merge[1], mycol = apply(sb51_atl_merge[-1], 1, max, na.rm = TRUE))
sb51_atl_scores <- sb51_atl_scores[dim(sb51_atl_scores)[1]:1,]
colnames(sb51_atl_scores) = c("TimeRemaining", "Away")

# NE Score

sb51_ne_pos <- super_bowl51 %>% filter(super_bowl51$posteam == "NE")
sb51_ne_pos <- data.frame(sb51_ne_pos$TimeSecs, sb51_ne_pos$PosTeamScore)
colnames(sb51_ne_pos) = c("TimeRemaining", "Score")

sb51_ne_def <- super_bowl51 %>% filter(super_bowl51$DefensiveTeam == "NE")
sb51_ne_def <- data.frame(sb51_ne_def$TimeSecs, sb51_ne_def$DefTeamScore) 
colnames(sb51_ne_def) = c("TimeRemaining", "Score")

sb51_ne_merge <- merge(sb51_ne_pos, sb51_ne_def, by = "TimeRemaining", all = TRUE)

sb51_ne_scores  <- cbind(sb51_ne_merge[1], mycol = apply(sb51_ne_merge[-1], 1, max, na.rm = TRUE))
sb51_ne_scores <- sb51_ne_scores[dim(sb51_ne_scores)[1]:1,]
colnames(sb51_ne_scores) = c("TimeRemaining", "Home")
    
# merge both ATL and NE Scores in one dataframe
sb51_scores <- merge(sb51_atl_scores, sb51_ne_scores, by = "TimeRemaining")
sb51_scores <- data.frame(sb51_scores[dim(sb51_scores)[1]:1,], rep("Super Bowl 51", nrow(sb51_scores)))
colnames(sb51_scores) = c("TimeRemaining","Home", "Away", "Super Bowl")


# modify score to incldue touchdown extra point
sb51_scores[79:111,2] <- 21
sb51_scores[112:193,2] <- 28
sb51_scores[190:193,3] <- 34
    
# Super Bowl 50
# CAR
sb50_car_pos <- super_bowl50 %>% filter(super_bowl50$posteam == "CAR")
sb50_car_pos <- data.frame(sb50_car_pos$TimeSecs, sb50_car_pos$PosTeamScore)
colnames(sb50_car_pos) = c("TimeRemaining", "Score")

sb50_car_def <- super_bowl50 %>% filter(super_bowl50$DefensiveTeam == "CAR")
sb50_car_def <- data.frame(sb50_car_def$TimeSecs, sb50_car_def$DefTeamScore) 
colnames(sb50_car_def) = c("TimeRemaining", "Score")

sb50_car_merge <- merge(sb50_car_pos, sb50_car_def, by = "TimeRemaining", all = TRUE)

sb50_car_scores  <- cbind(sb50_car_merge[1], mycol = apply(sb50_car_merge[-1], 1, max, na.rm = TRUE))
sb50_car_scores <- sb50_car_scores[dim(sb50_car_scores)[1]:1,]
colnames(sb50_car_scores) = c("TimeRemaining", "Away")
    
# DEN
sb50_den_pos <- super_bowl50 %>% filter(super_bowl50$posteam == "DEN")
sb50_den_pos <- data.frame(sb50_den_pos$TimeSecs, sb50_den_pos$PosTeamScore)
colnames(sb50_den_pos) = c("TimeRemaining", "Score")

sb50_den_def <- super_bowl50 %>% filter(super_bowl50$DefensiveTeam == "DEN")
sb50_den_def <- data.frame(sb50_den_def$TimeSecs, sb50_den_def$DefTeamScore) 
colnames(sb50_den_def) = c("TimeRemaining", "Score")

sb50_den_merge <- merge(sb50_den_pos, sb50_den_def, by = "TimeRemaining", all = TRUE)

sb50_den_scores  <- cbind(sb50_den_merge[1], mycol = apply(sb50_den_merge[-1], 1, max, na.rm = TRUE))
sb50_den_scores <- sb50_den_scores[dim(sb50_den_scores)[1]:1,]
colnames(sb50_den_scores) = c("TimeRemaining", "Home")

sb50_scores <- merge(sb50_car_scores, sb50_den_scores, by = "TimeRemaining")
sb50_scores <- data.frame(sb50_scores[dim(sb50_scores)[1]:1,], rep("Super Bowl 50", nrow(sb50_scores)))
colnames(sb50_scores) = c("TimeRemaining", "Away", "Home", "Super Bowl")

# modify score to incldue touchdown extra point
# Away team: CAR
sb50_scores[47:147,2] <- 7
sb50_scores[148:187,2] <- 10

# Home team: DEN
sb50_scores[23:62,3] <- 10
sb50_scores[63:112,3] <- 13
sb50_scores[113:169,3] <- 16
sb50_scores[170:187,3] <- 24


# Super Bowl 49 
# NE
sb49_ne_pos <- super_bowl49 %>% filter(super_bowl49$posteam == "NE")
sb49_ne_pos <- data.frame(sb49_ne_pos$TimeSecs, sb49_ne_pos$PosTeamScore)
colnames(sb49_ne_pos) = c("TimeRemaining", "Score")

sb49_ne_def <- super_bowl49 %>% filter(super_bowl49$DefensiveTeam == "NE")
sb49_ne_def <- data.frame(sb49_ne_def$TimeSecs, sb49_ne_def$DefTeamScore) 
colnames(sb49_ne_def) = c("TimeRemaining", "Score")

sb49_ne_merge <- merge(sb49_ne_pos, sb49_ne_def, by = "TimeRemaining", all = TRUE)

sb49_ne_scores  <- cbind(sb49_ne_merge[1], mycol = apply(sb49_ne_merge[-1], 1, max, na.rm = TRUE))
sb49_ne_scores <- sb49_ne_scores[dim(sb49_ne_scores)[1]:1,]
colnames(sb49_ne_scores) = c("TimeRemaining", "Away")

# SEA
sb49_sea_pos <- super_bowl49 %>% filter(super_bowl49$posteam == "SEA")
sb49_sea_pos <- data.frame(sb49_sea_pos$TimeSecs, sb49_sea_pos$PosTeamScore)
colnames(sb49_sea_pos) = c("TimeRemaining", "Score")

sb49_sea_def <- super_bowl49 %>% filter(super_bowl49$DefensiveTeam == "SEA")
sb49_sea_def <- data.frame(sb49_sea_def$TimeSecs, sb49_sea_def$DefTeamScore) 
colnames(sb49_sea_def) = c("TimeRemaining", "Score")

sb49_sea_merge <- merge(sb49_sea_pos, sb49_sea_def, by = "TimeRemaining", all = TRUE)

sb49_sea_scores  <- cbind(sb49_sea_merge[1], mycol = apply(sb49_sea_merge[-1], 1, max, na.rm = TRUE))
sb49_sea_scores <- sb49_sea_scores[dim(sb49_sea_scores)[1]:1,]
colnames(sb49_sea_scores) = c("TimeRemaining", "Home")

sb49_scores <- merge(sb49_ne_scores, sb49_sea_scores, by = "TimeRemaining")
sb49_scores <- data.frame(sb49_scores[dim(sb49_scores)[1]:1,], rep("Super Bowl 49", nrow(sb49_scores)))
colnames(sb49_scores) = c("TimeRemaining", "Away","Home", "Super Bowl")

# Super Bowl 48
# SEA
sb48_sea_pos <- super_bowl48 %>% filter(super_bowl48$posteam == "SEA")
sb48_sea_pos <- data.frame(sb48_sea_pos$TimeSecs, sb48_sea_pos$PosTeamScore)
colnames(sb48_sea_pos) = c("TimeRemaining", "Score")

sb48_sea_def <- super_bowl48 %>% filter(super_bowl48$DefensiveTeam == "SEA")
sb48_sea_def <- data.frame(sb48_sea_def$TimeSecs, sb48_sea_def$DefTeamScore) 
colnames(sb48_sea_def) = c("TimeRemaining", "Score")

sb48_sea_merge <- merge(sb48_sea_pos, sb48_sea_def, by = "TimeRemaining", all = TRUE)

sb48_sea_scores  <- cbind(sb48_sea_merge[1], mycol = apply(sb48_sea_merge[-1], 1, max, na.rm = TRUE))
sb48_sea_scores <- sb48_sea_scores[dim(sb48_sea_scores)[1]:1,]
colnames(sb48_sea_scores) = c("TimeRemaining", "Away")

# DEN
sb48_den_pos <- super_bowl48 %>% filter(super_bowl48$posteam == "DEN")
sb48_den_pos <- data.frame(sb48_den_pos$TimeSecs, sb48_den_pos$PosTeamScore)
colnames(sb48_den_pos) = c("TimeRemaining", "Score")

sb48_den_def <- super_bowl48 %>% filter(super_bowl48$DefensiveTeam == "DEN")
sb48_den_def <- data.frame(sb48_den_def$TimeSecs, sb48_den_def$DefTeamScore) 
colnames(sb48_den_def) = c("TimeRemaining", "Score")

sb48_den_merge <- merge(sb48_den_pos, sb48_den_def, by = "TimeRemaining", all = TRUE)

sb48_den_scores  <- cbind(sb48_den_merge[1], mycol = apply(sb48_den_merge[-1], 1, max, na.rm = TRUE))
sb48_den_scores <- sb48_den_scores[dim(sb48_den_scores)[1]:1,]
colnames(sb48_den_scores) = c("TimeRemaining", "Home")

sb48_scores <- merge(sb48_sea_scores, sb48_den_scores, by = "TimeRemaining")
sb48_scores <- data.frame(sb48_scores[dim(sb48_scores)[1]:1,], rep("Super Bowl 48", nrow(sb48_scores)))
colnames(sb48_scores) = c("TimeRemaining", "Away", "Home", "Super Bowl")

# modify score to incldue touchdown extra point
sb48_scores[69:84,2] <- 22
sb48_scores[85:112,2] <- 29
sb48_scores[113:130,2] <- 36
sb48_scores[131:159,2] <- 43

# Super Bowl 47
# BAL
sb47_bal_pos <- super_bowl47 %>% filter(super_bowl47$posteam == "BAL")
sb47_bal_pos <- data.frame(sb47_bal_pos$TimeSecs, sb47_bal_pos$PosTeamScore)
colnames(sb47_bal_pos) = c("TimeRemaining", "Score")

sb47_bal_def <- super_bowl47 %>% filter(super_bowl47$DefensiveTeam == "BAL")
sb47_bal_def <- data.frame(sb47_bal_def$TimeSecs, sb47_bal_def$DefTeamScore) 
colnames(sb47_bal_def) = c("TimeRemaining", "Score")

sb47_bal_merge <- merge(sb47_bal_pos, sb47_bal_def, by = "TimeRemaining", all = TRUE)

sb47_bal_scores  <- cbind(sb47_bal_merge[1], mycol = apply(sb47_bal_merge[-1], 1, max, na.rm = TRUE))
sb47_bal_scores <- sb47_bal_scores[dim(sb47_bal_scores)[1]:1,]
colnames(sb47_bal_scores) = c("TimeRemaining", "Away")


# SF
sb47_sf_pos <- super_bowl47 %>% filter(super_bowl47$posteam == "SF")
sb47_sf_pos <- data.frame(sb47_sf_pos$TimeSecs, sb47_sf_pos$PosTeamScore)
colnames(sb47_sf_pos) = c("TimeRemaining", "Score")

sb47_sf_def <- super_bowl47 %>% filter(super_bowl47$DefensiveTeam == "SF")
sb47_sf_def <- data.frame(sb47_sf_def$TimeSecs, sb47_sf_def$DefTeamScore) 
colnames(sb47_sf_def) = c("TimeRemaining", "Score")

sb47_sf_merge <- merge(sb47_sf_pos, sb47_sf_def, by = "TimeRemaining", all = TRUE)

sb47_sf_scores  <- cbind(sb47_sf_merge[1], mycol = apply(sb47_sf_merge[-1], 1, max, na.rm = TRUE))
sb47_sf_scores <- sb47_sf_scores[dim(sb47_sf_scores)[1]:1,]
colnames(sb47_sf_scores) = c("TimeRemaining", "Home")

sb47_scores <- merge(sb47_bal_scores, sb47_sf_scores, by = "TimeRemaining")
sb47_scores <- data.frame(sb47_scores[dim(sb47_scores)[1]:1,], rep("Super Bowl 47", nrow(sb47_scores)))
colnames(sb47_scores) = c("TimeRemaining", "Away","Home", "Super Bowl")

# Super Bowl 46
# NYG
sb46_nyg_pos <- super_bowl46 %>% filter(super_bowl46$posteam == "NYG")
sb46_nyg_pos <- data.frame(sb46_nyg_pos$TimeSecs, sb46_nyg_pos$PosTeamScore)
colnames(sb46_nyg_pos) = c("TimeRemaining", "Score")

sb46_nyg_def <- super_bowl46 %>% filter(super_bowl46$DefensiveTeam == "NYG")
sb46_nyg_def <- data.frame(sb46_nyg_def$TimeSecs, sb46_nyg_def$DefTeamScore) 
colnames(sb46_nyg_def) = c("TimeRemaining", "Score")

sb46_nyg_merge <- merge(sb46_nyg_pos, sb46_nyg_def, by = "TimeRemaining", all = TRUE)

sb46_nyg_scores  <- cbind(sb46_nyg_merge[1], mycol = apply(sb46_nyg_merge[-1], 1, max, na.rm = TRUE))
sb46_nyg_scores <- sb46_nyg_scores[dim(sb46_nyg_scores)[1]:1,]
colnames(sb46_nyg_scores) = c("TimeRemaining", "Away")

# NE
sb46_ne_pos <- super_bowl46 %>% filter(super_bowl46$posteam == "NE")
sb46_ne_pos <- data.frame(sb46_ne_pos$TimeSecs, sb46_ne_pos$PosTeamScore)
colnames(sb46_ne_pos) = c("TimeRemaining", "Score")

sb46_ne_def <- super_bowl46 %>% filter(super_bowl46$DefensiveTeam == "NE")
sb46_ne_def <- data.frame(sb46_ne_def$TimeSecs, sb46_ne_def$DefTeamScore) 
colnames(sb46_ne_def) = c("TimeRemaining", "Score")

sb46_ne_merge <- merge(sb46_ne_pos, sb46_ne_def, by = "TimeRemaining", all = TRUE)

sb46_ne_scores  <- cbind(sb46_ne_merge[1], mycol = apply(sb46_ne_merge[-1], 1, max, na.rm = TRUE))
sb46_ne_scores <- sb46_ne_scores[dim(sb46_ne_scores)[1]:1,]
colnames(sb46_ne_scores) = c("TimeRemaining", "Home")

sb46_scores <- merge(sb46_nyg_scores, sb46_ne_scores, by = "TimeRemaining")
sb46_scores <- data.frame(sb46_scores[dim(sb46_scores)[1]:1,], rep("Super Bowl 46", nrow(sb46_scores)))
colnames(sb46_scores) = c("TimeRemaining", "Away", "Home", "Super Bowl")

sb46_scores[30:62,3] <- 14 

# Super Bowl 45
# PIT
sb45_pit_pos <- super_bowl45 %>% filter(super_bowl45$posteam == "PIT")
sb45_pit_pos <- data.frame(sb45_pit_pos$TimeSecs, sb45_pit_pos$PosTeamScore)
colnames(sb45_pit_pos) = c("TimeRemaining", "Score")

sb45_pit_def <- super_bowl45 %>% filter(super_bowl45$DefensiveTeam == "PIT")
sb45_pit_def <- data.frame(sb45_pit_def$TimeSecs, sb45_pit_def$DefTeamScore) 
colnames(sb45_pit_def) = c("TimeRemaining", "Score")

sb45_pit_merge <- merge(sb45_pit_pos, sb45_pit_def, by = "TimeRemaining", all = TRUE)

sb45_pit_scores  <- cbind(sb45_pit_merge[1], mycol = apply(sb45_pit_merge[-1], 1, max, na.rm = TRUE))
sb45_pit_scores <- sb45_pit_scores[dim(sb45_pit_scores)[1]:1,]
colnames(sb45_pit_scores) = c("TimeRemaining", "Away")

# GB
sb45_gb_pos <- super_bowl45 %>% filter(super_bowl45$posteam == "GB")
sb45_gb_pos <- data.frame(sb45_gb_pos$TimeSecs, sb45_gb_pos$PosTeamScore)
colnames(sb45_gb_pos) = c("TimeRemaining", "Score")

sb45_gb_def <- super_bowl45 %>% filter(super_bowl45$DefensiveTeam == "GB")
sb45_gb_def <- data.frame(sb45_gb_def$TimeSecs, sb45_gb_def$DefTeamScore) 
colnames(sb45_gb_def) = c("TimeRemaining", "Score")

sb45_gb_merge <- merge(sb45_gb_pos, sb45_gb_def, by = "TimeRemaining", all = TRUE)

sb45_gb_scores  <- cbind(sb45_gb_merge[1], mycol = apply(sb45_gb_merge[-1], 1, max, na.rm = TRUE))
sb45_gb_scores <- sb45_gb_scores[dim(sb45_gb_scores)[1]:1,]
colnames(sb45_gb_scores) = c("TimeRemaining", "Home")

sb45_scores <- merge(sb45_pit_scores, sb45_gb_scores, by = "TimeRemaining")
sb45_scores <- data.frame(sb45_scores[dim(sb45_scores)[1]:1,], rep("Super Bowl 45", nrow(sb45_scores)))
colnames(sb45_scores) = c("TimeRemaining", "Away","Home", "Super Bowl")

# modify score
sb45_scores[30:62,3] <- 14 
sb45_scores[63:126,3] <- 21
sb45_scores[127:149,3] <- 28
sb45_scores[150:158,3] <- 31 

# Super Bowl 44
# NO
sb44_no_pos <- super_bowl44 %>% filter(super_bowl44$posteam == "NO")
sb44_no_pos <- data.frame(sb44_no_pos$TimeSecs, sb44_no_pos$PosTeamScore)
colnames(sb44_no_pos) = c("TimeRemaining", "Score")

sb44_no_def <- super_bowl44 %>% filter(super_bowl44$DefensiveTeam == "NO")
sb44_no_def <- data.frame(sb44_no_def$TimeSecs, sb44_no_def$DefTeamScore) 
colnames(sb44_no_def) = c("TimeRemaining", "Score")

sb44_no_merge <- merge(sb44_no_pos, sb44_no_def, by = "TimeRemaining", all = TRUE)

sb44_no_scores  <- cbind(sb44_no_merge[1], mycol = apply(sb44_no_merge[-1], 1, max, na.rm = TRUE))
sb44_no_scores <- sb44_no_scores[dim(sb44_no_scores)[1]:1,]
colnames(sb44_no_scores) = c("TimeRemaining", "Away")

# IND
sb44_ind_pos <- super_bowl44 %>% filter(super_bowl44$posteam == "IND")
sb44_ind_pos <- data.frame(sb44_ind_pos$TimeSecs, sb44_ind_pos$PosTeamScore)
colnames(sb44_ind_pos) = c("TimeRemaining", "Score")

sb44_ind_def <- super_bowl44 %>% filter(super_bowl44$DefensiveTeam == "IND")
sb44_ind_def <- data.frame(sb44_ind_def$TimeSecs, sb44_ind_def$DefTeamScore) 
colnames(sb44_ind_def) = c("TimeRemaining", "Score")

sb44_ind_merge <- merge(sb44_ind_pos, sb44_ind_def, by = "TimeRemaining", all = TRUE)

sb44_ind_scores  <- cbind(sb44_ind_merge[1], mycol = apply(sb44_ind_merge[-1], 1, max, na.rm = TRUE))
sb44_ind_scores <- sb44_ind_scores[dim(sb44_ind_scores)[1]:1,]
colnames(sb44_ind_scores) = c("TimeRemaining", "Home")

sb44_scores <- merge(sb44_no_scores, sb44_ind_scores, by = "TimeRemaining")
sb44_scores <- data.frame(sb44_scores[dim(sb44_scores)[1]:1,], rep("Super Bowl 44", nrow(sb44_scores)))
colnames(sb44_scores) = c("TimeRemaining", "Away", "Home", "Super Bowl")
    
# modify score to incldue touchdown extra point
sb44_scores[136:148,2] <- 31 

#####
# Super Bowls that need score modifications: 6 out of 9 games
# Super Bowl 51: MAJOR, score should be 34 - 28 New England OK
# Super Bowl 50: MAJOR, Score should be 24 - 10 Denver OK
# Super Bowl 48: Should be 43 - 8 Seattle OK
# Super Bowl 45: Should be 31 - 25 Packers OK
# Super Bowl 44: Should be 31 - 17 New Orleans OK

# Merge all dataframes together: 

master_scores_df <- rbind(sb52_scores, sb51_scores, sb50_scores, sb49_scores, sb48_scores, 
sb47_scores, sb46_scores, sb45_scores, sb44_scores)

write.table(master_scores_df, file = "super_bowl_scores.csv", row.names = FALSE)

