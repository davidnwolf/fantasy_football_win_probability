
#Reading files from NFL-data on github, single game data from 2023

base_url <- "https://github.com/hvpkod/NFL-Data/tree/main/NFL-data-Players/2023"

get_folder_files <- function(folder_url) {
  tryCatch({
    folder_page <- read_html(folder_url)
    file_links <- folder_page %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      grep(".csv$", ., value = TRUE)
    
    raw_file_links <- paste0("https://raw.githubusercontent.com", gsub("/blob", "", file_links))
    return(raw_file_links)
  }, error = function(e) {
    message("Error reading folder: ", folder_url, "-", e$message)
    return(NULL)
  })
}

page <- read_html(base_url)

folder_links <- page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("/tree/main/NFL-data-Players/2023/", ., value = TRUE) %>%
  paste0("https://github.com", .)

all_file_links <- c()

for(folder in folder_links) {
  file_links <- get_folder_files(folder)
  if(!is.null(file_links)) {
    all_file_links <- unique(c(all_file_links, file_links))
  }
}

week_numbers <- c(1, 10:18, 2:9)

print(all_file_links)

all_weekly_data <- lapply(seq_along(all_file_links), function(i) {
  
  tryCatch({
    weekly_data <- read.csv(all_file_links[i], quote="", fill = TRUE, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", file_url, "-", e$message)
    return(NULL)
  })
})

all_weekly_data <- lapply(all_weekly_data, function(df) {
  missing_cols <- setdiff(all_columns, colnames(df))
  for(col in missing_cols) {
    df[[col]] <- NA
  }
  df <- df[all_columns]
  return(df)
})

#Cleaning data
combined_data <- bind_rows(all_weekly_data)

fantasy_positions <- c("QB", "RB", "WR", "TE", "K", "NA")
library(data.table)
combined_data <- combined_data[combined_data$Pos %in% fantasy_positions, ]
combined_data <- combined_data[combined_data$Team != "FA", ]
combined_data[is.na(combined_data)] <- 0

combined_data <- combined_data[combined_data$PlayerOpponent != "Bye", ]

cleaned_data <- combined_data %>%
  group_by(Team, PlayerOpponent)

cleaned_data <- cleaned_data %>%
  select(PlayerName, Pos, Team, PlayerOpponent, TotalPoints)

cleaned_data <- cleaned_data %>%
  group_by(Team, PlayerOpponent, Pos) %>%
  mutate(TeamRank = rank(-TotalPoints, ties.method = "min")) %>%
  ungroup()

cleaned_data <- cleaned_data %>% distinct()

#Calculate variances and correlations among top players
top_players <- cleaned_data %>%
  select(PlayerName, Pos, Team, PlayerOpponent, TotalPoints, TeamRank) %>%
  filter((Pos == "QB" & TeamRank == 1) |
         (Pos == "RB" & TeamRank <= 2) |
         (Pos == "WR" & TeamRank <= 2) |
         (Pos == "TE" & TeamRank <= 2) |
         (Pos == "K"  & TeamRank == 1))


top_players <- top_players[(top_players$Pos == "QB" & top_players$TeamRank == 1) |
                           (top_players$Pos == "RB" & top_players$TeamRank <= 2) |
                           (top_players$Pos == "WR" & top_players$TeamRank <= 2) |
                           (top_players$Pos == "TE" & top_players$TeamRank <= 2) |
                           (top_players$Pos == "K"  & top_players$TeamRank == 1), ]

top_scores <- top_players %>%
  group_by(Team, PlayerOpponent) %>%
  reframe(
    QB1_Points = first(TotalPoints[Pos == "QB" & TeamRank == 1], na.rm = TRUE),
    RB1_Points = first(TotalPoints[Pos == "RB" & TeamRank == 1], na.rm = TRUE),
    RB2_Points = first(TotalPoints[Pos == "RB" & TeamRank == 2], na.rm = TRUE),
    WR1_Points = first(TotalPoints[Pos == "WR" & TeamRank == 1], na.rm = TRUE),
    WR2_Points = first(TotalPoints[Pos == "WR" & TeamRank == 2], na.rm = TRUE),
    TE1_Points = first(TotalPoints[Pos == "TE" & TeamRank == 1], na.rm = TRUE),
    TE2_Points = first(TotalPoints[Pos == "TE" & TeamRank == 2], na.rm = TRUE),
    K_Points   = first(TotalPoints[Pos == "K"  & TeamRank == 1], na.rm = TRUE),
  )
top_scores <- top_scores[top_scores$QB1_Points != 0, ]


covariances <- matrix(c(1:64), nrow = 8)
correlations <- matrix(c(1:64), nrow = 8)

colnames <- colnames(top_scores)

for(i in 3:10){
  for(j in 3:10){
    covariances[i-2,j-2] <- cov(top_scores[ ,i],top_scores[ ,j], use = "complete.obs")
    correlations[i-2,j-2] <- cor(top_scores[ ,i], top_scores[ ,j], use = "complete.obs")
  }
}

#get projections

weeks <- c(1:17)

projected_links <- c()

fantasy_positions <- c("QB", "RB", "WR", "TE", "K")

for(week in weeks) {
  for (pos in fantasy_positions) {
    url <- paste0("https://github.com/hvpkod/NFL-Data/tree/main/NFL-data-Players/2023/", week, "/projected/", pos, "_projected.csv")
    projected_links <- c(projected_links, url)
  }
}
print(projected_links)
projected_links_raw <- gsub("https://github.com/(.*)/tree/(.*)", 
                            "https://raw.githubusercontent.com/\\1/\\2", 
                            projected_links)
projection_list <- list()
for(link in projected_links_raw) {
  tryCatch({
    data <- read.csv(link, stringsAsFactors = FALSE, quote = "\"", fill = TRUE)
    data$week <- ceiling(which(projected_links_raw == link)/5)
    projection_list[[link]] <- data
    }, warning = function(w) {
    message("Warning while reading: ", link, "-", conditionMessage(w))
    }, error = function(e) {
    message("Error while reading: ", link, "-", conditionMessage(e))
  })
}
combined_projections <- bind_rows(projection_list)
combined_projections <- combined_projections[combined_projections$Team != "FA", ]
combined_projections[is.na(combined_projections)] <- 0
QBs_projections <- combined_projections %>%
  filter(Pos == "QB") %>%
  select(week, PlayerName, Pos, Team, PlayerOpponent, PlayerWeekProjectedPts)

RBs_projections <- combined_projections %>%
  filter(Pos == "RB") %>%
  select(week, PlayerName, Pos, Team, PlayerOpponent, PlayerWeekProjectedPts)

WRs_projections <- combined_projections %>%
  filter(Pos == "WR") %>%
  select(week, PlayerName, Pos, Team, PlayerOpponent, PlayerWeekProjectedPts)

TEs_projections <- combined_projections %>%
  filter(Pos == "TE") %>%
  select(week, PlayerName, Pos, Team, PlayerOpponent, PlayerWeekProjectedPts)

Ks_projections <- combined_projections %>%
  filter(Pos == "K") %>%
  select(week, PlayerName, Pos, Team, PlayerOpponent, PlayerWeekProjectedPts)

Flex_projections <- bind_rows(RBs_projections, WRs_projections, TEs_projections)

QBs_projections <- QBs_projections %>%
  group_by(week) %>%
  mutate(ProjectedWeekRank = rank(-PlayerWeekProjectedPts, ties.method = "min")) %>%
  ungroup()

RBs_projections <- RBs_projections %>%
  group_by(week) %>%
  mutate(ProjectedWeekRank = rank(-PlayerWeekProjectedPts, ties.method = "min")) %>%
  ungroup()

WRs_projections <- WRs_projections %>%
  group_by(week) %>%
  mutate(ProjectedWeekRank = rank(-PlayerWeekProjectedPts, ties.method = "min")) %>%
  ungroup()

TEs_projections <- WRs_projections %>%
  group_by(week) %>%
  mutate(ProjectedWeekRank = rank(-PlayerWeekProjectedPts, ties.method = "min")) %>%
  ungroup()

Ks_projections <- Ks_projections %>%
  group_by(week) %>%
  mutate(ProjectedWeekRank = rank(-PlayerWeekProjectedPts, ties.method = "min")) %>%
  ungroup()

Flex_projections <- Flex_projections %>%
  group_by(week) %>%
  mutate(ProjectedWeekRank = rank(-PlayerWeekProjectedPts, ties.method = "min")) %>%
  ungroup()

fantasy_QBs <- QBs_projections$PlayerName[QBs_projections$week == 1 & QBs_projections$ProjectedWeekRank <= 24]
fantasy_RBs <- RBs_projections$PlayerName[RBs_projections$week == 1 & RBs_projections$ProjectedWeekRank <= 40]
fantasy_WRs <- WRs_projections$PlayerName[WRs_projections$week == 1 & WRs_projections$ProjectedWeekRank <= 40]
fantasy_TEs <- TEs_projections$PlayerName[TEs_projections$week == 1 & TEs_projections$ProjectedWeekRank <= 24]
fantasy_Flex <- Flex_projections$PlayerName[Flex_projections$week == 1 & Flex_projections$ProjectedWeekRank <= 100]
fantasy_Ks <- Ks_projections$PlayerName[Ks_projections$week == 1 & Ks_projections$ProjectedWeekRank <= 32]

combined_projections <- data.frame(Player = combined_projections$PlayerName,
                                   Week = combined_projections$week,
                                   Projected_Points = combined_projections$PlayerWeekProjectedPts,
                                   Team = combined_projections$Team,
                                   Opponent = combined_projections$PlayerOpponent)

combined_projections <- combined_projections[combined_projections$Projected_Points != 0, ]
   

#set up sample data frame using my fantasy football league                                
Fantasy_Teams <- data.frame(
  team = c("David", "Justin", "Mitch", "Barak", "Blake", "Jake",
           "Josh", "Alec", "Jonah", "Jordan", "Simon", "Max"),
  QB = character(12),
  RB1 = character(12),
  RB2 = character(12),
  WR1 = character(12),
  WR2 = character(12),
  TE = character(12),
  Flex = character(12),
  K = character(12),
  QB_projected = rep(0,12),
  RB1_projected = rep(0,12),
  RB2_projected = rep(0,12),
  WR1_projected = rep(0,12),
  WR2_projected = rep(0,12),
  TE_projected = rep(0,12),
  Flex_projected = rep(0,12),
  K_projected = rep(0,12),
  Team_Projected = rep(0,12)
)


Fantasy_Teams$QB <- sample(fantasy_QBs, size = 12, replace = FALSE)
Fantasy_Teams$QB_projected <- combined_projections$Projected_Points[match(Fantasy_Teams$QB, combined_projections$Player)]
Fantasy_Teams$RB1 <- sample(fantasy_RBs, size = 12, replace = FALSE)
Fantasy_Teams$RB1_projected <- combined_projections$Projected_Points[match(Fantasy_Teams$RB1, combined_projections$Player)]
Fantasy_Teams$RB2 <- sample(setdiff(fantasy_RBs, Fantasy_Teams$RB1), size = 12, replace = FALSE)
Fantasy_Teams$RB2_projected <- combined_projections$Projected_Points[match(Fantasy_Teams$RB2, combined_projections$Player)]
Fantasy_Teams$WR1 <- sample(fantasy_WRs, size = 12, replace = FALSE)
Fantasy_Teams$WR1_projected <- combined_projections$Projected_Points[match(Fantasy_Teams$WR1, combined_projections$Player)]
Fantasy_Teams$WR2 <- sample(setdiff(fantasy_WRs, Fantasy_Teams$WR1), size = 12, replace = FALSE)
Fantasy_Teams$WR2_projected <- combined_projections$Projected_Points[match(Fantasy_Teams$WR2, combined_projections$Player)]
Fantasy_Teams$TE <- sample(fantasy_TEs, size = 12, replace = FALSE)
Fantasy_Teams$TE_projected <- combined_projections$Projected_Points[match(Fantasy_Teams$TE, combined_projections$Player)]
used_players <- c(Fantasy_Teams$RB1, Fantasy_Teams$RB2, Fantasy_Teams$WR1, Fantasy_Teams$WR2, Fantasy_Teams$TE)
Fantasy_Teams$Flex <- sample(setdiff(fantasy_Flex, used_players), size = 12, replace = FALSE)
Fantasy_Teams$Flex_projected <- combined_projections$Projected_Points[match(Fantasy_Teams$Flex, combined_projections$Player)]
Fantasy_Teams$K <- sample(fantasy_Ks, size = 12, replace = FALSE)
Fantasy_Teams$K_projected <- combined_projections$Projected_Points[match(Fantasy_Teams$K, combined_projections$Player)]

Fantasy_Teams <- Fantasy_Teams %>%
  mutate(Team_Projected = rowSums(select(Fantasy_Teams, QB_projected, RB1_projected, RB2_projected, WR1_projected, 
         WR2_projected, TE_projected, Flex_projected, K_projected), na.rm = TRUE))

variances <- data.frame(
  Player = unique(combined_data$PlayerName),
  Position = character(n),
  Team = character(n),
  week1_points = rep(0,n),
  week2_points = rep(0,n),
  week3_points = rep(0,n),
  week4_points = rep(0,n),
  week5_points = rep(0,n),
  week6_points = rep(0,n),
  week7_points = rep(0,n),
  week8_points = rep(0,n),
  week9_points = rep(0,n),
  week10_points = rep(0,n),
  week11_points = rep(0,n),
  week12_points = rep(0,n),
  week13_points = rep(0,n),
  week14_points = rep(0,n),
  week15_points = rep(0,n),
  week16_points = rep(0,n),
  week17_points = rep(0,n)
)
variances$Position <- combined_data$Pos[match(variances$Player, combined_data$PlayerName)]
variances$Team <- combined_data$Team[match(variances$Player, combined_data$PlayerName)]

combined_data$game <- paste0(combined_data$Team, combined_data$PlayerOpponent)
combined_projections$game <- paste0(combined_projections$Team, combined_projections$PlayerOpponent)
combined_data$week <- combined_projections$week[match(combined_data$game, combined_projections$game)]

for (player in unique(variances$Player)){
  for(week in 1:17){
    col_number <- week + 3
    row_index <- which(variances$Player == player)
    week_points <- combined_data$TotalPoints[combined_data$week == week & combined_data$PlayerName == player]
    if (length(week_points) == 1) {
      variances[row_index, col_number] <- week_points
    } else {
      variances[row_index, col_number] <- NA
    }
  }
}

variances$Player_Variance <- apply(variances[,4:20], 1, var, na.rm = TRUE)

Fantasy_Teams$Team_Variance <- rep(0,12)

for(manager in Fantasy_Teams$manager){
  manager_index <- which(Fantasy_Teams$manager == manager)
  for(i in 2:9){
    variances_index <- which(variances$Player == Fantasy_Teams[manager_index,i])
    nfl_team <- variances$Team[variances_index]
    Fantasy_Teams$Team_Variance[manager_index] <- Fantasy_Teams$Team_Variance[manager_index] + variances$Player_Variance[variances_index]
    for(j in 2:9){
      if(j != i){
        variances_index_2 <- which(variances$Player == Fantasy_Teams[manager_index,j])
        nfl_team_2 <- variances$Team[variances_index_2]
        if(nfl_team == nfl_team_2){
          Fantasy_Teams$Team_Variance[manager_index] <- Fantasy_Teams$Team_Variance[manager_index] + 2 * covariances[i-1,j-1]
        }
      }
    }
  }
}

#Calculating win-probabilities against opponent

get_win_probability <- function(manager_1, manager_2){
  team_1 <- Fantasy_Teams[Fantasy_Teams$manager == manager_1,2:9]
  team_2 <- Fantasy_Teams[Fantasy_Teams$manager == manager_2,2:9]
  projected_matchup_difference <- Fantasy_Teams$Team_Projected[Fantasy_Teams$manager == manager_1] - Fantasy_Teams$Team_Projected[Fantasy_Teams$manager == manager_2]
  matchup_variance <- Fantasy_Teams$Team_Variance[Fantasy_Teams$manager == manager_1] + Fantasy_Teams$Team_Variance[Fantasy_Teams$manager == manager_2]
  for(i in team_1){
    for(j in team_2){
      nfl_team <- variances$Team[variances$Player == i]
      nfl_team_2 <- variances$Team[variances$Player == j]
      if(nfl_team == nfl_team_2){
        matchup_variance <- matchup_variance + 2 * covariances[which(team_1 == i),which(team_2 == j)]
      }
    }
  }
team_1_win_probability <- pnorm(projected_matchup_difference,0,sqrt(matchup_variance))
team_2_win_probability <- 1- team_1_win_probability
return(c(team_1_win_probability,team_2_win_probability))
}

