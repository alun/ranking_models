library(tidyverse)

source('Config.R', prompt.echo =TRUE)
source('PlotConfig.R', prompt.echo =TRUE)

# load ATP source data
source_data <- readr::read_csv(files$file_in) %>%
  select(c(
    'winner_player_id',
    'loser_player_id',
    'winner_sets_won',
    'loser_sets_won'
  ))

# keep players with only the minimum number of sets played
MIN_SETS <- 10

# calculate sets played by all player, keep only those with more than MIN_SETS
sets_played <- union_all(
  # played as winner
  source_data %>%
    group_by(winner_player_id) %>%
    summarise(sets_played = sum(winner_sets_won) + sum(loser_sets_won)) %>%
    dplyr::rename(player_id = winner_player_id),
  # played as loser
  source_data %>%
    group_by(loser_player_id) %>%
    summarise(sets_played = sum(winner_sets_won) + sum(loser_sets_won)) %>%
    dplyr::rename(player_id = loser_player_id)
) %>%
  group_by(player_id) %>%
  summarise(sets_played = sum(sets_played)) %>%
  filter(sets_played >= MIN_SETS)


prepared_data <- source_data %>% filter(
  winner_player_id %in% sets_played$player_id &
    loser_player_id %in% sets_played$player_id
)

# double verify prepared_data players match players selected in sets_player
identical(sort(
  union(
    prepared_data$winner_player_id,
    prepared_data$loser_player_id
  )
), sort(sets_played$player_id)) # TRUE

# find the total number of players and the total number of sets they played
sets_played %>% summarize(total_players = n(),
                          total_sets = sum(sets_played))


# plot the distribution of players across sets played
save_plot(
  ggplot(sets_played,
         aes(x = sets_played)) +
    geom_histogram(
      bins = 20,
      linewidth = 1.5,
      alpha = colors$alpha,
      fill = colors$color_2,
      na.rm = TRUE
    ) +
    xlab("Sets played") +
    ylab("Count of players") +
    ggtitle("Sets played distribution"),
  file = charts$sets_played
)


# build a unique player index using integers starting from 1
players <- c(prepared_data$winner_player_id, prepared_data$loser_player_id)

player_index <- new.env(hash=TRUE)
n <- 1
for (player in players) {
  if (is.na(mget(player, ifnotfound=NA, envir = player_index))) {
    assign(player, n, envir = player_index)  
    n <- n + 1
  }
}

players <- ls(player_index)
get_player_index <- function (p) get(p, player_index)

player_index_df <- tibble(index = sapply(players, get_player_index),
                          player = players)

# store the index for future use
player_index_df %>% write_csv(files$index_out)


# store sets played per player
left_join(player_index_df, sets_played, by = join_by(player == player_id)) %>%
  select(index, sets_played) %>% write_csv(files$sets_played_out)

# prepare data for Bradley-Terry input

# number of total comparisons (sets)
N <- sum(prepared_data$winner_sets_won) + sum(prepared_data$loser_sets_won)

player0 <- rep(NA, N)
player1 <- rep(NA, N)
winner <- rep(NA, N)
i <- 1

for (r in 1:nrow(prepared_data)) {
  winner_sets_won <- prepared_data$winner_sets_won[r]
  loser_sets_won <- prepared_data$loser_sets_won[r]
  
  for (t in rep(NA, winner_sets_won)) {
    player0[i] = get_player_index(prepared_data$winner_player_id[r])
    player1[i] = get_player_index(prepared_data$loser_player_id[r])
    winner[i] = 0
    i <- i + 1
  }
  for (t in rep(NA, loser_sets_won)) {
    player0[i] = get_player_index(prepared_data$winner_player_id[r])
    player1[i] = get_player_index(prepared_data$loser_player_id[r])
    winner[i] = 1
    i <- i + 1
  }
}

bt <- tibble(player0=player0, player1=player1, winner=winner)
bt %>% write_csv(files$bradley_terry_out)

# prepare data for SBM inputs (adjastency matrix with the number of wins)

K <- max(player_index_df$index)
Y = matrix(0, nrow=K, ncol=K)

for (r in 1:nrow(prepared_data)) {
  winner_sets_won <- prepared_data$winner_sets_won[r]
  loser_sets_won <- prepared_data$loser_sets_won[r]
  
  winner_idx = get_player_index(prepared_data$winner_player_id[r])
  loser_idx = get_player_index(prepared_data$loser_player_id[r])
  
  Y[winner_idx, loser_idx] = Y[winner_idx, loser_idx] + winner_sets_won
  Y[loser_idx, winner_idx] = Y[loser_idx, winner_idx] + loser_sets_won
}

Y %>% write.csv(files$sbm_out, row.names = FALSE)

