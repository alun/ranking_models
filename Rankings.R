library(tidyverse)

source('Config.R')
source('PlotConfig.R')

# load ATP rankings
player_index <- read_csv(files$index_out)

atp <-
  read_csv(files$ranking_atp, show_col_types = FALSE) %>%
  filter(week_title == '2017.11.20') %>%
  select(rank_number, player_id) %>%
  left_join(player_index, join_by(player_id == player)) %>%
  select(index, rank_number) %>%
  dplyr::rename(atp_rank = rank_number, player = index) %>%
  drop_na()

# join with B-T rankings

bradley_terry <- read_csv(files$ranking_bradley_terry)

ranks <-
  atp %>%
  left_join(bradley_terry, join_by(player)) %>%
  select(atp_rank, rank) %>%
  dplyr::rename(bt_rank = rank) %>%
  drop_na()

# save rankings comparison plot
save_plot(
  ggplot(ranks, aes(x=bt_rank, y=atp_rank)) +
    geom_abline(color=colors$color_2) +
    geom_point(alpha=colors$alpha) +
    xlim(0, 300) +
    ylim(0, 300) +
    xlab("Bradley-  Terry rank") +
    ylab("ATP rank") +
    ggtitle("Rank comparison"),
  charts$atp_vs_bt
)

# RMSE and correlation
sqrt(mean((ranks$atp_rank - ranks$bt_rank) ^ 2))
cor(ranks) 

# join with SBM rankings
p_strong <- read_csv(files$ranking_p_strong_cluster, show_col_types = FALSE)

ranks_2 <-
  atp %>%
  left_join(p_strong, by = join_by(player)) %>%
  select(atp_rank, rank) %>%
  dplyr::rename(pstrong_rank = rank) %>%
  drop_na()

# save rankings comparison plot
save_plot(
  ggplot(ranks_2, aes(x = pstrong_rank, y = atp_rank)) +
    geom_abline(color = colors$color_2) +
    geom_point(alpha = colors$alpha) +
    xlim(0, 300) +
    ylim(0, 300) +
    ylab("ATP rank") +
    xlab("Rank by P of strong cluster") +
    ggtitle("Rank comparison GSBM"),
  charts$atp_vs_pstrong
)

# RMSE and correlation
sqrt(mean((ranks_2$atp_rank - ranks_2$pstrong_rank) ^ 2))
cor(ranks_2) 
