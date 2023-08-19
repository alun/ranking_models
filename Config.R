files <- list(
  file_in = 'data/input/match_scores_2017_unindexed_csv.csv',
  ranking_atp = 'data/input/rankings_1973-2017_csv.csv',
  index_out = 'data/produced/player_index.csv',
  sets_played_out = 'data/produced/sets_played.csv',
  bradley_terry_out = 'data/produced/bradley_terry_2017.csv',
  sbm_out = 'data/produced/sbm_2017.csv',
  ranking_bayes = 'data/produced/ranking_bayes.csv',
  ranking_p_strong_cluster = 'data/produced/ranking_p_strong_cluster.csv'
)

charts <- list(
  sets_played = "charts/sets_played.svg",
  prior_motivation = "charts/prior_motivation.svg",
  abilities = "charts/abilities.svg",
  ability_variance = "charts/ability_variance.svg",
  prob_winning_splits = "charts/prob_winning_splits.svg",
  rank_vs_cluster = "charts/rank_vs_cluster.svg",
  rank_vs_cluster_1_prob = "charts/rank_vs_cluster_1_prob.svg",
  sbm_input = "charts/sbm_input.svg",
  ability_vs_cluster = "charts/ability_vs_cluster.svg",
  cluster_sizes = "charts/cluster_sizes.svg",
  atp_vs_bt = "charts/atp_vs_bt.svg",
  atp_vs_pstrong.svg = "charts/atp_vs_pstrong.svg"
)

colors <- list(
  alpha = 0.7,
  color_0 = 'black',
  color_1 = 'green',
  color_2 = 'magenta'
)