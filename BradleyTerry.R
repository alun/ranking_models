library(ggplot2)
library(ggpubr)
library(reshape)
library(rstan)
library(tidyverse)
library(boot)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores(logical = FALSE))

source('Config.R')
source('PlotConfig.R')

# data
outcomes <- read_csv(files$bradley_terry_out)
player_index <- read_csv(files$index_out)
sets_played <- read_csv(files$sets_played_out)
K <- nrow(player_index)
head(outcomes)

# the Bradley-Terry Model
bradley_terry <- stan_model("bradley_terry.stan")
model_data <- list(
  K = K,
  N = nrow(outcomes),
  player1 = outcomes$player1,
  player0 = outcomes$player0,
  y = outcomes$winner
)

# fit the model and sample from the posterior
posterior <- sampling(bradley_terry, data = model_data)

# extract posterior player abilities
extracted_pars <- rstan::extract(posterior, pars = c('alpha'))
abilities_sample <- as_tibble(extracted_pars$alpha)

abilities <- as_tibble(cbind(
  1:K, 
  t(abilities_sample %>% summarize(across(everything(), mean))),
  t(abilities_sample %>% summarize(across(everything(), ~quantile(., probs = 0.05)))),
  t(abilities_sample %>% summarize(across(everything(), ~quantile(., probs = 0.50)))),
  t(abilities_sample %>% summarize(across(everything(), ~quantile(., probs = 0.95))))
))
names(abilities) <- c('player', 'alpha_mean', 'alpha_p05', 'alpha_p50', 'alpha_p95')

# rank player based on average ability
ranking <-
  abilities %>% 
  left_join(sets_played, join_by(player == index)) %>%
  arrange(desc(alpha_mean))
ranking$rank = 1:K

# save estimated rankings 
write_csv(ranking, files$ranking_bradley_terry)

# rankings/abilities plot
save_plot(
  ggplot(ranking, aes(x = rank, y = alpha_mean)) +
    geom_ribbon(
      aes(ymin = alpha_p05, ymax = alpha_p95),
      alpha = colors$alpha,
      fill = colors$color_1
    ) +
    geom_point(color = colors$color_0, alpha = colors$alpha) +
    geom_line(color = colors$color_0) +
    xlab("Rank") +
    ylab("Ability") +
    ggtitle("Ranking by ability"),
  file = charts$abilities
)

# study the variance in estimated abitlities
abilities_by_rank <- abilities_sample[, ranking$player]
ranking$ability_variance <-
  sapply(as.data.frame(abilities_by_rank), var)

save_plot(
  ggplot(ranking, aes(x = sets_played, y = 1 / ability_variance)) +
    geom_point(color = colors$color_2, alpha = colors$alpha) +
    xlab("Sets played") +
    ylab("Ability variance reciprocal") +
    ggtitle("Variance of estimated abilities"),
  file = charts$ability_variance
)

# motivation for prior
set.seed(332)
N <- 1000

plot.p.win <- function(sd, first = FALSE) {
  alpha_1 <- rnorm(n = N, mean = 1, sd = sd)
  alpha_2 <- rnorm(n = N, mean = 1, sd = sd)
  
  lab <- ""
  if (first) {
    lab <- "Density"
  }
  
  ggplot(tibble(p = inv.logit(alpha_1 - alpha_2)), aes(x = p)) +
    stat_bin(
      alpha = 0.5,
      fill = 'green',
      bins = 20,
      aes(y = after_stat(density))
    ) +
    ylim(0, 2.5) +
    geom_density(alpha = 0.5) +
    xlab("P") +
    ylab(lab) 
}

save_plot(
  annotate_figure(
    ggarrange(
      plot.p.win(0.5, first = TRUE),
      plot.p.win(1),
      plot.p.win(2),
      common.legend = TRUE,
      legend = "bottom",
      nrow = 1,
      ncol = 3
    ),
    text_grob("Prior probability of winning density",
              size = 22)
  ),
  charts$prior_motivation,
  width = 16,
  height = 8
)

# group splits simulation
sim_prob_win <- function (split_at, method = 'between') {
  top = 1:split_at
  bottom = (split_at + 1):K
  
  if (method == 'top') {
    group_1 = sample(top, size = N, replace = TRUE)
    group_2 = sample(top, size = N, replace = TRUE)
  } else if (method == 'between') {
    group_1 = sample(top, size = N, replace = TRUE)
    group_2 = sample(bottom, size = N, replace = TRUE)
  } else if (method == 'bottom') {
    group_1 = sample(bottom, size = N, replace = TRUE)
    group_2 = sample(bottom, size = N, replace = TRUE)
  }
  
  p <-
    apply(as.matrix(abilities_by_rank[, group_1] - abilities_by_rank[, group_2]),
          2,
          inv.logit)
  
  apply(p, 1, mean)
}

sim_prob_win_multi <- function(split_ats = (1:9 / 10), method='between') {
  res <- NULL
  for (at in split_ats) {
    p <- sim_prob_win(as.integer(at * K), method=method)
    sims <- tibble(at=as.integer(at * 100), p=p)
    if (is.null(res)) {
      res = sims
    }
    else {
      res = union_all(res, sims)
    }
  }
  res
}

lims <- ylim(0.3, 0.85)

plot <- ggarrange(
  ggplot(sim_prob_win_multi(method = 'top'), aes(x = factor(at), y = p)) +
    geom_violin(alpha = 0.5, fill = 'magenta') +
    geom_boxplot(alpha = 0.5, fill = 'green') +
    xlab("Split at, %") +
    ylab("P of winning") + lims,
  ggplot(sim_prob_win_multi(method = 'between'), aes(x = factor(at), y = p)) +
    geom_violin(alpha = 0.5, fill = 'magenta') +
    geom_boxplot(alpha = 0.5, fill = 'green') +
    
    xlab("Split at, %") + 
    ylab("") + lims,
  ggplot(sim_prob_win_multi(method = 'bottom'), aes(x = factor(at), y = p)) +
    geom_violin(alpha = 0.5, fill = 'magenta') +
    geom_boxplot(alpha = 0.5, fill = 'green') +
    xlab("Split at, %") + 
    ylab("") + lims,
  common.legend = TRUE,
  legend = "bottom",
  nrow = 1,
  ncol = 3
)

save_plot(
  annotate_figure(plot, text_grob("Posterior predictive probability", 
                                  size = 22)),
  charts$prob_winning_splits,
  width = 16,
  height = 8
)

