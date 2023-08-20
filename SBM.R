library(tidyverse)
library(Rcpp)
library(boot)

source('Config.R')
source('PlotConfig.R')

# load adjacency matrix containing number of victories of player-i over player-j
data <- read_csv(files$sbm_out)

# plot heat map for the adjacency matrix for the first N player
N <- 20
res <- list(
  x = rep(0, N * N),
  y = rep(0, N * N),
  wins = rep(0, N * N)
)

for (i in 1:N) {
  for (j in 1:N) {
    k = 1 + ((i-1)*N+(j-1))
    res$y[k] = i
    res$x[k] = j
    res$wins[k] = as.numeric(data[i,j])
  }
}

longer.data <- tibble(x = factor(res$x), y = factor(res$y), Wins = res$wins)

save_plot(
  ggplot(longer.data, aes(x, y, fill = Wins)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = colors$color_2) +
    scale_x_discrete("Player j") +
    scale_y_discrete("Player i", limits = rev(levels(longer.data$y))) +
    ggtitle(paste("GSBM input data (Y), first", N, "players")),
  charts$sbm_input
)


# infer the SBM using Metropolis-Hastings impleentation in C++
sourceCpp('cpp/main.cpp')
results <-
  runMetropolisHastings(
    as.matrix(data),
    numCommunities = 2,
    numIterations = 500,
    burnIn = 100
  )

dim(results$z)

# fix group assignments to use R-style 1,2,... indices
z <- t(results$z) + 1

plot(colMeans(z))

# top-right triangle of Pi matrix
dim(results$pi)

pi <- results$pi

# gather Bradley-Terry ranking and assigned clusters into a data frame
ranking_bradley_terry <- read_csv(files$ranking_bradley_terry)
dt <- ranking_bradley_terry %>% arrange(player)

argmax.freq <- function(v) {
  as.numeric(names(which.max(table(v))))
}
dt$cluster <- apply(z, 2, argmax.freq)
dt <- arrange(dt, rank)
dt

# plot density of the posterior Pi[1,2] 
save_plot(
  ggplot(tibble(p = t(pi)), aes(x = p)) +
    geom_histogram(aes(y = ..density..),
      alpha = colors$alpha,
      bins = 10,
      fill = colors$color_1,
    ) +
    geom_density(alpha = colors$alpha) +
    xlab("Posterior Pi[1,2]") +
    ylab("Density") +
    ggtitle("Posterior Pi Density"),
  file = charts$pi_posterior_distribution
)

# how Bradley-Terry abilities correlate with found SBM clusters?
save_plot(
  ggplot(dt, aes(x = rank, y = factor(cluster), color=factor(cluster))) +
    geom_point(alpha = 0.5, show.legend = FALSE) +
    xlab("Bradley-Terry rank") +
    ylab("GSBM cluster") +
    ggtitle("Cluster assignments"),
  charts$rank_vs_cluster
)

cor(dt$rank, dt$cluster)

# is Bradley-Terry rank a good predictor of strongest cluster
model <- glm((cluster - 1) ~ rank, data = dt, family = binomial)
summary(model)

# abilities vs cluster
save_plot(
  ggplot(dt, aes(x = factor(cluster), y = alpha_mean)) +
    geom_violin(alpha = colors$alpha, fill = colors$color_2) +
    geom_boxplot(alpha = colors$alpha, fill = colors$color_1) +
    xlab("GSBM cluster") +
    ylab("Expected ability") +
    ggtitle("Expected ability by cluster"),
  charts$ability_vs_cluster
)

# estimated cluster sizes
sizes_1 <- apply(z, 1, function(v) sum(v == 1))
sizes_2 <- apply(z, 1, function(v) sum(v == 2))

sizes <- as.data.frame(table(dt$cluster))
sizes <- cbind(sizes, c(sd(sizes_1), sd(sizes_2)))
colnames(sizes) = c("cluster", "size", "sd")
sizes

save_plot(
  ggplot(as.tibble(sizes), aes(x=cluster, y=size, fill=cluster)) + 
    geom_col(alpha=0.8, show.legend = FALSE) +
    geom_errorbar(aes(ymin = size - sd, ymax = size + sd, width = 0.2)) +
    xlab("GSBM cluster") +
    ylab("# of players") +
    guides(fill=guide_legend(title="GSBM cluster")) +
    ggtitle("Cluster sizes"),
  charts$cluster_sizes
)

# alternative ranking based on the probability of the strongest cluster
cluster_1_prob <- function(sample) {
  mean(sample == 1)
}

dt$cluster_1_prob <- apply(z[,dt$player], 2, cluster_1_prob)
dt$cluster_1_prob_sd <- apply(z[,dt$player], 2, cluster_1_prob_sd)

save_plot(
  ggplot(dt, aes(x = rank, y = cluster_1_prob)) +
    geom_point(alpha = colors$alpha, color = colors$color_2) +
    xlab("Bradley- Terry rank") +
    ylab("P") +
    ggtitle("Probability of the strongest cluster"),
  charts$rank_vs_cluster_1_prob
)

# store strong cluster probability-based rankings
sbm_ranking <- dt %>% arrange(desc(cluster_1_prob)) %>% select(player, cluster_1_prob)
sbm_ranking$rank <- 1:nrow(sbm_ranking)
sbm_ranking %>% write_csv(files$ranking_p_strong_cluster)

