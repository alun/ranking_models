#include <Rcpp.h>

#include <cassert>
#include <random>

using namespace std;
using namespace Rcpp;

#include "truncated_normal.hpp"

static const long unsigned int seed = 777;
static default_random_engine gen{seed};

// Generates random numbers between 0 and 1
double randomUniform() {
  // static random_device rd;
  // static mt19937 gen(rd());
  // static default_random_engine gen{seed};
  uniform_real_distribution<> dis(0.0, 1.0);
  return dis(gen);
}

// Generates random integers between a and b
int randomInt(int a, int b) {
  // static random_device rd;
  // static mt19937 gen(rd());
  // static default_random_engine gen{seed};
  uniform_int_distribution<> dis(a, b);
  return dis(gen);
}

// Calculates binomial coefficient
double n_choose_k(const double n, const double k) {
  double result = 1;
  for (int i = 1; i <= k; i++) {
    result *= (n - (k - i)) / i;
  }
  return result;
}

// helper function to model top-triangle of the Pi matrix - see indices.ipynb

int topTriangleSize(int n) { return n * (n - 1) / 2; }

int flatIndex(int i, int j, int n) {
  assert(("Only top triangle allowed", j > i));
  return (i * (n - 1 + (n - 1 - (i - 1))) / 2 + (j - i)) - 1;
}

void unFlatIndex(int k, int n, int& i, int& j) {
  assert(("Incorrect flat index", k < topTriangleSize(n)));
  i = 0;
  j = n - 1;
  while (k >= j) {
    k -= j;
    i += 1;
    j -= 1;
  }
  j = i + 1 + k;
}

double probWinning(const NumericVector& pi, int block_i, int block_j) {
  double probWinning = 0.5;

  // we only model the top triangle in pi
  int numBlocks = (1 + sqrt(1 + 8 * pi.size())) / 2;

  if (block_i != block_j) {
    int block_row = min(block_i, block_j);
    int block_col = max(block_i, block_j);

    probWinning = pi(flatIndex(block_row, block_col, numBlocks));
  }

  if (block_i > block_j) {
    // bottom triangle is symmetric
    probWinning = 1 - probWinning;
  }

  return probWinning;
}

// Calculates likelihood delta when moving single node to a new block (faster
// than full likelihood)
double logLikelihoodDelta(const IntegerMatrix& y, const IntegerVector& z,
                          const NumericVector& pi, int node, int blockTo) {
  double result = 0.0;
  int numNodes = y.rows();

  for (int i = 0; i < numNodes; i++) {
    if (i == node) continue;

    int numWins = y(node, i);
    int numLoses = y(i, node);

    int numGames = numWins + numLoses;
    if (numGames == 0) {
      continue;
    }

    double pOld = probWinning(pi, z(node), z(i));
    double pNew = probWinning(pi, blockTo, z(i));
    // binomial likelihood delta
    result += numWins * (log(pNew) - log(pOld)) +
              numLoses * (log(1 - pNew) - log(1 - pOld));
  }

  return result;
}

// Function to calculate the log-likelihood of the given graph under the
// Stochastic Block Model
// [[Rcpp::export]]
double logLikelihood(const IntegerMatrix& y, const IntegerVector& z,
                     const NumericVector& pi) {
  double result = 0.0;
  int numNodes = y.rows();
  int numCommunities = (1 + sqrt(1 + 8 * pi.size())) / 2;

  for (int i = 0; i < numNodes; i++) {
    for (int j = 0; j < numNodes; j++) {
      int numWins = y(i, j);
      int numLoses = y(j, i);
      int numGames = numWins + numLoses;
      if (numGames == 0) {
        continue;
      }

      double p = probWinning(pi, z(i), z(j));

      // binomial likelihood
      result += log(n_choose_k(numGames, numWins)) + numWins * log(p) +
                numLoses * log(1 - p);
    }
  }

  return result;
}

// Function to perform Metropolis-Hastings sampling for the Stochastic Block
// Model
// [[Rcpp::export]]
List runMetropolisHastings(const IntegerMatrix& y, int numCommunities,
                           int numIterations = 10000, int burnIn = 2000,
                           double stddevPi = 0.05) {
  if (y.rows() != y.cols()) {
    throw std::range_error("Expected a square outcomes matrix");
  }

  if (burnIn >= numIterations) {
    throw std::range_error(
        "Burn in can't be larger then the number of iterations");
  }

  int numNodes = y.rows();

  // Initialize the community assignments randomly
  IntegerVector z(numNodes);
  for (int i = 0; i < numNodes; i++) {
    // TODO try out Rcpp sugar for RNG
    // https://dirk.eddelbuettel.com/code/rcpp/Rcpp-quickref.pdf
    z[i] = randomInt(0, numCommunities - 1);
  }

  // Initialize the community winning probabilities randomly
  // We model only the top right probabilities
  // Bottom-left are inverse symmetric
  // Diagonal is 0.5 as we want maximum uncertainty for the comarison outcome
  // within the same group

  NumericVector pi(topTriangleSize(numCommunities));
  for (int k = 0; k < pi.size(); k++) {
    pi(k) = randomUniform();
  }

  int trueIterations = numIterations - burnIn;
  IntegerVector zSample(trueIterations * z.size());
  NumericVector piSample(trueIterations * pi.size());

  int truncated_norm_seed = 1;
  double currentLogLikelihood, newLogLikelihood;

  // Perform Metropolis-Hastings (Metropolis within Gibbs) sampling
  for (int iter = 0; iter < numIterations; iter++) {
    Rcout << "Iteration: " << iter << std::endl;

    // Try updating node assignments
    for (int node = 0; node < numNodes; node++) {
      // Calculate the current log-likelihood
      currentLogLikelihood = logLikelihood(y, z, pi);

      // Generate a new community assignment for the selected node
      int newBlock = randomInt(0, numCommunities - 1);

      // Calculate the new log-likelihood delta
      double delta = logLikelihoodDelta(y, z, pi, node, newBlock);

      // Accept or reject the new assignment based on the acceptance ratio
      if (delta > log(randomUniform())) {
        // Accept the new assignment
        currentLogLikelihood += delta;
        z[node] = newBlock;
      }
    }

    // Generate a random update to Pi
    for (int k = 0; k < pi.size(); k++) {
      double currentProb = pi[k];
      pi[k] = truncated_normal_ab_sample(pi[k], stddevPi, 0, 1,
                                         truncated_norm_seed);

      // Calculate the new log-likelihood
      newLogLikelihood = logLikelihood(y, z, pi);

      // Accept or reject the new assignment based on the acceptance ratio
      if (newLogLikelihood - currentLogLikelihood > log(randomUniform())) {
        // Accept the new assignment
        currentLogLikelihood = newLogLikelihood;
      } else {
        // Revert to the previous state
        pi[k] = currentProb;
      }
    }

    if (iter >= burnIn) {
      int afterBurnIter = iter - burnIn;

      copy(z.begin(), z.end(), zSample.begin() + afterBurnIter * z.size());
      copy(pi.begin(), pi.end(), piSample.begin() + afterBurnIter * pi.size());
    }

    try {
      Rcpp::checkUserInterrupt();
    } catch (Rcpp::internal::InterruptedException& e) {
      Rcout << "Caught an interrupt!" << std::endl;
      break;
    }
  }

  zSample.attr("dim") = Dimension(z.size(), trueIterations);
  piSample.attr("dim") = Dimension(pi.size(), trueIterations);

  return List::create(Named("z") = zSample, Named("lastZ") = z,
                      Named("pi") = piSample, Named("lastPi") = pi);
}