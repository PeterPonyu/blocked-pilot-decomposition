# The arithmetic behind the power ceiling and the representativeness test.
#
# Neither is a simulation. Both are functions of counts that were fixed before
# any report was scored, which is why this paper can state a ceiling without
# having been granted access to anything.

# Is the pre-registered draw representative of the split it came from? Exact
# two-sided hypergeometric, by summing the probability of every outcome no more
# likely than the one observed. The tolerance is there because two outcomes that
# are equally likely in exact arithmetic can differ in the last bit.
hyper_two_sided <- function(k, K, N, n) {
  support <- max(0, n - (N - K)):min(K, n)
  probs <- dhyper(support, K, N - K, n)
  observed <- dhyper(k, K, N - K, n)
  min(1, sum(probs[probs <= observed * (1 + 1e-9)]))
}

# Rows needed before the split's own prevalence puts `min_present` positives in
# a draw, on average. A finding the split holds no positives for cannot reach
# the gate at any draw size, which is reported as infinite rather than as a very
# large number, because the two mean different things.
rows_required <- function(present_split, n_test, min_present) {
  required <- ceiling(min_present * n_test / pmax(present_split, 1))
  required[present_split == 0] <- Inf
  required
}

bonferroni <- function(alpha, comparisons) alpha / comparisons
