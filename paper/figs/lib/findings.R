# One row per observation in the schema: what the frozen split contains, what
# the pre-registered subsample drew, and what each control arm did with it.
#
# Everything the manuscript says about power is a statement about this table, so
# it is assembled once and then checked against the records it came from rather
# than being rebuilt per figure.

build_per_finding <- function(findings, prevalence, controls) {
  do.call(rbind, lapply(findings, function(f) {
    gold <- prevalence$prevalence[[f]]
    maj <- controls$majority$per_finding[[f]]
    ret <- controls$retrieval$per_finding[[f]]
    data.frame(
      finding = f,
      present_split = gold$present,
      absent_split = gold$absent,
      uncertain_split = gold$uncertain,
      blank_split = gold$blank,
      rate = gold$present_rate,
      n_present = maj$n_present,
      maj_miss = maj$n_miss,
      maj_status = maj$status,
      maj_rate = if (is.null(maj$empirical_miss_rate)) NA_real_ else maj$empirical_miss_rate,
      maj_upper = if (is.null(maj$cp_miss_upper_90)) NA_real_ else maj$cp_miss_upper_90,
      ret_miss = ret$n_miss,
      ret_status = ret$status,
      ret_rate = if (is.null(ret$empirical_miss_rate)) NA_real_ else ret$empirical_miss_rate,
      ret_upper = if (is.null(ret$cp_miss_upper_90)) NA_real_ else ret$cp_miss_upper_90,
      stringsAsFactors = FALSE
    )
  }))
}

# How many of the subsample's rows each arm labelled positive for each finding.
attach_predictions <- function(per_finding, findings, controls) {
  arm_count <- function(arm) {
    vapply(findings, function(f) controls[[arm]]$per_finding[[f]]$n_pred_present,
           numeric(1), USE.NAMES = FALSE)
  }
  per_finding$maj_pred <- arm_count("majority")
  per_finding$ret_pred <- arm_count("retrieval")
  per_finding
}

# The two arms scored the same draw, so they must agree about what was in it.
# A disagreement would mean the miss rates below are computed against different
# denominators and are not comparable.
assert_arms_share_draw <- function(per_finding, findings, controls) {
  retrieval_present <- vapply(findings,
                              function(f) controls$retrieval$per_finding[[f]]$n_present,
                              numeric(1), USE.NAMES = FALSE)
  if (!identical(as.integer(per_finding$n_present), as.integer(retrieval_present))) {
    stop("the two control arms disagree about how many gold positives the subsample holds")
  }
  invisible(TRUE)
}

# The majority arm emits one constant report for every row, so each of its
# per-finding counts must be either none of the rows or all of them. If that
# ever fails, the arm is not what this manuscript describes and the build stops
# rather than reporting a label vector that was never a single string's.
assert_majority_is_constant <- function(per_finding, n_cohort) {
  if (!all(per_finding$maj_pred %in% c(0, n_cohort))) {
    stop("the majority arm's predictions vary across rows; it is not a constant report")
  }
  invisible(TRUE)
}

# The project recorded which findings it expected fewer than one positive for.
# Recomputing that list from the split's own counts is a check on both: if the
# recorded list and the arithmetic disagree, one of them is stale.
assert_thin_list_matches <- function(per_finding, recorded_thin) {
  recomputed <- sort(per_finding$finding[per_finding$expected < 1])
  if (!identical(recomputed, sort(recorded_thin))) {
    stop("the recorded list of findings expected below one positive does not match the split counts")
  }
  invisible(TRUE)
}

# The presence gate is stated by the driver rather than read from a field.
# Recovering it from the recorded statuses turns that statement into a checked
# one: if the gate the runs actually applied were anything else, these two would
# disagree and the build would stop.
assert_gate_is <- function(per_finding, min_present) {
  implied <- ifelse(per_finding$n_present >= min_present, "scored", "underpowered")
  if (!identical(implied, per_finding$maj_status) ||
      !identical(implied, per_finding$ret_status)) {
    stop("the recorded per-finding status does not match a presence gate of ", min_present)
  }
  invisible(TRUE)
}
