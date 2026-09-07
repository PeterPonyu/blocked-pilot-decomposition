# The three permissions, and what each one does and does not deliver.
#
# Same contract as lib/locks.R: the *set* of permissions and the relation
# between them come from the bound record, and only the wording lives here. The
# recorded text names hosts, courses and internal flags, none of which belong in
# a manuscript; if the recorded set ever stops matching the set described here
# the build stops, so the figure cannot describe a régime that is no longer the
# recorded one.
#
# The manuscript already states this claim in one sentence. The figure exists
# because the sentence is the kind that reads as obvious and is planned against
# incorrectly anyway: three approvals, each of which delivers exactly one of the
# three things needed, and none of which is progress toward the other two.

# Keyed by the identifier the record uses. `holds` is the short label for the
# permission itself; `delivers` names the one thing it grants, in the same
# vocabulary the body uses.
PERMISSION_TEXT <- list(
  "lock_1_hf_contact" = list(
    holds = "Access request\ngranted by the\nbenchmark maintainers",
    delivers = "The file defining\nwhich cases the\nbenchmark scores"),
  "lock_2_citi" = list(
    holds = "Human-subjects\nresearch training,\nin a named person",
    delivers = "The credential later\nagreements require\nof that person"),
  "lock_3_physionet_dua" = list(
    holds = "Data use agreement\nfor the underlying\nimages",
    delivers = "The radiographs\nthe benchmark\nscores")
)

# The order the figure reads in: the order a project meets them is not fixed by
# anything, so the record's own order is used rather than inventing a sequence
# the evidence does not claim.
build_permissions <- function(unlock_record) {
  locks <- unlock_record$three_locks_not_interchangeable$locks
  if (!setequal(names(PERMISSION_TEXT), locks$id)) {
    stop("the recorded set of permissions differs from the set this figure describes")
  }
  # The claim being drawn is that the relation is exactly the diagonal. That is
  # asserted against the record rather than assumed: each permission must name
  # the other two among the things it does not deliver, which is what the
  # record's own non-interchangeability rule says.
  for (i in seq_len(nrow(locks))) {
    others <- setdiff(locks$id, locks$id[i])
    text <- tolower(locks$what_it_does_not_unlock[i])
    named <- vapply(others, function(other) {
      key <- switch(other,
                    lock_1_hf_contact = "contact gate",
                    lock_2_citi = "citi",
                    lock_3_physionet_dua = "dua")
      grepl(key, text, fixed = TRUE)
    }, logical(1))
    if (!any(named)) {
      stop("a recorded permission does not exclude either of the other two: ", locks$id[i])
    }
  }
  data.frame(
    id = locks$id,
    holds = vapply(locks$id, function(k) PERMISSION_TEXT[[k]]$holds, character(1), USE.NAMES = FALSE),
    delivers = vapply(locks$id, function(k) PERMISSION_TEXT[[k]]$delivers, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

# The recorded route from the present state to a result, worded for a reader.
#
# Same contract again: the record fixes how many steps there are, their order,
# and which of them a machine can take unattended; this table fixes only the
# words. The recorded gates name hosts, scripts and internal artifacts, so they
# are restated as the condition each step waits on rather than quoted.
#
# What the table is for: the manuscript claims the blockage is administrative,
# and the shape of this route is the evidence for the claim. Only the first step
# needs a person, every later step is mechanical, and none of the later steps can
# start until the first one finishes.
UNLOCK_TEXT <- list(
  "0" = list(
    action = "Obtain the three approvals",
    actor = "A person",
    waits_on = "Nothing. No compute is provisioned and no gated file is fetched before the approvals are held, which is why the cost of being blocked here is zero."),
  "1" = list(
    action = "Fetch the file defining which cases are scored",
    actor = "Authorized runner",
    waits_on = "The access request has been granted, and retrieval occurs in an authorized environment."),
  "2" = list(
    action = "Check the file before spending compute",
    actor = "Authorized runner",
    waits_on = "The file parses, matches the task's schema, and holds the published number of cases. A file that merely resembles the right one stops here."),
  "3" = list(
    action = "Obtain the benchmark-listed images",
    actor = "A person, then authorized runner",
    waits_on = "Each image source's own agreement is held. Which agreements apply is decided by the benchmark's case list, not by the project."),
  "4" = list(
    action = "Generate a report for every case",
    actor = "Authorized runner",
    waits_on = "The three preceding steps have passed and the model is available in the authorized environment."),
  "5" = list(
    action = "Score the reports with the benchmark's own labeller",
    actor = "Authorized runner",
    waits_on = "The generated reports exist, and the scoring uses the benchmark's labeller rather than an approximation of it."),
  "6" = list(
    action = "Copy the scores back",
    actor = "Authorized runner",
    waits_on = "The scores were verified where they were computed. This step moves evidence and computes nothing."),
  "7" = list(
    action = "Record the result",
    actor = "Authorized runner",
    waits_on = "The scoring wrapper is given the benchmark's real split size, and its refusal of undersized samples stays active."),
  "8" = list(
    action = "Close the question",
    actor = "The project",
    waits_on = "The recorded result is committed and reviewed independently of the person who produced it.")
)

build_unlock_path <- function(unlock_record) {
  steps <- unlock_record$unlock_path
  if (!setequal(names(UNLOCK_TEXT), as.character(steps$step))) {
    stop("the recorded unlock route differs from the route this table describes")
  }
  keys <- as.character(steps$step)
  data.frame(
    step = steps$step,
    action = vapply(keys, function(k) UNLOCK_TEXT[[k]]$action, character(1), USE.NAMES = FALSE),
    actor = vapply(keys, function(k) UNLOCK_TEXT[[k]]$actor, character(1), USE.NAMES = FALSE),
    waits_on = vapply(keys, function(k) UNLOCK_TEXT[[k]]$waits_on, character(1), USE.NAMES = FALSE),
    human_only = grepl("human only", steps$actor, fixed = TRUE),
    stringsAsFactors = FALSE
  )
}

# The long form of the claim, as a grid: one row per permission held, one column
# per thing needed, and a mark only where holding the row delivers the column.
permission_grid <- function(permissions) {
  n <- nrow(permissions)
  grid <- expand.grid(row = seq_len(n), col = seq_len(n))
  grid$delivered <- grid$row == grid$col
  grid$holds <- permissions$holds[grid$row]
  grid$delivers <- permissions$delivers[grid$col]
  grid
}
