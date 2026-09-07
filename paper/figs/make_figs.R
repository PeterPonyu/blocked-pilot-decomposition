# Figure entry point for 003. Emits into figs/out/.
# Refuses to draw anything that is not bound in evidence/evidence_manifest.json.
#
# Side effect by design: this script also writes tex/generated_numbers.tex and the
# generated result tables. Every quantity the manuscript prints comes from here,
# so prose cannot drift away from the bytes that were hashed.
#
# The work is split so that each file has one reason to change: figs/lib holds
# reading, the obstacle wording, the per-observation table and its checks, the
# power arithmetic and formatting; figs/panels holds one figure each; this file
# holds the order they run in and the checks that must pass before any of them run.
#
# Run from the paper directory:  Rscript figs/make_figs.R

suppressPackageStartupMessages({
  library(ggplot2)
  library(jsonlite)
})

for (unit in c("rtx_theme.R", "lib/evidence.R", "lib/emit.R", "lib/locks.R",
               "lib/permissions.R", "lib/stats.R", "lib/findings.R")) {
  source(file.path("figs", unit))
}

dir.create(file.path("figs", "out"), showWarnings = FALSE, recursive = TRUE)

manifest <- load_manifest()
read_bound <- evidence_reader(manifest, find_repo_root())

locks_record <- read_bound$json("E-LOCKS")
unlock_design <- read_bound$json("E-NEXT")
blocker <- read_bound$json("E-BLOCKER")
split <- read_bound$json("E-SPLIT")
heldout <- read_bound$json("E-HELDOUT")
prevalence <- read_bound$json("E-PREVALENCE")
controls <- read_bound$json("E-CONTROLS")
retrieval_surface <- read_bound$json("E-RETRIEVAL")
majority_surface <- read_bound$json("E-MAJORITY")
harness <- read_bound$json("E-HARNESS")

## ---------------------------------------------------------------------------
## The three quantities everything else is built from.
## ---------------------------------------------------------------------------

N_TEST <- prevalence$n            # rows in the frozen test split
N_COHORT <- controls$n            # rows in the pre-registered held-out subsample
MIN_PRESENT <- 5                  # the gate the analysis code applies, fixed before the runs
ALPHA <- 0.05

## ---------------------------------------------------------------------------
## The access obstacles, taken from the record and worded here.
## ---------------------------------------------------------------------------

lock_rows <- locks_record$rows
locks <- build_locks(lock_rows)
lock_kinds <- lock_kind_of(locks)

# The permissions are drawn from a second record, so the two have to agree about
# how many there are. Without this the obstacle figure could count three
# permissions while the matrix drew two, and both would look internally
# consistent.
permissions <- build_permissions(unlock_design)
if (nrow(permissions) != sum(lock_kinds == "Permission")) {
  stop("the unlock record and the obstacle record disagree on how many permissions there are")
}

# The route out, and the property the manuscript reads off it: exactly one step
# needs a person, and it is the first one.
unlock_steps <- build_unlock_path(unlock_design)
if (sum(unlock_steps$human_only) != 1L || !unlock_steps$human_only[1]) {
  stop("the recorded route no longer has a single human-only step at its head")
}

## ---------------------------------------------------------------------------
## One row per observation, then every check the manuscript's claims rest on.
## ---------------------------------------------------------------------------

findings <- names(prevalence$prevalence)
per_finding <- build_per_finding(findings, prevalence, controls)
assert_arms_share_draw(per_finding, findings, controls)
per_finding <- attach_predictions(per_finding, findings, controls)
assert_majority_is_constant(per_finding, N_COHORT)

## The label-state composition is derived from the same bound prevalence table
## as every other finding-level quantity.  Build it once, then assert that each
## finding accounts for exactly one state per frozen-test report before any
## panel can draw it.  In particular, `blank` is not silently folded into
## `absent`.
LABEL_STATE_ORDER <- c("present", "absent", "uncertain", "blank")
LABEL_STATE_LABELS <- c(
  present = "Positive",
  absent = "Negative",
  uncertain = "Uncertain",
  blank = "Not mentioned (not negative)"
)
label_states <- do.call(rbind, lapply(findings, function(f) {
  gold <- prevalence$prevalence[[f]]
  counts <- vapply(LABEL_STATE_ORDER, function(state) {
    value <- gold[[state]]
    if (length(value) != 1L || is.na(value) || !is.numeric(value) ||
        value < 0 || value != floor(value)) {
      stop("invalid ", state, " count for finding ", f)
    }
    as.numeric(value)
  }, numeric(1), USE.NAMES = FALSE)
  data.frame(finding = f, state = LABEL_STATE_ORDER, count = counts,
             fraction = counts / N_TEST, stringsAsFactors = FALSE)
}))

if (nrow(label_states) != length(findings) * length(LABEL_STATE_ORDER) ||
    anyDuplicated(paste(label_states$finding, label_states$state, sep = "\u001f"))) {
  stop("label-state table must have one row per finding and state")
}
state_totals <- aggregate(count ~ finding, data = label_states, FUN = sum)
state_totals <- state_totals[match(findings, state_totals$finding), , drop = FALSE]
if (nrow(state_totals) != length(findings) || any(state_totals$count != N_TEST)) {
  stop("label-state counts do not sum to the frozen test size for every finding")
}
present_totals <- label_states[label_states$state == "present", , drop = FALSE]
present_totals <- present_totals[match(findings, present_totals$finding), , drop = FALSE]
recorded_rates <- vapply(findings, function(f) {
  as.numeric(prevalence$prevalence[[f]]$present_rate)
}, numeric(1), USE.NAMES = FALSE)
if (nrow(present_totals) != length(findings) ||
    any(abs(present_totals$fraction - recorded_rates) > 1e-12)) {
  stop("present-rate fields disagree with the bound label-state counts")
}

per_finding$n_required <- rows_required(per_finding$present_split, N_TEST, MIN_PRESENT)
per_finding$exceeds_split <- per_finding$n_required > N_TEST

per_finding$expected <- N_COHORT * per_finding$present_split / N_TEST
per_finding$p_repr <- mapply(hyper_two_sided, per_finding$n_present,
                             per_finding$present_split, N_TEST, N_COHORT)
assert_thin_list_matches(per_finding, controls$majority$e_k_lt1)

BONFERRONI <- bonferroni(ALPHA, nrow(per_finding))
per_finding$unrepresentative <- per_finding$p_repr < BONFERRONI

scored <- per_finding[per_finding$maj_status == "scored", ]
if (nrow(scored) == 0) stop("no finding cleared the presence gate; the pilot has nothing to report")
assert_gate_is(per_finding, MIN_PRESENT)

## ---------------------------------------------------------------------------
## Figures. Each panel reads the objects above and writes one file.
## ---------------------------------------------------------------------------

for (unit in c("fig1_access_locks.R", "fig2_permissions.R", "fig3_required_rows.R",
               "fig4_draw_vs_split.R", "fig5_control_assertions.R",
               "fig6_scored_cells.R", "fig7_label_states.R")) {
  source(file.path("figs", "panels", unit))
}

## ---------------------------------------------------------------------------
## Numbers and tables. Emitted, never retyped.
## ---------------------------------------------------------------------------

worst <- per_finding[which.min(per_finding$p_repr), ]
rarest <- per_finding[order(-per_finding$n_required)[1], ]
boundary <- per_finding[per_finding$n_required == N_TEST, ]

# The one finding the constant majority report is labelled positive for.
maj_asserted <- per_finding$finding[per_finding$maj_pred > 0]
if (length(maj_asserted) != 1L) {
  stop("the constant majority report is labelled positive for ", length(maj_asserted),
       " findings; the manuscript describes exactly one")
}

write_generated(c(
  macro("NLocks", nrow(lock_rows)),
  macro("NPermissionLocks", sum(lock_kinds == "Permission")),
  macro("NPermissionPairs", nrow(permissions) * (nrow(permissions) - 1L)),
  macro("NUnlockSteps", nrow(unlock_steps)),
  macro("NMachineSteps", sum(!unlock_steps$human_only)),
  macro("NArtifactLocks", sum(lock_kinds == "Artifact")),
  macro("NSubstitutionLocks", sum(lock_kinds == "Substitution refused")),
  macro("NFindings", nrow(per_finding)),
  macro("NTest", N_TEST),
  macro("NCohort", N_COHORT),
  macro("CoveragePct", fmt(100 * N_COHORT / N_TEST, 1)),
  macro("MinPresent", MIN_PRESENT),
  macro("NScored", nrow(scored)),
  macro("NUnderpowered", sum(per_finding$maj_status == "underpowered")),
  macro("NScoredExpected", sum(per_finding$expected >= MIN_PRESENT)),
  macro("NThin", sum(per_finding$expected < 1)),
  macro("NZeroDrawn", sum(per_finding$n_present == 0)),
  macro("NExceedSplit", sum(per_finding$exceeds_split)),
  macro("NBoundary", nrow(boundary)),
  macro("BoundaryFinding", boundary$finding[1]),
  macro("RarestFinding", rarest$finding),
  macro("RarestRequired", format(rarest$n_required, big.mark = ",")),
  macro("RarestPresent", rarest$present_split),
  macro("NTrain", split$splits$train |> length()),
  macro("NVal", split$splits$val |> length()),
  macro("NPatients", format(split$n_patients, big.mark = ",")),
  macro("NImages", format(split$n_images, big.mark = ",")),
  macro("NOutsideInventory", format(split$png_not_in_split_inventory, big.mark = ",")),
  macro("HarnessImages", harness$n_images),
  macro("Bonferroni", fmt(BONFERRONI, 4)),
  macro("NUnrepresentative", sum(per_finding$unrepresentative)),
  macro("SkewFinding", worst$finding),
  macro("SkewObserved", worst$n_present),
  macro("SkewExpected", fmt(worst$expected, 1)),
  macro("SkewSplitPresent", worst$present_split),
  macro("SkewP", sci(worst$p_repr)),
  macro("ScoredOne", scored$finding[1]),
  macro("ScoredTwo", scored$finding[2]),
  macro("ScoredOnePresent", scored$n_present[1]),
  macro("ScoredTwoPresent", scored$n_present[2]),
  macro("ScoredOneExpected", fmt(scored$expected[1], 1)),
  macro("ScoredTwoExpected", fmt(scored$expected[2], 1)),
  macro("ScoredTwoSplit", scored$present_split[2]),
  macro("MajTemplate", controls$majority_findings),
  macro("MajAsserted", maj_asserted),
  macro("MajAssertedSplit", per_finding$present_split[per_finding$finding == maj_asserted]),
  macro("MajAssertedDrawn", per_finding$n_present[per_finding$finding == maj_asserted]),
  macro("RetAssertedFindings", sum(per_finding$ret_pred > 0)),
  macro("RetAssertedOne", per_finding$ret_pred[per_finding$finding == scored$finding[1]]),
  macro("RetAssertedTwo", per_finding$ret_pred[per_finding$finding == scored$finding[2]]),
  macro("MajMissOne", fmt(scored$maj_rate[1])),
  macro("MajMissTwo", fmt(scored$maj_rate[2])),
  macro("RetMissOne", fmt(scored$ret_rate[1])),
  macro("RetMissTwo", fmt(scored$ret_rate[2])),
  macro("RetMissedOne", scored$ret_miss[1]),
  macro("RetMissedTwo", scored$ret_miss[2]),
  macro("RetUpperOne", fmt(scored$ret_upper[1])),
  macro("RetUpperTwo", fmt(scored$ret_upper[2])),
  macro("MaxBlank", max(per_finding$blank_split)),
  macro("MaxBlankFinding", per_finding$finding[which.max(per_finding$blank_split)]),
  macro("MedianBlankPct", fmt(100 * median(per_finding$blank_split) / N_TEST, 0)),
  macro("SurfaceRetrievalF", fmt(retrieval_surface$metrics$token_f1_findings, 3)),
  macro("SurfaceMajorityF", fmt(majority_surface$metrics$majority_token_f1_findings, 3)),
  macro("SurfaceRatio", fmt(retrieval_surface$metrics$token_f1_findings /
                              majority_surface$metrics$majority_token_f1_findings, 2)),
  macro("SurfaceRetrievalExact", fmt(100 * retrieval_surface$metrics$exact_match_findings, 1)),
  macro("SurfaceMajorityExact", fmt(100 * majority_surface$metrics$majority_exact_match_findings, 1)),
  macro("BlockerDate", substr(blocker$written_utc, 1, 10)),
  macro("SelectionSalt", heldout$selection_salt),
  macro("NEvidence", nrow(manifest$entries)),
  macro("EvidenceBytes", format(sum(manifest$entries$bytes), big.mark = ","))
), "generated_numbers.tex")

## Every observation in the schema, with what it would take to score it.

write_generated(c(
  # Seven columns at the default column padding come out just over the text
  # width. The padding is tightened rather than the table being scaled down:
  # a \resizebox would shrink this table's type below every other table's.
  "\\begingroup",
  "\\setlength{\\tabcolsep}{5pt}",
  "\\begin{tabular}{lrrrrrl}",
  "\\toprule",
  paste("Observation & Positive & Unmentioned & Rows needed & Drawn & Expected & Status \\\\"),
  paste("& in split & in split & for the gate & & & \\\\"),
  "\\midrule",
  paste0(
    short_name(per_finding$finding), " & ",
    per_finding$present_split, " & ",
    per_finding$blank_split, " & ",
    ifelse(is.finite(per_finding$n_required),
           ifelse(per_finding$exceeds_split,
                  paste0("\\textbf{", format(per_finding$n_required, big.mark = ","), "}"),
                  format(per_finding$n_required, big.mark = ",")),
           "---"), " & ",
    per_finding$n_present, " & ",
    fmt(per_finding$expected, 1), " & ",
    ifelse(per_finding$maj_status == "scored", "scored", "underpowered"),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\endgroup"
), "generated_table_power.tex")

## The recorded route from the present state to a result.

write_generated(c(
  "\\begingroup",
  "\\setlength{\\tabcolsep}{4pt}",
  # Keep every text column fixed-width so the long actor label cannot force an
  # overfull table.  The widths leave room for the step column and intercolumn
  # padding while retaining readable, unscaled type.
  "\\begin{tabular}{@{}r>{\\raggedright\\arraybackslash}p{0.24\\linewidth}>{\\raggedright\\arraybackslash}p{0.18\\linewidth}>{\\raggedright\\arraybackslash}p{0.48\\linewidth}@{}}",
  "\\toprule",
  "Step & Action & Who acts & What it waits on \\\\",
  "\\midrule",
  paste0(
    unlock_steps$step, " & ",
    unlock_steps$action, " & ",
    unlock_steps$actor, " & ",
    unlock_steps$waits_on, " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\endgroup"
), "generated_table_unlock.tex")

## The manifest itself, so the evidence discipline can be checked rather than believed.

write_generated(evidence_table(manifest), "generated_table_evidence.tex")

## The cells that cleared the gate, one block per observation.

write_generated(c(
  "\\begin{tabular}{llrrrr}",
  "\\toprule",
  "Observation & Control & Gold positives & Missed & Miss rate & 90\\% upper \\\\",
  "\\midrule",
  unlist(lapply(seq_len(nrow(scored)), function(i) c(
    paste0(short_name(scored$finding[i]), " & Majority template & ", scored$n_present[i], " & ",
           scored$maj_miss[i], " & ", fmt(scored$maj_rate[i]), " & ", fmt(scored$maj_upper[i]), " \\\\"),
    paste0(" & Indication retrieval & ", scored$n_present[i], " & ",
           scored$ret_miss[i], " & ", fmt(scored$ret_rate[i]), " & ", fmt(scored$ret_upper[i]), " \\\\")
  ))),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_scored.tex")

message(sprintf("wrote 7 figures to figs/out and 5 generated tex files to tex/ (%d of %d observations scored)",
                nrow(scored), nrow(per_finding)))
