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
battery <- read_bound$json("E-BATTERY")
controls_333 <- read_bound$json("E-CONTROLS-333")
gold_ablation <- read_bound$json("E-GOLD-ABLATION")
audit_receipt <- read_bound$json("E-AUDIT-RECEIPT")

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
## Labeller battery, full-split controls, gold-placeholder ablation.
## Every printed quantity below is stop()'d against a named field.
## ---------------------------------------------------------------------------

as_chr <- function(x) {
  if (is.null(x) || length(x) == 0) character(0) else as.character(unlist(x, use.names = FALSE))
}

chex_word <- function(code) {
  code <- as.integer(code)
  if (identical(code, 1L)) return("positive")
  if (identical(code, 0L)) return("negative")
  if (identical(code, -1L)) return("uncertain")
  if (identical(code, 2L)) return("blank")
  stop("unrecognised CheXbert code ", code)
}

finding_node <- function(per_finding, name) {
  node <- per_finding[[name]]
  if (is.null(node) && is.data.frame(per_finding) && name %in% rownames(per_finding)) {
    node <- as.list(per_finding[name, , drop = TRUE])
  }
  if (is.null(node)) stop("per_finding is missing ", name)
  node
}

field_int <- function(node, field, owner) {
  value <- node[[field]]
  if (length(value) != 1L || is.na(as.integer(value))) {
    stop(owner, " field ", field, " is missing or not a single integer")
  }
  as.integer(value)
}

if (is.null(audit_receipt$outputs[["battery.json"]]) ||
    is.null(audit_receipt$outputs[["controls_333.json"]]) ||
    is.null(audit_receipt$outputs[["gold_placeholder_ablation.json"]])) {
  stop("RECEIPT.json outputs is missing battery.json, controls_333.json, or gold_placeholder_ablation.json")
}

if (!identical(as.integer(battery$n_strings), 74L)) {
  stop("n_strings is ", battery$n_strings, "; expected 74")
}

ablation <- battery$summary$template_ablation
raw_pos <- as_chr(ablation$template_raw$flags$positives)
if (!identical(raw_pos, "Cardiomegaly")) {
  stop("template_raw positives are [", paste(raw_pos, collapse = ", "),
       "]; expected [Cardiomegaly]")
}
if ("No Finding" %in% raw_pos || isTRUE(ablation$template_raw$flags$no_finding)) {
  stop("template_raw No Finding is asserted; it must not be")
}

minus_pos <- as_chr(ablation$template_minus_first_sentence$flags$positives)
if (!identical(minus_pos, "No Finding")) {
  stop("template_minus_first_sentence positives are [", paste(minus_pos, collapse = ", "),
       "]; expected [No Finding]")
}

ph_flags <- ablation$template_placeholder_removed$flags
if (!isTRUE(ph_flags$asserts_cardiomegaly) || !("Cardiomegaly" %in% as_chr(ph_flags$positives))) {
  stop("template_placeholder_removed is not still Cardiomegaly positive")
}
if (!identical(as.logical(ablation$removing_placeholder_flips_template_to_no_finding), FALSE)) {
  stop("removing_placeholder_flips_template_to_no_finding is ",
       ablation$removing_placeholder_flips_template_to_no_finding, "; expected false")
}

plausible <- c("remained stable", "not changed", "no change", "been stable", "[MASK]")
repl <- ablation$first_sentence_replacements
for (word in plausible) {
  cardio <- repl[[word]]$labels[["Cardiomegaly"]]
  if (!identical(as.integer(cardio), -1L)) {
    stop("first_sentence_replacements '", word, "' Cardiomegaly is ",
         cardio, "; expected -1")
  }
}

if (!identical(as.integer(controls_333$gold$n_scorable), 9L)) {
  stop("n_scorable is ", controls_333$gold$n_scorable, "; expected 9")
}
if (!identical(as.integer(controls_333$presence_gate), 5L)) {
  stop("presence_gate is ", controls_333$presence_gate, "; expected 5")
}
if (!identical(as.integer(controls_333$n), 333L)) {
  stop("controls_333 n is ", controls_333$n, "; expected 333")
}
if (!isTRUE(controls_333$reproduction_check_32$reproduces_iu_chexbert_control_baselines_exactly)) {
  stop("reproduction_check_32.reproduces_iu_chexbert_control_baselines_exactly is not true")
}

maj_cardio <- finding_node(controls_333$majority$per_finding, "Cardiomegaly")
if (!identical(field_int(maj_cardio, "n_hit", "majority Cardiomegaly n_hit"), 41L) ||
    !identical(field_int(maj_cardio, "n_present", "majority Cardiomegaly n_present"), 41L)) {
  stop("majority Cardiomegaly n_hit/n_present are ",
       maj_cardio$n_hit, "/", maj_cardio$n_present, "; expected 41/41")
}
if (!identical(field_int(maj_cardio, "n_pred_present", "majority Cardiomegaly n_pred_present"), 333L)) {
  stop("majority Cardiomegaly n_pred_present is ", maj_cardio$n_pred_present, "; expected 333")
}

if (!identical(as.integer(gold_ablation$n_texts_with_placeholder), 143L)) {
  stop("n_texts_with_placeholder is ", gold_ablation$n_texts_with_placeholder, "; expected 143")
}
if (!identical(as.integer(gold_ablation$summary$rows_with_any_label_change), 16L)) {
  stop("rows_with_any_label_change is ", gold_ablation$summary$rows_with_any_label_change,
       "; expected 16")
}
if (!isTRUE(gold_ablation$reproduction_check$recomputed_raw_prevalence_matches_existing_exactly)) {
  stop("recomputed_raw_prevalence_matches_existing_exactly is not true")
}

gold_pf <- gold_ablation$per_finding
if (is.data.frame(gold_pf) && "delta_present" %in% names(gold_pf)) {
  max_abs_delta <- max(abs(as.numeric(gold_pf$delta_present)))
  lo_any <- as.integer(gold_pf[["Lung Opacity", "any_state_change"]])
} else {
  max_abs_delta <- max(vapply(gold_pf, function(node) abs(as.numeric(node$delta_present)), numeric(1)))
  lo_any <- as.integer(gold_pf[["Lung Opacity"]]$any_state_change)
}
if (!identical(as.integer(max_abs_delta), 1L)) {
  stop("max |delta_present| over findings is ", max_abs_delta, "; expected 1")
}
if (!identical(lo_any, 6L)) {
  stop("per_finding Lung Opacity any_state_change is ", lo_any, "; expected 6")
}

plausible_one <- repl[["remained stable"]]

## ---------------------------------------------------------------------------
## Figures. Each panel reads the objects above and writes one file.
## ---------------------------------------------------------------------------

for (unit in c("fig0_three_reasons.R", "fig1_access_locks.R", "fig2_permissions.R",
               "fig3_required_rows.R", "fig4_draw_vs_split.R",
               "fig5_control_assertions.R", "fig6_scored_cells.R",
               "fig7_label_states.R", "fig8_prevalence_sampling_ceiling.R")) {
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
  macro("EvidenceBytes", format(sum(manifest$entries$bytes), big.mark = ",")),
  macro("BatteryRawPositive", "Cardiomegaly"),
  macro("BatteryMinusFirst", "No Finding"),
  macro("BatteryPlaceholderFlips", "does not"),
  macro("BatteryPlausibleUncertain", "five"),
  macro("BatteryN", as.integer(battery$n_strings)),
  macro("FullSplitScorable", as.integer(controls_333$gold$n_scorable)),
  macro("FullSplitN", as.integer(controls_333$n)),
  macro("FullSplitReproduces", "reproduces"),
  macro("FullSplitMajCardioHit", field_int(maj_cardio, "n_hit", "majority Cardiomegaly n_hit")),
  macro("FullSplitMajCardioPred", field_int(maj_cardio, "n_pred_present", "majority Cardiomegaly n_pred_present")),
  macro("GoldPlaceholderTexts", as.integer(gold_ablation$n_texts_with_placeholder)),
  macro("GoldRowsChanged", as.integer(gold_ablation$summary$rows_with_any_label_change)),
  macro("GoldMaxDeltaPresent", as.integer(max_abs_delta)),
  macro("GoldLungOpacityAnyState", lo_any)
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

battery_rows <- list(
  list(name = "template, as written", node = ablation$template_raw),
  list(name = "placeholder token removed", node = ablation$template_placeholder_removed),
  list(name = "first sentence deleted", node = ablation$template_minus_first_sentence),
  list(name = "first sentence alone", node = ablation$first_sentence_alone),
  list(name = "plausible English (remained stable)", node = plausible_one)
)
write_generated(c(
  "\\begin{tabular}{lll}",
  "\\toprule",
  "String variant & Cardiomegaly & No Finding \\\\",
  "\\midrule",
  vapply(battery_rows, function(row) {
    paste0(row$name, " & ",
           chex_word(row$node$labels[["Cardiomegaly"]]), " & ",
           chex_word(row$node$labels[["No Finding"]]), " \\\\")
  }, character(1)),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_battery.tex")

message(sprintf("wrote 9 figures to figs/out and 6 generated tex files to tex/ (%d of %d observations scored)",
                nrow(scored), nrow(per_finding)))
