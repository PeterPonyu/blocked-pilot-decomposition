# Figure entry point for 003. Emits into figs/out/.
# Refuses to draw anything that is not bound in evidence/evidence_manifest.json.
#
# Side effect by design: this script also writes tex/generated_numbers.tex and the
# generated result tables. Every quantity the manuscript prints comes from here,
# so prose cannot drift away from the bytes that were hashed.
#
# Run from the paper directory:  Rscript figs/make_figs.R

suppressPackageStartupMessages({
  library(ggplot2)
  library(jsonlite)
})

source(file.path("figs", "rtx_theme.R"))

out_dir <- file.path("figs", "out")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

find_repo_root <- function(start = normalizePath(".")) {
  cur <- start
  repeat {
    if (file.exists(file.path(cur, ".git"))) return(cur)
    nxt <- dirname(cur)
    if (identical(nxt, cur)) stop("no repository root above ", start)
    cur <- nxt
  }
}

repo_root <- find_repo_root()

manifest_path <- file.path("evidence", "evidence_manifest.json")
manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = TRUE)

if (!identical(manifest$state, "BOUND") || length(manifest$entries) == 0) {
  stop("evidence manifest is not BOUND; bind the evidence before drawing anything.")
}

# Fail closed: a figure may only read bytes whose hash still matches the binding.
bound_path <- function(id) {
  row <- manifest$entries[manifest$entries$id == id, ]
  if (nrow(row) != 1L) stop("no unique evidence entry bound under id ", id)
  path <- file.path(repo_root, row$path[[1]])
  if (!file.exists(path)) stop("bound evidence has disappeared: ", id)
  actual <- digest::digest(path, algo = "sha256", file = TRUE)
  if (!identical(actual, row$sha256[[1]])) stop("bound evidence drifted on disk: ", id)
  path
}

read_bound <- function(id) jsonlite::fromJSON(bound_path(id), simplifyVector = TRUE)

locks <- read_bound("E-LOCKS")
blocker <- read_bound("E-BLOCKER")
split <- read_bound("E-SPLIT")
heldout <- read_bound("E-HELDOUT")
prevalence <- read_bound("E-PREVALENCE")
controls <- read_bound("E-CONTROLS")
retrieval_surface <- read_bound("E-RETRIEVAL")
majority_surface <- read_bound("E-MAJORITY")
harness <- read_bound("E-HARNESS")

## ---------------------------------------------------------------------------
## The three quantities everything else is built from.
## ---------------------------------------------------------------------------

N_TEST <- prevalence$n            # rows in the frozen test split
N_COHORT <- controls$n            # rows in the pre-registered held-out subsample
MIN_PRESENT <- 5                  # the gate the analysis code applies, fixed before the runs

findings <- names(prevalence$prevalence)
# Axis and table labels only. The manuscript prints the schema's own label text,
# so the two never disagree about which observation is being named.
short_name <- function(x) sub("Enlarged Cardiomediastinum", "Enl. Cardiomediastinum", x)

# One row per finding: what the split contains, what the subsample drew, and
# what each control arm did with it.
per_finding <- do.call(rbind, lapply(findings, function(f) {
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

retrieval_present <- vapply(findings, function(f) controls$retrieval$per_finding[[f]]$n_present,
                            numeric(1), USE.NAMES = FALSE)
if (!identical(as.integer(per_finding$n_present), as.integer(retrieval_present))) {
  stop("the two control arms disagree about how many gold positives the subsample holds")
}

# How many of the subsample's rows each arm labelled positive for each finding.
per_finding$maj_pred <- vapply(findings, function(f) controls$majority$per_finding[[f]]$n_pred_present,
                               numeric(1), USE.NAMES = FALSE)
per_finding$ret_pred <- vapply(findings, function(f) controls$retrieval$per_finding[[f]]$n_pred_present,
                               numeric(1), USE.NAMES = FALSE)

# The majority arm emits one constant report for every row, so each of its
# per-finding counts must be either none of the rows or all of them. If that
# ever fails, the arm is not what this manuscript describes and the build stops
# rather than reporting a label vector that was never a single string's.
if (!all(per_finding$maj_pred %in% c(0, N_COHORT))) {
  stop("the majority arm's predictions vary across rows; it is not a constant report")
}

# Rows needed before the split's own prevalence puts MIN_PRESENT positives in a
# draw, on average. This is arithmetic on the bound counts, not a simulation.
per_finding$n_required <- ceiling(MIN_PRESENT * N_TEST / pmax(per_finding$present_split, 1))
per_finding$n_required[per_finding$present_split == 0] <- Inf
per_finding$exceeds_split <- per_finding$n_required > N_TEST

# Is the pre-registered draw representative of the split it came from? Exact
# two-sided hypergeometric, by summing the probability of every outcome no more
# likely than the one observed.
hyper_two_sided <- function(k, K, N, n) {
  support <- max(0, n - (N - K)):min(K, n)
  probs <- dhyper(support, K, N - K, n)
  observed <- dhyper(k, K, N - K, n)
  min(1, sum(probs[probs <= observed * (1 + 1e-9)]))
}
per_finding$expected <- N_COHORT * per_finding$present_split / N_TEST
per_finding$p_repr <- mapply(hyper_two_sided, per_finding$n_present,
                             per_finding$present_split, N_TEST, N_COHORT)

# The project recorded which findings it expected fewer than one positive for.
# Recomputing that list from the split's own counts is a check on both: if the
# recorded list and the arithmetic disagree, one of them is stale.
recomputed_thin <- sort(per_finding$finding[per_finding$expected < 1])
if (!identical(recomputed_thin, sort(controls$majority$e_k_lt1))) {
  stop("the recorded list of findings expected below one positive does not match the split counts")
}

BONFERRONI <- 0.05 / nrow(per_finding)
per_finding$unrepresentative <- per_finding$p_repr < BONFERRONI

scored <- per_finding[per_finding$maj_status == "scored", ]
if (nrow(scored) == 0) stop("no finding cleared the presence gate; the pilot has nothing to report")

# MIN_PRESENT is stated above rather than read from a field. Recovering it from
# the recorded statuses turns that statement into a checked one: if the gate the
# runs actually applied were anything other than MIN_PRESENT, these two lines
# would disagree and the build would stop.
gate_implied <- ifelse(per_finding$n_present >= MIN_PRESENT, "scored", "underpowered")
if (!identical(gate_implied, per_finding$maj_status) ||
    !identical(gate_implied, per_finding$ret_status)) {
  stop("the recorded per-finding status does not match a presence gate of ", MIN_PRESENT)
}

save_fig <- function(plot, name, width, height) {
  ggplot2::ggsave(file.path(out_dir, paste0(name, ".pdf")), plot,
                  width = width, height = height, units = "in", device = cairo_pdf)
  invisible(NULL)
}

## ---------------------------------------------------------------------------
## Figure 1 -- what each access requirement asks for, and whether more data or
## more compute would satisfy it.
## ---------------------------------------------------------------------------

lock_rows <- locks$rows

# The recorded facts name internal scripts and flags, which do not belong in a
# manuscript. The set of obstacles is therefore taken from the evidence and the
# wording from this table; if the two ever disagree the build stops, so the
# figure cannot describe a set of obstacles that is no longer the recorded one.
LOCK_TEXT <- list(
  "HF contact" = list(
    kind = "Permission: a person or an institution must act",
    title = "Access request to the benchmark host",
    detail = "The benchmark repository is contact-gated. Access is granted by its maintainers on request; no amount of local computation substitutes for the grant."),
  "CITI" = list(
    kind = "Permission: a person or an institution must act",
    title = "Human-subjects research training",
    detail = "A recognised research-ethics training certificate, issued to a named person. It is a separate requirement from the access request and does not imply it."),
  "PhysioNet MIMIC-CXR DUA" = list(
    kind = "Permission: a person or an institution must act",
    title = "Data use agreement for the underlying images",
    detail = "The images the benchmark scores are governed by their own credentialed data use agreement, which is a further step after access to the benchmark itself."),
  "data.json absent" = list(
    kind = "Artifact: a file that is not on this machine",
    title = "The evaluation split file",
    detail = "The file that defines which cases the benchmark scores was not present at any of the three locations checked. It comes with the access grant above rather than being a further permission, so it is a second thing to hold, not a second door."),
  "IU is not CheXbench" = list(
    kind = "Substitution refused: the local corpus is a different thing",
    title = "The locally held corpus is not the benchmark",
    detail = "A different public chest-radiograph corpus is available here. It is not the benchmark's data, and reporting a score on it under the benchmark's name would be a category error."),
  "wrap refuses n<=8" = list(
    kind = "Substitution refused: the local corpus is a different thing",
    title = "The scoring wrapper rejects the local sample size",
    detail = "The scoring code refuses any sample that is not the benchmark's own split size, so the eight-image local run cannot be dressed as a benchmark result even by accident.")
)

if (!setequal(names(LOCK_TEXT), lock_rows$lock)) {
  stop("the recorded set of obstacles differs from the set this figure describes")
}

f1 <- data.frame(
  lock = lock_rows$lock,
  y = rev(seq_len(nrow(lock_rows))),
  stringsAsFactors = FALSE
)
KIND_ORDER <- c("Permission: a person or an institution must act",
                "Artifact: a file that is not on this machine",
                "Substitution refused: the local corpus is a different thing")
f1$kind <- factor(vapply(f1$lock, function(k) LOCK_TEXT[[k]]$kind, character(1), USE.NAMES = FALSE),
                  levels = KIND_ORDER)
if (anyNA(f1$kind)) stop("an obstacle carries a category this figure does not order")
f1$title <- vapply(f1$lock, function(k) LOCK_TEXT[[k]]$title, character(1), USE.NAMES = FALSE)

wrap_to <- function(x, width) vapply(x, function(s) paste(strwrap(s, width), collapse = "\n"),
                                     character(1), USE.NAMES = FALSE)
f1$detail <- wrap_to(vapply(f1$lock, function(k) LOCK_TEXT[[k]]$detail, character(1),
                            USE.NAMES = FALSE), 74)

fig1 <- ggplot(f1, aes(x = 0, y = y)) +
  geom_tile(aes(fill = kind), width = 1.96, height = 0.9, colour = "grey40", linewidth = 0.3) +
  geom_text(aes(label = title), x = -0.95, hjust = 0, vjust = -0.55,
            size = 2.9, fontface = "bold") +
  geom_text(aes(label = detail), x = -0.95, hjust = 0, vjust = 0.85,
            size = 2.3, lineheight = 0.95, colour = "grey20") +
  scale_fill_manual(values = c(
    "Permission: a person or an institution must act" = "#FBE3E4",
    "Artifact: a file that is not on this machine" = "#E8E4F3",
    "Substitution refused: the local corpus is a different thing" = "#EAEAEA"
  ), name = NULL) +
  scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.4, nrow(f1) + 0.6), expand = c(0, 0)) +
  guides(fill = guide_legend(ncol = 1)) +
  rtx_theme() +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.border = element_blank(), legend.position = "bottom",
        legend.text = element_text(size = 7), legend.key.size = unit(0.32, "cm"),
        legend.margin = margin(t = -2))

save_fig(fig1, "fig1_access_locks", 6.1, 4.4)

## ---------------------------------------------------------------------------
## Figure 2 -- how many rows each finding needs before it can be scored at all,
## against how many the split and the subsample actually hold.
## ---------------------------------------------------------------------------

f2 <- per_finding[order(per_finding$n_required, per_finding$finding), ]
f2$label <- factor(short_name(f2$finding), levels = short_name(f2$finding))
f2$plot_required <- pmin(f2$n_required, N_TEST * 6)
f2$capped <- is.infinite(f2$n_required)

fig2 <- ggplot(f2, aes(x = label)) +
  geom_col(aes(y = plot_required, fill = exceeds_split), width = 0.68) +
  geom_hline(yintercept = N_TEST, linetype = "22", linewidth = 0.4, colour = "grey20") +
  geom_hline(yintercept = N_COHORT, linetype = "42", linewidth = 0.4, colour = "#B2182B") +
  annotate("text", x = 0.7, y = N_TEST * 1.28, hjust = 0, size = 2.5, colour = "grey20",
           label = sprintf("all %d reports in the frozen test split", N_TEST)) +
  annotate("label", x = 0.7, y = N_COHORT * 0.6, hjust = 0, size = 2.5, colour = "#B2182B",
           fill = "white", linewidth = 0, label.padding = unit(0.06, "lines"),
           label = sprintf("the %d reports actually labelled", N_COHORT)) +
  geom_text(data = f2[f2$capped, ], aes(y = plot_required, label = "no positives in the split"),
            hjust = 1.03, vjust = 0.5, angle = 90, size = 2.2, colour = "white") +
  scale_fill_manual(values = c(`FALSE` = "#4D7EA8", `TRUE` = "#B2182B"), guide = "none") +
  scale_y_log10(name = sprintf("Reports needed for %d expected positives", MIN_PRESENT),
                breaks = c(10, 30, 100, 333, 1000, 2000),
                labels = c("10", "30", "100", "333", "1000", "2000")) +
  scale_x_discrete(name = NULL) +
  rtx_theme() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7))

save_fig(fig2, "fig2_required_rows", 6.1, 3.2)

## ---------------------------------------------------------------------------
## Figure 3 -- what the subsample drew against what the split predicts.
## ---------------------------------------------------------------------------

f3 <- per_finding[order(-per_finding$present_split), ]
f3$label <- factor(short_name(f3$finding), levels = short_name(f3$finding))
f3$flag <- ifelse(f3$unrepresentative, "Draw differs from the split",
                  "Consistent with the split")

fig3 <- ggplot(f3, aes(x = label)) +
  geom_segment(aes(xend = label, y = expected, yend = n_present),
               colour = "grey55", linewidth = 0.35) +
  geom_point(aes(y = expected), shape = 21, size = 1.9, fill = "white",
             colour = "grey30", stroke = 0.4) +
  geom_point(aes(y = n_present, colour = flag), size = 2.1) +
  geom_hline(yintercept = MIN_PRESENT - 0.5, linetype = "22", linewidth = 0.4,
             colour = "#1B7837") +
  annotate("text", x = nrow(f3) - 0.2, y = MIN_PRESENT + 0.9, hjust = 1, size = 2.5,
           colour = "#1B7837",
           label = sprintf("gate: %d gold positives", MIN_PRESENT)) +
  geom_text(data = f3[f3$unrepresentative, ],
            aes(y = n_present, label = sprintf("p = %s", format(signif(p_repr, 2), scientific = TRUE))),
            hjust = -0.15, size = 2.4, colour = "#B2182B") +
  scale_colour_manual(values = c("Consistent with the split" = "#4D7EA8",
                                 "Draw differs from the split" = "#B2182B"), name = NULL) +
  scale_y_continuous(name = "Gold positives in the draw",
                     limits = c(-0.4, max(f3$n_present, f3$expected) + 2.6),
                     breaks = seq(0, 14, 2)) +
  scale_x_discrete(name = NULL, expand = expansion(add = 0.7)) +
  rtx_theme() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7),
        legend.position = "bottom", legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"))

save_fig(fig3, "fig3_draw_vs_split", 6.1, 3.3)

## ---------------------------------------------------------------------------
## Figure 4 -- what the two controls actually assert, across all findings, next
## to what the draw actually contains.
## ---------------------------------------------------------------------------

f4pred <- rbind(
  data.frame(finding = per_finding$finding, series = "Gold positives in the draw",
             count = per_finding$n_present, stringsAsFactors = FALSE),
  data.frame(finding = per_finding$finding, series = "Asserted by the majority control",
             count = per_finding$maj_pred, stringsAsFactors = FALSE),
  data.frame(finding = per_finding$finding, series = "Asserted by the retrieval control",
             count = per_finding$ret_pred, stringsAsFactors = FALSE)
)
pred_order <- per_finding$finding[order(-per_finding$n_present, -per_finding$ret_pred)]
f4pred$label <- factor(short_name(f4pred$finding), levels = short_name(pred_order))
f4pred$series <- factor(f4pred$series, levels = c("Gold positives in the draw",
                                                  "Asserted by the majority control",
                                                  "Asserted by the retrieval control"))

fig4 <- ggplot(f4pred, aes(x = label, y = count, fill = series)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72) +
  geom_hline(yintercept = N_COHORT, linetype = "22", linewidth = 0.4, colour = "grey30") +
  annotate("text", x = nrow(per_finding) - 0.1, y = N_COHORT - 1.6, hjust = 1, size = 2.5,
           colour = "grey30",
           label = sprintf("all %d labelled reports", N_COHORT)) +
  scale_fill_manual(values = c("Gold positives in the draw" = "grey55",
                               "Asserted by the majority control" = "#B2182B",
                               "Asserted by the retrieval control" = "#4D7EA8"),
                    name = NULL) +
  scale_y_continuous(name = "Reports labelled positive", limits = c(0, N_COHORT + 1.2),
                     breaks = seq(0, 32, 8), expand = c(0, 0)) +
  scale_x_discrete(name = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  rtx_theme() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7),
        legend.position = "bottom", legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"), legend.margin = margin(t = -2))

save_fig(fig4, "fig4_control_assertions", 6.1, 3.4)

## ---------------------------------------------------------------------------
## Figure 5 -- the two cells that clear the gate, and what the controls did.
## ---------------------------------------------------------------------------

f5 <- rbind(
  data.frame(finding = scored$finding, arm = "Majority template",
             n_present = scored$n_present, miss = scored$maj_miss,
             rate = scored$maj_rate, upper = scored$maj_upper, stringsAsFactors = FALSE),
  data.frame(finding = scored$finding, arm = "Indication retrieval",
             n_present = scored$n_present, miss = scored$ret_miss,
             rate = scored$ret_rate, upper = scored$ret_upper, stringsAsFactors = FALSE)
)
f5$panel <- sprintf("%s (%d gold positives)", short_name(f5$finding), f5$n_present)

fig5 <- ggplot(f5, aes(x = arm, y = rate, colour = arm)) +
  geom_linerange(aes(ymin = rate, ymax = upper), linewidth = 0.5) +
  geom_point(size = 2.4) +
  geom_text(aes(y = upper, label = sprintf("90%% upper %.2f", upper)),
            vjust = -0.9, size = 2.4, colour = "grey25") +
  geom_text(aes(label = sprintf("%d of %d missed", miss, n_present)),
            vjust = 2.1, size = 2.4, colour = "grey25") +
  facet_wrap(~panel) +
  scale_colour_manual(values = c("Majority template" = "#B2182B",
                                 "Indication retrieval" = "#4D7EA8"), guide = "none") +
  scale_y_continuous(name = "Share of gold positives the control missed",
                     limits = c(0.74, 1.10), breaks = seq(0.75, 1.0, 0.05)) +
  scale_x_discrete(name = NULL) +
  rtx_theme() +
  theme(axis.text.x = element_text(size = 7.5))

save_fig(fig5, "fig5_scored_cells", 5.7, 2.9)

## ---------------------------------------------------------------------------
## Generated LaTeX.
## ---------------------------------------------------------------------------

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
sci <- function(p) format(signif(p, 2), scientific = TRUE)

worst <- per_finding[which.min(per_finding$p_repr), ]
rarest <- per_finding[order(-per_finding$n_required)[1], ]
boundary <- per_finding[per_finding$n_required == N_TEST, ]

# The one finding the constant majority report is labelled positive for.
maj_asserted <- per_finding$finding[per_finding$maj_pred > 0]
if (length(maj_asserted) != 1L) {
  stop("the constant majority report is labelled positive for ", length(maj_asserted),
       " findings; the manuscript describes exactly one")
}

lock_kinds <- sub(":.*$", "", as.character(f1$kind))

macro <- function(name, value) sprintf("\\newcommand{\\%s}{%s}", name, value)

numbers <- c(
  "% Generated by the figure script from hash-bound evidence. Do not hand-edit.",
  macro("NLocks", nrow(lock_rows)),
  macro("NPermissionLocks", sum(lock_kinds == "Permission")),
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
  macro("SelectionSalt", heldout$selection_salt)
)

writeLines(numbers, file.path("tex", "generated_numbers.tex"))

power_tab <- c(
  "% Generated by the figure script from hash-bound evidence. Do not hand-edit.",
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
  "\\end{tabular}"
)
writeLines(power_tab, file.path("tex", "generated_table_power.tex"))

scored_tab <- c(
  "% Generated by the figure script from hash-bound evidence. Do not hand-edit.",
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
)
writeLines(scored_tab, file.path("tex", "generated_table_scored.tex"))

message(sprintf("wrote 5 figures to %s and 3 generated tex files to tex/", out_dir))
