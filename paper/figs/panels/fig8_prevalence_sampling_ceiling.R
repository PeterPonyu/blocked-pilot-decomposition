# Figure 8. Full-split prevalence against the observed pre-registered draw.
# The red bars are the cells that stay underpowered under the presence gate the
# analysis applies (MIN_PRESENT gold positives in the draw).
#
# The two quantities live on one pair of proportional axes: the left axis counts
# positives in the whole frozen split, the right axis is the same position read
# as a count in a draw of N_COHORT rows (left times N_COHORT/N_TEST).  A bar top
# read on the right axis is therefore the expected count for the draw, and the
# filled point is what the draw actually held, so the gap between them is the
# sampling departure and nothing is squashed against zero.

gold <- controls_333$gold$present_counts
obs <- controls$majority$per_finding
findings8 <- names(gold)
observed <- vapply(findings8, function(f) as.numeric(obs[[f]]$n_present), numeric(1))
if (!identical(as.integer(N_TEST), as.integer(controls_333$n)) ||
    !identical(as.integer(N_COHORT), as.integer(controls$n))) {
  stop("sampling-ceiling panel denominators disagree with the bound full-split and draw artifacts")
}
full <- as.numeric(gold)
DRAW_PER_SPLIT <- N_COHORT / N_TEST
d <- data.frame(finding = findings8,
                full = full, observed = observed, expected = full * DRAW_PER_SPLIT,
                underpowered = observed < MIN_PRESENT,
                stringsAsFactors = FALSE)
if (!identical(sort(d$finding), sort(per_finding$finding))) {
  stop("sampling-ceiling panel findings differ from the per-observation table")
}
d <- d[order(-d$full, d$finding), ]
d$label <- factor(short_name(d$finding), levels = short_name(d$finding))
d$observed_on_split_axis <- d$observed / DRAW_PER_SPLIT

GATE_LABEL <- sprintf("Underpowered in the draw (fewer than %d positives)", MIN_PRESENT)
CLEAR_LABEL <- "Clears the presence gate in the draw"
POINT_LABEL <- sprintf("Observed positives in the %d-report draw (right axis)", N_COHORT)
d$gate <- factor(ifelse(d$underpowered, GATE_LABEL, CLEAR_LABEL),
                 levels = c(CLEAR_LABEL, GATE_LABEL))

y_top <- max(d$full, d$observed_on_split_axis) * 1.08

p <- ggplot(d, aes(x = label)) +
  geom_col(aes(y = full, fill = gate), width = 0.68) +
  geom_hline(yintercept = MIN_PRESENT / DRAW_PER_SPLIT, linetype = "22",
             linewidth = FIGURE_RULE_WIDTH, colour = "grey20") +
  annotate("text", x = nrow(d) + 0.4, y = MIN_PRESENT / DRAW_PER_SPLIT, hjust = 1, vjust = -0.5,
           size = FIGURE_ANNOTATION_SIZE, colour = "grey20",
           label = sprintf("presence gate: %d positives in the draw", MIN_PRESENT)) +
  geom_point(aes(y = observed_on_split_axis, shape = POINT_LABEL), size = 2.2,
             colour = PAL$blue) +
  scale_fill_manual(values = setNames(c(PAL$light_blue, PAL$light_red),
                                      c(CLEAR_LABEL, GATE_LABEL)), name = NULL) +
  scale_shape_manual(values = setNames(16, POINT_LABEL), name = NULL) +
  scale_y_continuous(name = sprintf("Positive labels in the %d-report split", N_TEST),
                     limits = c(0, y_top), expand = c(0, 0),
                     sec.axis = sec_axis(~ . * DRAW_PER_SPLIT,
                                         name = sprintf("Positives in the %d-report draw", N_COHORT),
                                         breaks = seq(0, 12, 2))) +
  scale_x_discrete(name = NULL) +
  guides(fill = guide_legend(order = 1, nrow = 1), shape = guide_legend(order = 2, nrow = 1)) +
  rtx_theme() +
  theme(axis.text.x = rotated_axis_text(),
        legend.position = "bottom", legend.box = "vertical",
        legend.spacing.y = unit(2, "pt"))

save_fig(p, "fig8_prevalence_sampling_ceiling", FIGURE_TEXT_WIDTH_IN, 3.5)
