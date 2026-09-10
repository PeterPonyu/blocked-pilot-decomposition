# Figure 3. How many reports each observation needs before it can be scored at
# all, against how many the split and the subsample actually hold. The two
# reference lines are the ceiling: a bar above the upper one is an observation
# the whole corpus cannot power, whatever model is evaluated on it.

f2 <- per_finding[order(per_finding$n_required, per_finding$finding), ]
# The axis prints the schema's own finding names (shortened only where the
# generated tables shorten them too), in this panel's sorted order.
f2$label <- factor(short_name(f2$finding), levels = short_name(f2$finding))
if (anyNA(f2$label) || anyDuplicated(f2$label)) {
  stop("fig3 finding axis labels are not a one-to-one mapping")
}

# An observation with no positives in the split needs infinitely many rows. The
# bar is drawn at a finite height so the axis stays readable, and labelled with
# the reason so the height is not mistaken for an estimate.
f2$plot_required <- pmin(f2$n_required, N_TEST * 6)
f2$capped <- is.infinite(f2$n_required)
f2$reach <- factor(ifelse(f2$exceeds_split,
                          "Needs more than the whole test split",
                          "Reachable within the test split"),
                   levels = c("Reachable within the test split",
                              "Needs more than the whole test split"))

Y_FLOOR <- 6
Y_CEILING <- N_TEST * 8

p <- ggplot(f2, aes(x = label)) +
  geom_col(aes(y = plot_required, fill = reach), width = 0.68) +
  geom_hline(yintercept = N_TEST, linetype = "22", linewidth = FIGURE_RULE_WIDTH,
             colour = "grey20") +
  geom_hline(yintercept = N_COHORT, linetype = "42", linewidth = FIGURE_RULE_WIDTH,
             colour = "grey20") +
  # Both reference lines are named where the bars are shortest, so the label
  # never sits on a bar.
  annotate("text", x = 0.62, y = N_TEST * 1.45, hjust = 0, vjust = 0,
           size = FIGURE_ANNOTATION_SIZE, colour = "grey20",
           label = sprintf("whole test split: %d reports", N_TEST)) +
  annotate("text", x = 0.62, y = N_COHORT * 1.45, hjust = 0, vjust = 0,
           size = FIGURE_ANNOTATION_SIZE, colour = "grey20",
           label = sprintf("labelled draw: %d reports", N_COHORT)) +
  geom_text(data = f2[f2$capped, ], aes(y = plot_required, label = "no positives in the split"),
            hjust = 0.5, vjust = 1.4, size = FIGURE_ANNOTATION_SIZE, colour = "white") +
  scale_fill_manual(values = c("Reachable within the test split" = PAL$blue,
                               "Needs more than the whole test split" = PAL$red),
                    name = NULL) +
  scale_y_log10(name = sprintf("Reports needed for %d expected positives", MIN_PRESENT),
                breaks = c(10, 30, 100, 333, 1000, 2000),
                labels = c("10", "30", "100", "333", "1000", "2000"),
                expand = c(0, 0)) +
  # Bars on a log axis run to minus infinity; the panel floor clips them, so the
  # window has to be set on the coord and the clipping left on.
  coord_cartesian(ylim = c(Y_FLOOR, Y_CEILING)) +
  scale_x_discrete(name = NULL) +
  rtx_theme() +
  theme(axis.text.x = rotated_axis_text(),
        legend.position = "bottom")

save_fig(p, "fig3_required_rows", FIGURE_TEXT_WIDTH_IN, 3.3)
