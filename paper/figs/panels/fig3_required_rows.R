# Figure 2. How many reports each observation needs before it can be scored at
# all, against how many the split and the subsample actually hold. The two
# reference lines are the ceiling: a bar above the upper one is an observation
# the whole corpus cannot power, whatever model is evaluated on it.

f2 <- per_finding[order(per_finding$n_required, per_finding$finding), ]
f2$label <- factor(short_name(f2$finding), levels = short_name(f2$finding))

# An observation with no positives in the split needs infinitely many rows. The
# bar is drawn at a finite height so the axis stays readable, and labelled with
# the reason so the height is not mistaken for an estimate.
f2$plot_required <- pmin(f2$n_required, N_TEST * 6)
f2$capped <- is.infinite(f2$n_required)

p <- ggplot(f2, aes(x = label)) +
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

save_fig(p, "fig3_required_rows", FIGURE_TEXT_WIDTH_IN, 3.2)
