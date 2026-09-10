# Figure 7. The four states emitted by CheXbert for every finding in the
# frozen test split.  This is a composition of label outputs, not a clinical
# prevalence plot: `blank` means the report did not mention the finding and is
# deliberately kept separate from an explicit negative label.

state_order <- LABEL_STATE_ORDER
state_labels <- LABEL_STATE_LABELS

# Put the rarest positive states at the bottom of the horizontal display and
# the most common ones at the top.  The ordering is deterministic and uses the
# schema name as a tie-breaker, so a rebuild cannot silently reshuffle rows.
present_counts <- tapply(label_states$count[label_states$state == "present"],
                         label_states$finding[label_states$state == "present"],
                         identity)
finding_order <- findings[order(as.numeric(present_counts[findings]), findings)]
label_states$finding <- factor(label_states$finding, levels = finding_order)
label_states$state <- factor(label_states$state, levels = state_order)

present_rows <- label_states[label_states$state == "present", ]
present_rows$label <- sprintf("%d positive (%.1f%%)", present_rows$count,
                              100 * present_rows$fraction)

# The stack starts with the positive state at zero, so the positive share is read
# straight off the axis; the counts are printed beyond the 100 per cent edge in
# the outer margin, so the panel itself ends where the partition does.
p <- ggplot(label_states, aes(x = finding, y = fraction, fill = state)) +
  geom_col(width = 0.74, colour = "white", linewidth = FIGURE_HAIRLINE_WIDTH,
           position = position_stack(reverse = TRUE)) +
  geom_text(data = present_rows,
            aes(x = finding, y = 1.012, label = label),
            inherit.aes = FALSE, hjust = 0, size = FIGURE_ANNOTATION_SIZE,
            colour = "grey15") +
  # The window is set on the coord, not the scale, so the labels just past the
  # 100 per cent edge are drawn in the margin rather than dropped as out of range.
  coord_flip(ylim = c(0, 1), clip = "off") +
  scale_y_continuous(name = "Share of frozen-test reports",
                     breaks = c(0, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0%", "25%", "50%", "75%", "100%"),
                     expand = c(0, 0)) +
  scale_x_discrete(name = NULL, labels = short_name) +
  scale_fill_manual(values = c(
    present = PAL$blue,
    absent = "#92C5DE",
    uncertain = "#F4A582",
    blank = "#D9D9D9"
  ), breaks = state_order, labels = unname(state_labels), name = NULL) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  rtx_theme() +
  theme(axis.text.y = element_text(size = FIGURE_AXIS_TEXT_SIZE),
        legend.position = "bottom",
        plot.margin = margin(t = 4, r = 78, b = 4, l = 4))

save_fig(p, "fig7_label_states", FIGURE_TEXT_WIDTH_IN, 3.8)
