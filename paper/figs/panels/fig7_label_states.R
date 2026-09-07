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
present_rows$label <- sprintf("+%d (%.1f%%)", present_rows$count,
                              100 * present_rows$fraction)

p <- ggplot(label_states, aes(x = finding, y = fraction, fill = state)) +
  geom_col(width = 0.74, colour = "white", linewidth = 0.16) +
  geom_text(data = present_rows,
            aes(x = finding, y = 1.012, label = label),
            inherit.aes = FALSE, hjust = 0, size = 2.25,
            colour = "grey20") +
  coord_flip(clip = "off") +
  scale_y_continuous(name = "Share of frozen-test reports",
                     limits = c(0, 1.10),
                     breaks = c(0, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0%", "25%", "50%", "75%", "100%"),
                     expand = c(0, 0)) +
  scale_x_discrete(name = NULL) +
  scale_fill_manual(values = c(
    present = "#2166AC",
    absent = "#92C5DE",
    uncertain = "#F4A582",
    blank = "#D9D9D9"
  ), breaks = state_order, labels = unname(state_labels), name = NULL) +
  labs(subtitle = sprintf("Four-state CheXbert labels across %d frozen test reports; each bar sums to 100%%",
                          N_TEST)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  rtx_theme() +
  theme(axis.text.y = element_text(size = 7.0, lineheight = 0.9),
        axis.text.x = element_text(size = 7.6),
        legend.position = "bottom", legend.text = element_text(size = 7.1),
        legend.key.size = unit(0.32, "cm"), legend.margin = margin(t = -2),
        plot.subtitle = element_text(size = 7.1, colour = "grey25"),
        plot.margin = margin(t = 13, r = 42, b = 8, l = 8))

save_fig(p, "fig7_label_states", FIGURE_TEXT_WIDTH_IN, 4.25)
