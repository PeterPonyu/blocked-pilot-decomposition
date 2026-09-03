# Figure 3. What the pre-registered subsample drew against what the split
# predicts it should have drawn. The segment is the gap between the two, and the
# gate line is what a draw has to clear before either control arm is scored on
# it at all: most observations sit below it whatever the draw happened to do.

f3 <- per_finding[order(-per_finding$present_split), ]
f3$label <- factor(short_name(f3$finding), levels = short_name(f3$finding))
f3$flag <- ifelse(f3$unrepresentative, "Draw differs from the split",
                  "Consistent with the split")

p <- ggplot(f3, aes(x = label)) +
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
            aes(y = n_present, label = sprintf("p = %s", sci(p_repr))),
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

save_fig(p, "fig4_draw_vs_split", FIGURE_TEXT_WIDTH_IN, 3.3)
