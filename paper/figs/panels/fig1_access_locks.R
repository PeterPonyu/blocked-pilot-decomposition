# Figure 1. What each access requirement asks for, and whether more data or more
# compute would satisfy it. The colour is the category rather than the severity:
# the paper's point is that these are three different kinds of obstacle, and
# that a project reporting one verdict for all of them hides which one stopped it.

limits <- locks_limits(locks)

p <- ggplot(locks, aes(x = 0, y = y)) +
  geom_tile(aes(fill = kind, height = height), width = 1.96,
            colour = "grey40", linewidth = 0.3) +
  # Both blocks hang from the top edge of their own tile, and the tile was sized
  # to hold them, so neither the title nor a longer detail can reach the border.
  geom_text(aes(label = title, y = top - LOCK_PAD_UNITS), x = -0.95,
            hjust = 0, vjust = 1, size = 2.9, fontface = "bold") +
  geom_text(aes(label = detail, y = top - LOCK_PAD_UNITS - LOCK_TITLE_UNITS),
            x = -0.95, hjust = 0, vjust = 1,
            size = 2.3, lineheight = 1.05, colour = "grey20") +
  scale_fill_manual(values = c(
    "Permission: a person or an institution must act" = "#FBE3E4",
    "Artifact: a file absent from the retained workspace" = "#E8E4F3",
    "Substitution refused: the local corpus is a different thing" = "#EAEAEA"
  ), name = NULL) +
  scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = limits, expand = c(0, 0)) +
  guides(fill = guide_legend(ncol = 1)) +
  rtx_theme() +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.border = element_blank(), legend.position = "bottom",
        legend.text = element_text(size = FIGURE_LEGEND_TEXT_SIZE),
        legend.key.size = unit(0.32, "cm"),
        legend.margin = margin(t = -2))

# The height follows the stack rather than being chosen for it: the tiles were
# already sized to their own text, so the canvas is that total plus the room the
# bottom legend takes.
save_fig(p, "fig1_access_locks", FIGURE_TEXT_WIDTH_IN,
         diff(limits) * LOCK_LINE_IN + 0.62)
