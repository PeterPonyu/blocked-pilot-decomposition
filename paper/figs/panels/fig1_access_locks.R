# Figure 1. What each access requirement asks for, and whether more data or more
# compute would satisfy it. The colour is the category rather than the severity:
# the paper's point is that these are three different kinds of obstacle, and
# that a project reporting one verdict for all of them hides which one stopped it.
#
# The tile colours are the paper's shared palette, so the permission tiles here
# are the same red as the permission box in the overview figure.

limits <- locks_limits(locks)

lock_fills <- c(PAL$fill_red, PAL$fill_purple, PAL$fill_grey)
lock_borders <- c(PAL$red, PAL$purple, PAL$grey)
names(lock_fills) <- KIND_ORDER
names(lock_borders) <- KIND_ORDER

p <- ggplot(locks, aes(x = 0, y = y)) +
  geom_tile(aes(fill = kind, colour = kind, height = height), width = 1.96,
            linewidth = 0.35) +
  # Both blocks hang from the top edge of their own tile, and the tile was sized
  # to hold them, so neither the title nor a longer detail can reach the border.
  geom_text(aes(label = title, y = top - LOCK_PAD_UNITS), x = -0.95,
            hjust = 0, vjust = 1, size = 3.0, fontface = "bold") +
  geom_text(aes(label = detail, y = top - LOCK_PAD_UNITS - LOCK_TITLE_UNITS),
            x = -0.95, hjust = 0, vjust = 1,
            size = FIGURE_ANNOTATION_SIZE, lineheight = 1.05, colour = "grey15") +
  scale_fill_manual(values = lock_fills, name = NULL, drop = FALSE) +
  scale_colour_manual(values = lock_borders, name = NULL, drop = FALSE) +
  scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = limits, expand = c(0, 0)) +
  guides(fill = guide_legend(ncol = 1), colour = guide_legend(ncol = 1)) +
  rtx_theme() +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.border = element_blank(), legend.position = "bottom",
        legend.key.spacing.y = unit(1, "pt"),
        legend.margin = margin(t = 2))

# The height follows the stack rather than being chosen for it: the tiles were
# already sized to their own text, so the canvas is that total plus the room the
# bottom legend takes.
save_fig(p, "fig1_access_locks", FIGURE_TEXT_WIDTH_IN,
         diff(limits) * LOCK_LINE_IN + 0.62)
