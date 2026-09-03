# Figure 2. The three permissions against the three things a project needs, with
# a mark only where holding the row delivers the column. The figure is the
# diagonal, and the diagonal is the claim: each approval delivers exactly one of
# the three, so holding one is not partial progress toward the other two. The
# empty cells carry the content here, which is why they are drawn as cells
# rather than left as whitespace.

grid <- permission_grid(permissions)

p <- ggplot(grid, aes(x = col, y = -row)) +
  geom_tile(aes(fill = delivered), colour = "grey45", linewidth = 0.3,
            width = 0.94, height = 0.94) +
  geom_text(aes(label = ifelse(delivered, "delivers", "still required")),
            family = FIGURE_FONT_FAMILY,
            colour = ifelse(grid$delivered, "grey15", "grey45"),
            fontface = ifelse(grid$delivered, "bold", "plain"),
            size = 2.6) +
  scale_fill_manual(values = c(`TRUE` = "#D8E4D0", `FALSE` = "#F4F4F4"), guide = "none") +
  scale_x_continuous(
    name = NULL, position = "top",
    breaks = seq_len(nrow(permissions)), labels = permissions$delivers,
    limits = c(0.5, nrow(permissions) + 0.5), expand = c(0, 0)) +
  scale_y_continuous(
    name = NULL, breaks = -seq_len(nrow(permissions)), labels = permissions$holds,
    limits = c(-nrow(permissions) - 0.5, -0.5), expand = c(0, 0)) +
  rtx_theme() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 7.0, lineheight = 1.05, face = "bold"),
        axis.text.y = element_text(size = 7.0, lineheight = 1.05, hjust = 0))

save_fig(p, "fig2_permissions", FIGURE_TEXT_WIDTH_IN, 2.35)
