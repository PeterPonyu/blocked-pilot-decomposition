# Figure 0 -- the three separable reasons, side by side.
#
# Every count is already computed from the bound records before this file is
# sourced. The figure does not add a scored cell, a permission, or a prevalence
# estimate. It only places the three owners on one page so the aggregate
# "the pilot failed" cannot be read as a single cause.

n_permission <- sum(lock_kinds == "Permission")
n_exceed <- sum(per_finding$exceeds_split)
n_underpowered <- sum(per_finding$maj_status == "underpowered")
maj_names <- per_finding$finding[per_finding$maj_pred > 0]
if (length(maj_names) != 1L) {
  stop("overview schematic expects exactly one majority-asserted finding")
}

boxes <- data.frame(
  id = c("permission", "prevalence", "labeller"),
  xmin = c(0.03, 0.345, 0.66),
  xmax = c(0.325, 0.64, 0.97),
  ymin = 0.22,
  ymax = 0.86,
  fill = c("#FBE3E4", "#E8F1F8", "#F8E8E8"),
  border = c("#B2182B", "#4D7EA8", "#8C4A16"),
  stringsAsFactors = FALSE
)
boxes$xmid <- (boxes$xmin + boxes$xmax) / 2

box_text <- data.frame(
  x = boxes$xmid,
  y = c(0.56, 0.56, 0.56),
  label = c(
    paste0(
      "PERMISSION\n",
      n_permission, " approvals a person\n",
      "or institution must grant.\n",
      "None is a file, a model,\n",
      "or a labelled report."
    ),
    paste0(
      "PREVALENCE\n",
      n_exceed, " of ", nrow(per_finding), " findings need\n",
      "more than the ", N_TEST, "-report\n",
      "split. ", n_underpowered, " stay underpowered\n",
      "in the ", N_COHORT, "-report draw."
    ),
    paste0(
      "LABELLER\n",
      "The constant unremarkable\n",
      "report is marked positive\n",
      "for ", maj_names, " on all\n",
      N_COHORT, " labelled rows."
    )
  ),
  stringsAsFactors = FALSE
)

fig0 <- ggplot() +
  geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = boxes$fill, colour = boxes$border, linewidth = 0.65) +
  geom_text(data = box_text, aes(x = x, y = y, label = label),
            family = FIGURE_FONT_FAMILY, size = 3.05, lineheight = 1.02,
            colour = "#202020") +
  annotate(
    "text", x = 0.5, y = 0.94,
    label = "Three owners, one vanished pilot",
    family = FIGURE_FONT_FAMILY, size = 3.4, fontface = "bold", colour = "#202020"
  ) +
  annotate(
    "label", x = 0.5, y = 0.10,
    label = "Computation removes none of these",
    family = FIGURE_FONT_FAMILY, size = 3.0, fontface = "bold",
    colour = "#4A4A4A", fill = "#F4F4F4",
    label.padding = grid::unit(0.18, "lines")
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
  labs(subtitle = "Each panel restates a count already bound in the obstacle, split, or control record") +
  theme_void() +
  theme(
    text = element_text(family = FIGURE_FONT_FAMILY),
    plot.subtitle = element_text(family = FIGURE_FONT_FAMILY, size = 8.0,
                                 colour = "#555555", hjust = 0.5,
                                 margin = margin(b = 5)),
    plot.margin = margin(t = 8, r = 8, b = 12, l = 8)
  )

save_fig(fig0, "fig0_three_reasons", FIGURE_TEXT_WIDTH_IN, 3.15)
