# Figure 0 -- the three separable reasons, side by side.
#
# Every count is already computed from the bound records before this file is
# sourced. The figure does not add a scored cell, a permission, or a prevalence
# estimate. It only places the three owners on one page so the aggregate
# "the pilot failed" cannot be read as a single cause.
#
# The heading of each box is the name the caption uses for that panel, and the
# banner is the one property the caption says holds across all three.  There is
# no figure title: the caption carries the sentence, the figure carries the counts.

n_permission <- sum(lock_kinds == "Permission")
n_exceed <- sum(per_finding$exceeds_split)
n_underpowered <- sum(per_finding$maj_status == "underpowered")
maj_names <- per_finding$finding[per_finding$maj_pred > 0]
if (length(maj_names) != 1L) {
  stop("overview schematic expects exactly one majority-asserted finding")
}

boxes <- data.frame(
  id = c("permission", "prevalence", "labeller"),
  xmin = c(0.015, 0.345, 0.675),
  xmax = c(0.325, 0.655, 0.985),
  ymin = 0.30,
  ymax = 0.97,
  fill = c(PAL$fill_red, PAL$fill_blue, PAL$fill_orange),
  border = c(PAL$red, PAL$blue, PAL$orange),
  stringsAsFactors = FALSE
)
boxes$xmid <- (boxes$xmin + boxes$xmax) / 2

headings <- data.frame(
  x = boxes$xmid,
  y = boxes$ymax - 0.13,
  label = c("Permission", "Prevalence", "Labeller"),
  colour = boxes$border,
  stringsAsFactors = FALSE
)

# Centre the text on the box rather than on the canvas: an off-centre block
# reads as a box that was sized for something longer.
box_text <- data.frame(
  x = boxes$xmid,
  y = boxes$ymax - 0.25,
  label = c(
    paste0(
      n_permission, " approvals a person\n",
      "or an institution must grant.\n",
      "None is a file, a model,\n",
      "or a labelled report."
    ),
    paste0(
      n_exceed, " of ", nrow(per_finding), " findings need\n",
      "more than the ", N_TEST, "-report\n",
      "split; ", n_underpowered, " stay underpowered\n",
      "in the ", N_COHORT, "-report draw."
    ),
    paste0(
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
            fill = boxes$fill, colour = boxes$border, linewidth = 0.6) +
  geom_text(data = headings, aes(x = x, y = y, label = label),
            family = FIGURE_FONT_FAMILY, size = 3.2, fontface = "bold",
            colour = headings$colour) +
  geom_text(data = box_text, aes(x = x, y = y, label = label),
            family = FIGURE_FONT_FAMILY, size = 2.75, lineheight = 1.05,
            vjust = 1, colour = "#202020") +
  annotate(
    "label", x = 0.5, y = 0.12,
    label = "Computation removes none of these",
    family = FIGURE_FONT_FAMILY, size = 3.0, fontface = "bold",
    colour = "#202020", fill = "#F2F2F2", linewidth = 0.3,
    label.padding = grid::unit(0.25, "lines"), label.r = grid::unit(0, "lines")
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = FIGURE_FONT_FAMILY),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2)
  )

save_fig(fig0, "fig0_three_reasons", FIGURE_TEXT_WIDTH_IN, 1.75)
