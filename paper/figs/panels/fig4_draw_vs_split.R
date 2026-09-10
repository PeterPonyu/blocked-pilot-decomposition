# Figure 4. What the pre-registered subsample drew against what the split
# predicts it should have drawn. The segment is the gap between the two, and the
# gate line is what a draw has to clear before either control arm is scored on
# it at all: most observations sit below it whatever the draw happened to do.
#
# Marker convention, shared with the caption: open = expected, filled = observed.
# The one observed count that differs from the split keeps a filled marker (it
# is an observation, not an expectation) and changes colour and shape instead.

f3 <- per_finding[order(-per_finding$present_split), ]
f3$label <- factor(short_name(f3$finding), levels = short_name(f3$finding))
if (anyNA(f3$label) || anyDuplicated(f3$label)) {
  stop("fig4 finding axis labels are not a one-to-one mapping")
}

SERIES_EXPECTED <- "Expected from the split's prevalence"
SERIES_CONSISTENT <- "Observed, consistent with the split"
SERIES_DIFFERS <- "Observed, differs from the split"
series_levels <- c(SERIES_EXPECTED, SERIES_CONSISTENT, SERIES_DIFFERS)

pts <- rbind(
  data.frame(label = f3$label, y = f3$expected, series = SERIES_EXPECTED,
             stringsAsFactors = FALSE),
  data.frame(label = f3$label, y = f3$n_present,
             series = ifelse(f3$unrepresentative, SERIES_DIFFERS, SERIES_CONSISTENT),
             stringsAsFactors = FALSE)
)
pts$series <- factor(pts$series, levels = series_levels)

y_top <- max(f3$n_present, f3$expected) + 1.8

p <- ggplot(f3, aes(x = label)) +
  geom_hline(yintercept = MIN_PRESENT - 0.5, linetype = "22", linewidth = FIGURE_RULE_WIDTH,
             colour = "grey20") +
  geom_segment(aes(xend = label, y = expected, yend = n_present),
               colour = "grey55", linewidth = 0.35) +
  geom_point(data = pts, aes(x = label, y = y, shape = series, colour = series, fill = series),
             size = 2.3, stroke = 0.5) +
  annotate("text", x = nrow(f3) + 0.45, y = MIN_PRESENT - 0.5, hjust = 1, vjust = -0.5,
           size = FIGURE_ANNOTATION_SIZE, colour = "grey20",
           label = sprintf("presence gate: %d gold positives", MIN_PRESENT)) +
  geom_text(data = f3[f3$unrepresentative, ],
            aes(y = n_present, label = sprintf("p = %s", sci(p_repr))),
            hjust = -0.3, size = FIGURE_ANNOTATION_SIZE, colour = PAL$red) +
  scale_shape_manual(values = setNames(c(21, 21, 23), series_levels), name = NULL) +
  scale_colour_manual(values = setNames(c("grey30", PAL$blue, PAL$red), series_levels),
                      name = NULL) +
  scale_fill_manual(values = setNames(c("white", PAL$blue, PAL$red), series_levels),
                    name = NULL) +
  scale_y_continuous(name = "Gold positives in the draw",
                     limits = c(-0.4, y_top),
                     breaks = seq(0, 14, 2), expand = c(0, 0)) +
  scale_x_discrete(name = NULL, expand = expansion(add = 0.7)) +
  guides(shape = guide_legend(nrow = 1), colour = guide_legend(nrow = 1),
         fill = guide_legend(nrow = 1)) +
  rtx_theme() +
  theme(axis.text.x = rotated_axis_text(),
        legend.position = "bottom")

save_fig(p, "fig4_draw_vs_split", FIGURE_TEXT_WIDTH_IN, 3.4)
