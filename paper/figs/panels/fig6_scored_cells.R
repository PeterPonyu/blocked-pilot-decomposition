# Figure 6. The only cells that cleared the presence gate, and what the controls
# did with them. The interval runs from the observed miss rate up to its 90 per
# cent upper bound, which is the honest width at these counts: both arms miss
# nearly everything, and the bound is wide enough that the two cannot be ranked.

f5 <- rbind(
  data.frame(finding = scored$finding, arm = "Majority template",
             n_present = scored$n_present, miss = scored$maj_miss,
             rate = scored$maj_rate, upper = scored$maj_upper, stringsAsFactors = FALSE),
  data.frame(finding = scored$finding, arm = "Indication retrieval",
             n_present = scored$n_present, miss = scored$ret_miss,
             rate = scored$ret_rate, upper = scored$ret_upper, stringsAsFactors = FALSE)
)
# Same order as the scored-cells table: the majority control first.
f5$arm <- factor(f5$arm, levels = c("Majority template", "Indication retrieval"))
f5$panel <- sprintf("%s (%d gold positives)", short_name(f5$finding), f5$n_present)
# One label per arm, set beside the marker so neither the interval nor the rule
# at one can run through it.  A point sitting on the ceiling gets its label
# hung below the marker instead of centred on the rule.
f5$note <- sprintf("%d of %d missed\n90%% upper %.2f", f5$miss, f5$n_present, f5$upper)
f5$note_vjust <- ifelse(f5$rate >= 1, 1.15, 0.5)

p <- ggplot(f5, aes(x = arm, y = rate, colour = arm)) +
  geom_hline(yintercept = 1, linetype = "22", linewidth = FIGURE_RULE_WIDTH, colour = "grey35") +
  geom_linerange(aes(ymin = rate, ymax = upper), linewidth = 0.6) +
  geom_point(size = 2.4) +
  geom_text(aes(label = note, vjust = note_vjust), hjust = 0, nudge_x = 0.09,
            size = FIGURE_ANNOTATION_SIZE, lineheight = 0.95, colour = "grey20") +
  facet_wrap(~panel) +
  scale_colour_manual(values = c("Majority template" = PAL$red,
                                 "Indication retrieval" = PAL$blue), guide = "none") +
  scale_y_continuous(name = "Share of gold positives the control missed",
                     limits = c(0.78, 1.035), breaks = seq(0.80, 1.0, 0.05),
                     expand = c(0, 0)) +
  scale_x_discrete(name = NULL, expand = expansion(add = c(0.5, 1.0))) +
  rtx_theme() +
  theme(axis.text.x = element_text(size = FIGURE_AXIS_TEXT_SIZE),
        panel.spacing.x = unit(8, "pt"))

save_fig(p, "fig6_scored_cells", 0.86 * FIGURE_TEXT_WIDTH_IN, 2.7)
